# Queue for Tox21 Enricher
library(config)
library(DBI)
library(future)
library(ggplot2)
library(parallel)
library(plyr)
library(pool)
library(promises)
library(purrr)
library(rjson)
library(RPostgres)
library(stringr)
library(tidyverse)
library(uuid)

# Set working directory
# Uncomment this if the queue is having trouble finding the config.yml file when deployed through Docker.
#setwd(paste0(path.expand('~'), '/tox21enricher/'))

# Load params from config file
tox21config <- config::get("tox21enricher")
tox21queue <- config::get("tox21enricher-queue")
APP_DIR <- tox21config$appdir
IN_DIR <- tox21config$indir
OUT_DIR <- tox21config$outdir
CLEANUP_TIME <- tox21queue$cleanupTime
DELETE_TIME <- tox21queue$deleteTime

while(TRUE){
    # Check if anything has been sitting in the queue for longer than a day, and auto cancel those
    currentDate <- Sys.time()
    # Connect to DB
    pool <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    query <- sqlInterpolate(ANSI(), paste0("SELECT queue.uuid, queue.finished, transaction.timestamp_posted, transaction.timestamp_started, transaction.timestamp_finished FROM queue LEFT JOIN transaction ON queue.uuid=transaction.uuid WHERE finished=0;"), id="getIncomplete")
    outp <- dbGetQuery(pool, query)
    poolClose(pool)
    if(nrow(outp) > 0){
        badTransactions <- unname(unlist(apply(outp, 1, function(x){
            if(x["timestamp_started"] == "not started") {
                return(NULL)
            }
            if(as.numeric(difftime(currentDate, as.POSIXlt(x["timestamp_started"]), units="hours")) > CLEANUP_TIME) {
                return(x["uuid"])
            }
            return(NULL)
        })))
        print("clearing the following transactions...") 
        print(badTransactions)
        pool <- dbPool(
            drv=RPostgres::Postgres(),
            dbname=tox21queue$database,
            host=tox21queue$host,
            user=tox21queue$uid,
            password=tox21queue$pwd,
            port=tox21queue$port,
            idleTimeout=3600000
        )
        lapply(badTransactions, function(x){
            query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET finished=1, cancel=1, error='Canceled by queue cleanup.' WHERE uuid='", x, "';"), id="updateBad")
            outp <- dbGetQuery(pool, query)
            query <- sqlInterpolate(ANSI(), paste0("UPDATE transaction SET cancel=1 WHERE uuid='", x, "';"), id="updateBad")
            outp <- dbGetQuery(pool, query)
        })
        poolClose(pool)
    }
    
    ##########################################################################################################
    ## Check if any request has been around for longer than the max time to be stored (probably a month or so)
    # Connect to DB
    pool <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    query <- sqlInterpolate(ANSI(), paste0("SELECT queue.uuid, queue.finished, queue.cancel, transaction.timestamp_posted, transaction.timestamp_started, transaction.timestamp_finished FROM queue LEFT JOIN transaction ON queue.uuid=transaction.uuid;"), id="getAllToDelete")
    outp <- dbGetQuery(pool, query)
    poolClose(pool)
    
    if(nrow(outp) > 0){
        oldTransactions <- unname(unlist(apply(outp, 1, function(x){
            if(x["cancel"] == 1) { # if cancel flag = 1, delete no matter what
                return(x["uuid"])
            }
            if(as.numeric(difftime(currentDate, as.POSIXlt(x["timestamp_posted"]), units="days")) > DELETE_TIME) {
                return(x["uuid"])
            }
            return(NULL)
        })))
        if(length(oldTransactions) > 0){
            print("deleting the following transactions...") 
            print(oldTransactions)
            pool <- dbPool(
                drv=RPostgres::Postgres(),
                dbname=tox21queue$database,
                host=tox21queue$host,
                user=tox21queue$uid,
                password=tox21queue$pwd,
                port=tox21queue$port,
                idleTimeout=3600000
            )
            lapply(oldTransactions, function(x){
                # delete from database
                query <- sqlInterpolate(ANSI(), paste0("DELETE FROM queue WHERE uuid='", x, "';"), id="deleteOld")
                outp <- dbGetQuery(pool, query)
                query <- sqlInterpolate(ANSI(), paste0("DELETE FROM transaction WHERE uuid='", x, "';"), id="deleteOld")
                outp <- dbGetQuery(pool, query)
                
                # delete files from filesystem input and output directories
                inDir <- paste0(APP_DIR, IN_DIR, "/", x)
                outDir <- paste0(APP_DIR, OUT_DIR, "/", x)
                inFiles <- Sys.glob(paste0(inDir, "/*"), dirmark=FALSE)
                outFiles <- Sys.glob(paste0(outDir, "/*"), dirmark=FALSE)
                lapply(inFiles, function(x){
                    unlink(x, recursive=TRUE)
                })
                lapply(outFiles, function(x){
                    unlink(x, recursive=TRUE)
                })
                unlink(inDir, recursive=TRUE)
                unlink(outDir, recursive=TRUE)
            })
            poolClose(pool)
        }
    }
    
    Sys.sleep(5)
}

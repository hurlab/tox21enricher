# Queue for Tox21 Enricher
library(config)
library(future)
library(ggplot2)
library(parallel)
library(plyr)
library(pool)
library(promises)
library(purrr)
library(rjson)
library(RPostgreSQL)
library(stringr)
library(tidyverse)
library(uuid)

# Set working directory
# Uncomment this if the queue is having trouble finding the config.yml file when deployed through Docker.
#setwd(paste0(path.expand('~'), '/tox21enricher/'))

# Load params from config file
tox21config <- config::get("tox21enricher")
tox21queue <- config::get("tox21enricher-queue")
CORES <- tox21config$cores
APP_DIR <- tox21config$appdir
CLEANUP_TIME <- tox21queue$cleanupTime

while(TRUE){
    # Check if anything has been sitting in the queue for longer than a day, and auto cancel those
    currentDate <- Sys.time()
    # Connect to DB
    pool <- dbPool(
        drv=dbDriver("PostgreSQL", max.con=100),
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
            drv=dbDriver("PostgreSQL", max.con=100),
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
    Sys.sleep(5)
}

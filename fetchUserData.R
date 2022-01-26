# Query ipstack API to fetch geolocation data given an IP address from the database: https://ipstack.com/
library(config)
library(DBI)
library(future)
library(ggplot2)
library(httr)
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
tox21queue <- config::get("tox21enricher-queue")
API_KEY <- tox21queue$apikey
BASE_URL <- "http://api.ipstack.com/"

# Get all ip addresses from transactions list that aren't currently in user_data list
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
query <- sqlInterpolate(ANSI(), paste0("SELECT ip FROM transaction WHERE ip NOT IN (SELECT ip from user_data);"), id="getIP")
outp <- dbGetQuery(pool, query)
poolClose(pool)

# Filter out localhost entries (don't need to process)
outp <- outp %>% filter(ip != '127.0.0.1')

# If we have new ip addresses, get the user data and and them to the database user_data table
if(nrow(outp) > 0){
    print("Adding user data to database...")
    # Get only unique IP addresses from db
    ipaddr <- unique(outp$ip)
    lapply(ipaddr, function(ip){
        # Query ipstack API
        resp <- NULL
        tryIP <- tryCatch({
            resp <- GET(url=paste0(BASE_URL, ip), query=list(access_key=API_KEY))
            if(resp$status_code != 200){
                if(resp$status_code == 104){
                    print("Error: monthly usage limit reached for plan.")
                }
                return(FALSE)
            }
            TRUE
        }, error=function(cond){
            print(paste0("Error: ", cond))
            return(FALSE)
        })
        if(!tryIP){
            # error here
            print("Error: Problem connecting to the ipstack API. Check your query and try again.")
            return(FALSE)
        } else {
            param_ip <- content(resp)$ip
            param_type <- content(resp)$type
            param_continent <- content(resp)$continent_name
            param_country <- content(resp)$country_name
            param_region <- content(resp)$region_name
            param_city <- content(resp)$city
            param_zip <- content(resp)$zip
            # Security module not available on the ipstack free plan, disabled for now
            #param_is_proxy <- content(resp)$is_proxy
            #param_proxy_type <- content(resp)$proxy_type
            #param_is_crawler <- content(resp)$is_crawler
            #param_crawler_name <- content(resp)$crawler_name
            #param_crawler_type <- content(resp)$crawler_type
            #param_is_tor <- content(resp)$is_tor
            #param_threat_level <- content(resp)$threat_level
            #param_threat_types <- content(resp)$threat_types
            
            # Add retrieved data to database for given ip address
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
            query <- sqlInterpolate(ANSI(), paste0("INSERT INTO user_data(ip, type, continent, country, region, city, zip) VALUES('", param_ip, "', '", param_type, "', '", param_continent, "', '", param_country, "', '", param_region, "', '", param_city, "', '", param_zip, "');"), id="addUserData")
            outp <- dbExecute(pool, query)
            poolClose(pool)
        }
        # Sleep to avoid DoSing the API
        Sys.sleep(0.5)
        return(TRUE)
    })
    print("... done.")
} else {
    print("No new IP addresses.")
}

#* @apiTitle Tox21Enricher-Shiny API
#* @apiDescription This is the Plumber API for the Tox21Enricher-Shiny application. The Plumber API serves as the primary point of contact between the Tox21Enricher-Shiny client application and the other components of the Tox21Enricher-Shiny server-side utilities. Requests to perform enrichment and to access the application’s database are processed through the Plumber API via HTTP requests. The Plumber API may also be directly queried via a command-line tool like cURL.
#* @apiContact list(name="Email contact", url="mailto:parker.combs@und.edu")
#* @apiVersion 1.0

library(data.table)
library(DBI)
library(future)
library(ggplot2)
library(httr)
library(parallel)
library(plyr)
library(pool)
library(promises)
library(rjson)
library(RPostgres)
library(stringr)
library(tidyverse)
library(utils)
library(uuid)

# Disable scientific notation
options(scipen=999)

# Increase globals maximum size for futures (this is required to pass large global variables [> 500 MiB] in memory to async processes).
options(future.globals.maxSize = 2000 * 1024^2)

## Code to run on startup, not part of API endpoints / global variables
# Load params from config file
tox21config <- config::get("tox21enricher")
tox21queue <- config::get("tox21enricher-queue")
APP_DIR <- tox21config$appdir
IN_DIR <- tox21config$indir
OUT_DIR <- tox21config$outdir
ARCHIVE_DIR <- tox21queue$archivedir

if(nchar(APP_DIR) < 1){
    print("Error: please define APP_DIR.")
    stop()
}
if(nchar(IN_DIR) < 1){
    print("Error: please define IN_DIR.")
    stop()
}
if(nchar(OUT_DIR) < 1){
    print("Error: please define OUT_DIR.")
    stop()
}
if(APP_DIR == "/"){
    print("Error: invalid value for APP_DIR. Cannot be root directory.")
    stop()
}
if(IN_DIR == "/"){
    print("Error: invalid value for IN_DIR. Cannot be root directory.")
    stop()
}
if(OUT_DIR == "/"){
    print("Error: invalid value for OUT_DIR. Cannot be root directory.")
    stop()
}
# If not setting archive directory, warn user that files will not be archived
if(nchar(ARCHIVE_DIR) < 1){
    print("Warning: ARCHIVE_DIR not set. Previous request data will not be archived.")
}

# Make sure cores is a positive integer
if(is.na(as.numeric(tox21config$cores))){
    print("Error: specified number of cores must be a number.")
    stop()
}
if(as.numeric(tox21config$cores) != round(as.numeric(tox21config$cores)) | as.numeric(tox21config$cores) <= 0){ # integer check
    print("Error: specified number of cores must be a positive integer (1+).")
    stop()
}
CORES <- as.numeric(tox21config$cores) * 2

if(is.na(as.numeric(tox21queue$cleanupTime))){
    print("Error: cleanupTime must be a number.")
    stop()
}

if(is.na(as.numeric(tox21queue$deleteTime))){
    print("Error: deleteTime must be a number.")
    stop()
}

if(as.numeric(tox21queue$cleanupTime) %% 1 != 0){
    print("Error: cleanupTime must be an integer.")
    stop()
}

if(as.numeric(tox21queue$deleteTime) %% 1 != 0){
    print("Error: deleteTime must be an integer.")
    stop()
}

if(as.numeric(tox21queue$cleanupTime) < 1){
    print("Error: cleanupTime must be at least 1.")
    stop()
}

if(as.numeric(tox21queue$deleteTime) < 1){
    if(as.numeric(tox21queue$deleteTime) != -1){
        print("Error: deleteTime must be at least 1 to enable archival or equal to -1 to disable archival.")
        stop()
    }
}

CLEANUP_TIME <- as.numeric(tox21queue$cleanupTime)
DELETE_TIME <- as.numeric(tox21queue$deleteTime)


INPUT_MAX <- tox21queue$inputMax
if(INPUT_MAX > 16){ # Tox21Enricher only supports a max of 16 concurrent input sets and a minimum of 1 set.
    print("Warning: Found an inputMax value exceeding 16 in config.yml. Tox21Enricher only supports a maximum of 16 sets. INPUT_MAX will be set to 16.")
    INPUT_MAX <- 16
} else if(INPUT_MAX < 1){
    print("Warning: Found an inputMax value less than 1 in config.yml. Tox21Enricher only supports a minimum of 1 set. INPUT_MAX will be set to 1.")
    INPUT_MAX <- 1
}
PVALUE_DISPLAY <- 0.2
pvalue_trycatch <- tryCatch({
    PVALUE_DISPLAY <- as.numeric(tox21queue$pvaluedisplay)
}, error=function(e){
    print("Error: invalid p-value in config at 'pvaluedisplay'. Setting to 0.2.")
    PVALUE_DISPLAY <- 0.2
})
if(is.null(tox21queue$pvaluedisplay)){
    print("Error: invalid p-value in config at 'pvaluedisplay'. Setting to 0.2.")
    PVALUE_DISPLAY <- 0.2
}
if(is.na(PVALUE_DISPLAY)){
    print("Error: invalid p-value in config at 'pvaluedisplay'. Setting to 0.2.")
    PVALUE_DISPLAY <- 0.2
}

# ########################################## #


# Reusable function for generating database connection
conn <- function(){
    # return(dbPool(
    #     drv=RPostgres::Postgres(),
    #     dbname=tox21config$database,
    #     host=tox21config$host,
    #     user=tox21config$uid,
    #     password=tox21config$pwd,
    #     port=tox21config$port,
    #     idleTimeout=3600000
    # ))
    return(DBI::dbConnect(
        drv=RPostgres::Postgres(),
        dbname=tox21config$database,
        host=tox21config$host,
        user=tox21config$uid,
        password=tox21config$pwd,
        port=tox21config$port
    ))
}
connQueue <- function(){
    # return(dbPool(
    #     drv=RPostgres::Postgres(),
    #     dbname=tox21queue$database,
    #     host=tox21queue$host,
    #     user=tox21queue$uid,
    #     password=tox21queue$pwd,
    #     port=tox21queue$port,
    #     idleTimeout=3600000
    # ))
    
    return(DBI::dbConnect(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port
    ))
    
}

# Function for queue cleanup - should be called whenever a new request is received
queueCleanup <- function(){
    # Check if anything has been sitting in the queue for longer than a day, and auto cancel those
    currentDate <- Sys.time()
    # Connect to DB
    pool <- connQueue()
    query <- sqlInterpolate(ANSI(), paste0("SELECT queue.uuid, queue.finished, transaction.timestamp_posted, transaction.timestamp_started, transaction.timestamp_finished FROM queue LEFT JOIN transaction ON queue.uuid=transaction.uuid WHERE finished=0;"), id="getIncomplete")
    outp <- dbGetQuery(pool, query)
    # poolClose(pool)
    dbDisconnect(pool)
    if(nrow(outp) > 0){
        badTransactions <- unname(unlist(apply(outp, 1, function(x){
            if(is.na(x["timestamp_posted"]) | is.na(x["timestamp_started"])) {
                return(NULL)
            }
            if(x["timestamp_started"] == "not started") {
                return(NULL)
            }
            if(as.numeric(difftime(currentDate, as.POSIXlt(x["timestamp_started"]), units="hours")) > CLEANUP_TIME) {
                return(x["uuid"])
            }
            return(NULL)
        })))
        print("Clearing the following transactions...")
        print(badTransactions)
        pool <- connQueue()
        if(length(badTransactions) > 0){
            lapply(badTransactions, function(x){
                query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET finished=1, cancel=1, error='Cancelled by queue cleanup.' WHERE uuid='", x, "';"), id="updateBad")
                outp <- dbExecute(pool, query)
                query <- sqlInterpolate(ANSI(), paste0("UPDATE transaction SET cancel=1 WHERE uuid='", x, "';"), id="updateBad")
                outp <- dbExecute(pool, query)
            })
        }
        # poolClose(pool)
        dbDisconnect(pool)
    }
    
    ## Check if any request has been around for longer than the max time to be stored
    # Connect to DB
    pool <- connQueue()
    query <- sqlInterpolate(ANSI(), paste0("SELECT queue.uuid, queue.finished, queue.cancel, transaction.timestamp_posted, transaction.timestamp_started, transaction.timestamp_finished FROM queue LEFT JOIN transaction ON queue.uuid=transaction.uuid WHERE transaction.delete=0;"), id="getAllToDelete")
    outp <- dbGetQuery(pool, query)
    # poolClose(pool)
    dbDisconnect(pool)
    
    # Remove result files for old transactions from the filesystem (preserve in database)
    if(nrow(outp) > 0){
        oldTransactions <- unname(unlist(apply(outp, 1, function(x){
            if(is.na(x["timestamp_posted"]) | is.na(x["timestamp_started"])) {
                return(NULL)
            }
            if(x["cancel"] == 1) { # if cancel flag = 1, delete no matter what
                return(x["uuid"])
            }
            if(as.numeric(difftime(currentDate, as.POSIXlt(x["timestamp_posted"]), units="days")) > DELETE_TIME) { # If posted date exceeds set date to delete
                return(x["uuid"])
            }
            return(NULL)
        })))
        
        if(length(oldTransactions) > 0){
            print("Deleting the following old transaction data from the filesystem...")
            print(oldTransactions)
            lapply(oldTransactions, function(x){
                # define input/output/archive directories
                inDir <- paste0(APP_DIR, IN_DIR, "/", x)
                outDir <- paste0(APP_DIR, OUT_DIR, "/", x)
                
                # if archive directory is defined, then zip result files (output) and move to archive directory
                if(nchar(ARCHIVE_DIR) >= 1){
                    archiveDir <- paste0(APP_DIR, ARCHIVE_DIR)
                    zipFile <- paste0(outDir, "/tox21enricher_", x, ".zip")
                    if(file.exists(zipFile)){
                        file.copy(zipFile, paste0(archiveDir, "/tox21enricher_", x, ".zip"), copy.date=TRUE) # move zip file to archive directory
                    }
                }
                
                # delete files from filesystem input and output directories
                inFiles <- Sys.glob(paste0(inDir, "/*"), dirmark=FALSE)
                outFiles <- Sys.glob(paste0(outDir, "/*"), dirmark=FALSE)
                lapply(inFiles, function(y) unlink(y, recursive=TRUE))
                lapply(outFiles, function(y) unlink(y, recursive=TRUE))
                unlink(inDir, recursive=TRUE)
                unlink(outDir, recursive=TRUE)
                
                # set delete flag in database = 1 so this won't be reprocessed
                # Connect to DB
                poolDelete <- connQueue()
                query <- sqlInterpolate(ANSI(), paste0("UPDATE transaction SET delete=1 WHERE uuid='", x, "';"), id="setDeleteFlags")
                outp <- dbExecute(poolDelete, query)
                # poolClose(poolDelete)
                dbDisconnect(poolDelete)
            })
        }
    }
}


# API connectivity details
# Change host address and port in config.yml
API_SECURE <- tox21config$apiSecure
if(API_SECURE){
    API_PROTOCOL <- "https://"
} else {
    API_PROTOCOL <- "http://"
}
API_HOST <- tox21config$apiHost
API_PORT <- tox21config$apiPort
if(nchar(as.character(API_PORT)) < 1){ #set to sentinel value if no port specified
    API_PORT <- -1
}
# Prepare API_ADDR connection string (this is to handle the situation in which the API is hosted in a location like: hostname:8080/subdir/subdir2/api)
hostnameSplit <- unlist(str_split(API_HOST, "/"))
API_ADDR <- ""
if(API_PORT == -1){ # No port specified
    if(length(hostnameSplit) < 2){
        API_ADDR <- paste0(hostnameSplit[1])
    } else {
        API_ADDR <- paste0(hostnameSplit[1], "/", paste0(hostnameSplit[2:length(hostnameSplit)], collapse="/"))
    }
} else {
    if(length(hostnameSplit) < 2){
        API_ADDR <- paste0(hostnameSplit[1], ":", API_PORT)
    } else {
        API_ADDR <- paste0(hostnameSplit[1], ":", API_PORT, "/", paste0(hostnameSplit[2:length(hostnameSplit)], collapse="/"))
    }
}
# strip ending "/" if part of address
if(grepl("/$", API_ADDR)){
    API_ADDR <- substr(API_ADDR, 1, nchar(API_ADDR) - 1)
}

# vv future plan - futures won't work correctly without this line vv
plan('multicore')

print(paste0("! Tox21Enricher Plumber API initializing..."))
print("! Loading annotation resources...")

## API endpoints

## SECTION 0: PERFORM ENRICHMENT AND GET ANNOTATIONS
# Queue for Tox21Enricher
# Cleanup anything old in the queue on startup
# Init database pool
poolClean <- connQueue()
# Unlock any currently locked, unfinished requests WITHOUT ERRORS AND THAT HAVEN'T BEEN canceled so they can be reprocessed
query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET lock=0 WHERE finished=0 AND error IS NULL AND cancel=0;"), id="unlockTransactions")
outp <- dbExecute(poolClean, query)
# Get list of transactions that are unlocked and reset status messages
query <- sqlInterpolate(ANSI(), paste0("SELECT uuid FROM queue WHERE lock=0 AND finished=0 AND error IS NULL AND cancel=0;"), id="resetStatus")
outp <- dbGetQuery(poolClean, query)
unlockedTransactions <- outp$uuid

if(length(unlockedTransactions) > 0){
    lapply(unlockedTransactions, function(x){
        query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=0 WHERE uuid='", x, "';"), id="resetStatus")
        outp <- dbExecute(poolClean, query)
        return(paste0("Reprocessing transaction: ", x))
    })
}

# Close pool
# poolClose(poolClean)
dbDisconnect(poolClean)

# Define info for connecting to PostgreSQL Tox21Enricher database on server startup
pool <- conn()
# Grab annotation list from Tox21Enricher database on server startup
queryAnnotations <- sqlInterpolate(ANSI(), "SELECT chemical_detail.casrn, annotation_class.annoclassname, annotation_detail.annoterm FROM term2casrn_mapping INNER JOIN chemical_detail ON term2casrn_mapping.casrnuid=chemical_detail.casrnuid INNER JOIN annotation_detail ON term2casrn_mapping.annotermid=annotation_detail.annotermid INNER JOIN annotation_class ON term2casrn_mapping.annoclassid=annotation_class.annoclassid;")
outpAnnotations <- dbGetQuery(pool, queryAnnotations)

# Grab annotation details
queryAnnoDetail <- sqlInterpolate(ANSI(), "SELECT annoclassid, annoterm, annotermid FROM annotation_detail;")
outpAnnoDetail <- dbGetQuery(pool, queryAnnoDetail)
# Grab all annotation class names
queryClasses <- sqlInterpolate(ANSI(), "SELECT annoclassid, annoclassname FROM annotation_class;")
outpClasses <- dbGetQuery(pool, queryClasses)
# Grab chemical details
queryChemDetail <- sqlInterpolate(ANSI(), "SELECT dtxrid, testsubstance_chemname, casrn FROM chemical_detail;")
outpChemDetail <- dbGetQuery(pool, queryChemDetail)
# Close DB connection
# poolClose(pool)
dbDisconnect(pool)

# Load base annotations
CASRN2DSSTox <- apply(outpChemDetail, 1, function(i){
    if (i["casrn"] != "") {
        return(i["dtxrid"])
    }
    return(NULL)
})
names(CASRN2DSSTox) <- apply(outpChemDetail, 1, function(i){
    if (i["casrn"] != "") {
        return(i["casrn"])
    }
    return(NULL)
})
CASRN2DSSTox <- CASRN2DSSTox[!vapply(CASRN2DSSTox, is.null, FUN.VALUE=logical(1))]
print("! Finished loading base annotations.")

# Load DrugMatrix Annotations
# CASRN2funCatTerm
CASRN2funCatTerm_lv1 <- mclapply(split(outpAnnotations, outpAnnotations$casrn), mc.cores=CORES, mc.silent=FALSE, function(x) split(x, x$annoclassname))
CASRN2funCatTerm <- mclapply(CASRN2funCatTerm_lv1, mc.cores=CORES, mc.silent=FALSE, function(x) lapply(x, function(y) y$annoterm))
# funCatTerm2CASRN
funCatTerm2CASRN_lv1 <- mclapply(split(outpAnnotations, outpAnnotations$annoclassname), mc.cores=CORES, mc.silent=FALSE, function(x) split(x, x$annoterm))
funCatTerm2CASRN <- mclapply(funCatTerm2CASRN_lv1, mc.cores=CORES, mc.silent=FALSE, function(x) lapply(x, function(y) y$casrn))

# funCat2CASRN
funCat2CASRN <- mclapply(split(outpAnnotations, outpAnnotations$annoclassname), mc.cores=CORES, mc.silent=FALSE, function(x) split(x, x$casrn))
# term2funCat
term2funCat <- mclapply(split(outpAnnotations, outpAnnotations$annoterm), mc.cores=CORES, mc.silent=FALSE, function(x) split(x, x$annoclassname))

print("! Finished loading DrugMatrix annotations.")
print("! Starting any previously unfinished requests.")

# Define function to get all annotation class names
getAnnotationListInternal <- function(res, req){
    pool <- conn()
    query <- sqlInterpolate(ANSI(), paste0("SELECT annoclassname FROM annotation_class;"))
    outp <- dbGetQuery(pool, query)
    # Close pool
    # poolClose(pool)
    dbDisconnect(pool)
    return(outp[, "annoclassname"])
}
fullAnnoClassStr <- paste0(paste0(getAnnotationListInternal(), collapse="=checked,"), "=checked,")

#######################################################################################

# Perform enrichment analysis (for internal use by Tox21Enricher only)
# enrichmentUUID UUID for Input/Output directory on local machine, generated by Tox21Enricher application.
# annoSelectStr String, comma-delimited, containing all enabled annotations for this enrichment process. Passed from Tox21Enricher application.
# nodeCutoff numerical value between 1-100 for the max number to use in clustering.
performEnrichment <- function(enrichmentUUID="-1", annoSelectStr=fullAnnoClassStr, nodeCutoff=10) {
    # Connect to db
    poolInput <- connQueue()
    # Get begin time for request
    beginTime <- Sys.time()
    # Set begin time in transaction table
    query <- sqlInterpolate(ANSI(), paste0("UPDATE transaction SET timestamp_started='", beginTime, "' WHERE uuid='", enrichmentUUID, "';"), id="updateTransactionStart")
    outp <- dbExecute(poolInput, query)
    
    # Close pool
    dbDisconnect(poolInput)
    
    # Enrichment parameters
    annoSelectStrSplit <- unlist(str_split(annoSelectStr, "=checked,"))
    annoSelectStrSplit <- annoSelectStrSplit[nchar(annoSelectStrSplit) > 0]
    # Create funCat2Selected
    funCat2Selected <- lapply(annoSelectStrSplit, function(i) 1)
    names(funCat2Selected) <- annoSelectStrSplit
    # Directories
    inDir <- paste0(APP_DIR, IN_DIR, enrichmentUUID) # Directory for input files for enrichment set
    outDir <- paste0(APP_DIR, OUT_DIR, enrichmentUUID) # Directory for output files for enrichment set
    # DSSTox Chart
    pvalueThresholdToDisplay <- PVALUE_DISPLAY # p-value that determines which terms are to be printed
    # DSSTox Clustering
    similarityThreshold <- 0.50
    initialGroupMembership <- 3
    multipleLinkageThreshold <- 0.50
    EASEThreshold <- 1.0
    # Calculate total CASRN count 
    funCat2CASRNCount <- unlist(lapply(names(funCat2Selected), function(funCat) length(names(funCat2CASRN[[funCat]]))))
    names(funCat2CASRNCount) <- names(funCat2Selected)
    funCat2termCount <- unlist(lapply(names(funCat2Selected), function(funCat) length(names(funCatTerm2CASRN[[funCat]]))))
    names(funCat2termCount) <- names(funCat2Selected)
    funCatTerm2CASRNCount <- lapply(names(funCat2Selected), function(funCat){
        tmp_funCatTerm2CASRNCount <- lapply(names(funCatTerm2CASRN[[funCat]]), function(term) length(funCatTerm2CASRN[[funCat]][[term]]))
        names(tmp_funCatTerm2CASRNCount) <- names(funCatTerm2CASRN[[funCat]])
        return(tmp_funCatTerm2CASRNCount)
    })
    names(funCatTerm2CASRNCount) <- names(funCat2Selected)
    # Load input DSSTox ID or CASRN ID sets
    inputFiles <- list.files(path=paste0(APP_DIR, IN_DIR, enrichmentUUID), pattern="*.txt", full.names=TRUE)

    # Throw error if no input sets
    if(length(inputFiles) < 1){
        # Initialize db connection pool
        poolStatus <- connQueue()
        # Set step flag for each set name
        query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=-1 WHERE uuid='", enrichmentUUID, "';"), id="fetchStatus")
        outp <- dbExecute(poolStatus, query)
        # Check if no errors
        query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
        requestCancel <- dbGetQuery(poolStatus, query)
        # Close pool
        # poolClose(poolStatus)
        dbDisconnect(poolStatus)
        if(nrow(requestCancel) > 0){
            print(paste0("Canceling request: ", enrichmentUUID))
            return("request canceled")
        }
        return("no valid input sets. Chemicals supplied or matched may not be in Tox21")
    }
    
    # Check which input sets are good
    ldf <- lapply(inputFiles, function(i){
        openInput <- tryCatch({
            data.frame(read.delim(file=i, header=FALSE, sep="\t", comment.char="", fill=TRUE), stringsAsFactors=FALSE)
        }, error=function(e){
            NULL
        })
    })

    # Assign names to ldf and remove nulls
    names(ldf) <- unlist(lapply(inputFiles, function(x) gsub(".txt", "", gsub(paste0(APP_DIR, IN_DIR, enrichmentUUID, "/"), "", x))))
    ldf <- ldf[!vapply(ldf, is.null, FUN.VALUE=logical(1))]
    
    # If there are no good input sets, cancel request
    if(length(ldf) < 1){
        # Initialize db connection pool
        poolStatus <- connQueue()
        # Set step flag for each set name
        query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=-1 WHERE uuid='", enrichmentUUID, "';"), id="fetchStatus")
        outp <- dbExecute(poolStatus, query)
        # Check if no errors
        query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
        requestCancel <- dbGetQuery(poolStatus, query)
        # Close pool
        # poolClose(poolStatus)
        dbDisconnect(poolStatus)
        if(nrow(requestCancel) > 0){
            print(paste0("Canceling request: ", enrichmentUUID))
            return("request canceled")
        }
        return("no lines available in any input set: cannot perform enrichment analysis")
    }
    
    inputIDListHash <- lapply(ldf, function(i) as.character(i$V1))
    print(paste0("Processing ", length(inputIDListHash), " set(s)."))
    
    # Perform EASE calculation
    outfileBaseNames <- lapply(seq_len(length(ldf)), function(setNameCtr){
        setNameItem <- str_remove(inputFiles[[setNameCtr]], paste0(APP_DIR, IN_DIR, enrichmentUUID, "/")) # Remove path
        setNameItem <- str_remove(setNameItem, ".txt") # Remove filename extension
        return(setNameItem)
    })
    names(inputIDListHash) <- outfileBaseNames
    
    # Connect to DB to get status info
    poolStatus <- connQueue()
    # Get set names from database
    query <- sqlInterpolate(ANSI(), paste0("SELECT setname FROM status WHERE uuid='", enrichmentUUID, "';"), id="fetchStatus")
    outp <- dbGetQuery(poolStatus, query)
    statusFiles <- outp[, "setname"]
    # Set step flag for each set name
    if(length(statusFiles) > 0){
        lapply(statusFiles, function(x){
            query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=2 WHERE uuid='", enrichmentUUID, "' AND setname='", x, "' AND step<>-1;"), id="fetchStatus")
            outp <- dbExecute(poolStatus, query)
        })
    }
    # Check if no errors
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
    requestCancel <- dbGetQuery(poolStatus, query)
    # Close pool
    # poolClose(poolStatus)
    dbDisconnect(poolStatus)
    
    if(nrow(requestCancel) > 0){
        print(paste0("Canceling request: ", enrichmentUUID))
        return("request canceled")
    }
    
    # Perform enrichment on each input set simultaneously - multi-core
    enrichmentStatusComplete <- mclapply(outfileBaseNames, mc.cores=CORES, mc.silent=FALSE, function(outfileBase){
        # Get list of CASRN names
        CASRNS <- inputIDListHash[[outfileBase]]
        
        # Check mapped CASRNS
        mappedCASRNs <- unique(CASRNS[CASRNS %in% names(CASRN2DSSTox)])
        
        # Perform enrichment analysis
        print(paste0("Performing enrichment on ", outfileBase, "..."))
        enrichmentStatus <- perform_CASRN_enrichment_analysis(CASRNS, paste0(APP_DIR, OUT_DIR, enrichmentUUID, "/"), outfileBase, mappedCASRNs, funCat2Selected, CASRN2funCatTerm, funCatTerm2CASRN, funCat2CASRNCount, funCat2termCount, funCatTerm2CASRNCount, pvalueThresholdToDisplay, similarityThreshold, initialGroupMembership, multipleLinkageThreshold, EASEThreshold, nodeCutoff, enrichmentUUID)
        return(enrichmentStatus)
    })
    
    # Create individual GCT file
    # Update status file
    # Connect to DB to get status info
    poolStatus <- connQueue()
    # Get set names from database
    query <- sqlInterpolate(ANSI(), paste0("SELECT setname FROM status WHERE uuid='", enrichmentUUID, "';"), id="fetchStatus")
    outp <- dbGetQuery(poolStatus, query)
    statusFiles <- outp[, "setname"]
    # Set step flag for each set name
    if(length(statusFiles) > 0){
        lapply(statusFiles, function(x){
            query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=7 WHERE uuid='", enrichmentUUID, "' AND setname='", x, "' AND step<>-1;"), id="fetchStatus")
            outp <- dbExecute(poolStatus, query)
        })
    }
    # Check if no errors
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
    requestCancel <- dbGetQuery(poolStatus, query)
    # Close pool
    # poolClose(poolStatus)
    dbDisconnect(poolStatus)
    if(nrow(requestCancel) > 0){
        print(paste0("Canceling request: ", enrichmentUUID))
        return("request canceled")
    }
    
    # Get the corresponding directory names
    baseinputDirName <- inDir
    baseDirName <- outDir
    baseShortDirName <- ""
    baseOutputDir <- paste0(baseDirName, "/gct_per_set/")
    baseOutputDirGct <- paste0(baseDirName, "/gct/")
    dir.create(baseOutputDir)
    
    # Load CASRN names
    CASRN2Name <- outpChemDetail$testsubstance_chemname
    names(CASRN2Name) <- outpChemDetail$casrn
    
    # Generate individual gct files
    process_variable_DAVID_CHART_directories_individual_file(baseinputDirName, baseDirName, baseOutputDir, '', '', "P", 0.05, "P", CASRN2Name, enrichmentUUID)
    # Create DAVID Chart/Cluster files
    create_david_chart_cluster(baseDirName, nodeCutoff, "ALL", "P", 0.05, "P", enrichmentUUID)
    # zip result files
    if(dir.exists(inDir) & dir.exists(outDir)){
        system2("cp", paste0("-r ", inDir, "/. ", outDir))
        inFilesToDelete <- Sys.glob(paste0(inDir, "/*"))
        inFilesToDelete <- unlist(lapply(inFilesToDelete, function(inFileToDelete) {
            tmp <- unlist(str_split(inFileToDelete, "/"))
            return(paste0(outDir, "/", tmp[length(tmp)]))
        }))
        system2("cd", paste0(outDir, "/ ; zip -r9X ./tox21enricher_", enrichmentUUID, ".zip ./*"))
        system2("rm", paste0(inFilesToDelete, collapse=" "))
    } else { # Return with error if did not complete. Do not update in database
        return("problem creating directory for request") 
    }
    
    # Update status file(s)
    # Connect to DB to get status info
    poolStatus <- connQueue()
    
    # Get ending time
    finishTime <- Sys.time()
    
    # Set finish time in transaction table
    query <- sqlInterpolate(ANSI(), paste0("UPDATE transaction SET timestamp_finished='", finishTime, "' WHERE uuid='", enrichmentUUID, "';"), id="transactionUpdateFinished")
    outp <- dbExecute(poolStatus, query)
    # Set step flag for each set name
    query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=8 WHERE uuid='", enrichmentUUID, "' AND step<>-1;"), id="fetchStatus")
    outp <- dbExecute(poolStatus, query)
    # Check if no errors
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
    requestCancel <- dbGetQuery(poolStatus, query)
    # Close pool
    # poolClose(poolStatus)
    dbDisconnect(poolStatus)
    if(nrow(requestCancel) > 0){
        print(paste0("Canceling request: ", enrichmentUUID))
        return("request canceled")
    }
    # return success
    return("success")
}

## ENRICHMENT SUBROUTINES
# Generate individual GCT files
process_variable_DAVID_CHART_directories_individual_file <- function(inputDirName, dirName, outputDir, extTag, additionalFileName, sigColumn, sigCutOff, valueColumn, CASRN2Name, enrichmentUUID) {
    # Load the input file
    setInputFiles <- Sys.glob(paste0(inputDirName, "/", "*.txt"), dirmark=FALSE)
    
    # Check the directory for any file
    infilesAll <- Sys.glob(paste0(dirName, "/", "*_Chart.txt"), dirmark=FALSE)
    # Return if no files
    if (length(infilesAll) < 1) {
        return(FALSE)
    }
    
    # Step0. Get only the files that actually have contents
    infiles <- lapply(infilesAll, function(infile) {
        infileDF <- tryCatch({
            infileDF <- read.delim(infile, sep="\t", header=TRUE, comment.char="", fill=TRUE, stringsAsFactors=FALSE)
            infileDF
        }, error=function(cond){
            print("Error reading .gct file.")
            print(cond)
            data.frame()
        })
        if(nrow(infileDF) < 1){
            return(NULL)
        }
        return(infile)
    })
    infiles <- infiles[!vapply(infiles, is.null, FUN.VALUE=logical(1))]
    
    # Get pvalue type
    poolInput <- connQueue() # Connect to db
    queryPvalue <- sqlInterpolate(ANSI(), paste0("SELECT pvalue FROM transaction WHERE uuid='", enrichmentUUID, "';"), id="getTransactionPvalue")
    outpPvalue <- dbGetQuery(poolInput, queryPvalue)
    # poolClose(poolInput) # Close pool
    dbDisconnect(poolInput)
    PVALUE_TYPE <- outpPvalue[1, "pvalue"]

    # Get significance column (nominal pvalue or adjusted pvalue (BH-correction))
    sigColumnName <- "P"
    valueColumnName <- "P"
    if(PVALUE_TYPE == "nominal"){
        sigColumnName <- "P"
        valueColumnName <- "P"
    } else if(PVALUE_TYPE == "adjusted") {
        sigColumnName <- "BH"
        valueColumnName <- "BH"
    } 
    sigColumnIndex <- get_column_index(sigColumnName) # 5=p-value, 12=BY p-value, 13=Benjamini_Hochberg
    valueColumnIndex <- get_column_index(valueColumnName) # 5=p-value, 12=BY p-value, 13=Benjamini_Hochberg
    
    # Step1. Get the list of significant terms
    mclapply(infiles, mc.cores=CORES, mc.silent=FALSE, function(infile){
        tmp1 <- unlist(str_split(infile, "/"))
        tmp1[length(tmp1[[1]])] <- gsub(".txt", "", tmp1[[1]][length(tmp1[[1]])])
        tmp2 <- tmp1
        DATA <- tryCatch({
            DATA <- read.delim(file=infile, header=TRUE, sep="\t", comment.char="", fill=TRUE) 
            DATA
        }, error=function(cond){
            print("Error reading .gct file.")
            print(cond)
            data.frame()
        })
        if(nrow(DATA) < 1){
            # If no rows, make blank .gct file
            setFileName <- gsub(".txt", "", tmp2[length(tmp2)])
            tryCatch({
                OUTFILE <- file(paste0(outputDir, setFileName, ".gct"))
                outputContent <- paste0("#1.2\n", 0, "\t", 0, "\nCASRN\tName")
                writeLines(outputContent, OUTFILE) 
                close(OUTFILE)
            }, error=function(cond){
                print("Error creating output file.")
                print(cond)
            })
        } else {
            term2pvalue <- lapply(seq_len(nrow(DATA)), function(line){
                tmpSplit <- vector("list", 13)
                tmpSplit[1] <- as.character(DATA[line, "Category"])
                tmpSplit[2] <- as.character(DATA[line, "Term"])
                tmpSplit[3] <- as.character(DATA[line, "Count"])
                tmpSplit[4] <- as.character(DATA[line, "X."])
                tmpSplit[5] <- as.character(DATA[line, "PValue"])
                tmpSplit[6] <- as.character(DATA[line, "CASRNs"])
                tmpSplit[7] <- as.character(DATA[line, "List.Total"])
                tmpSplit[8] <- as.character(DATA[line, "Pop.Hits"])
                tmpSplit[9] <- as.character(DATA[line, "Pop.Total"])
                tmpSplit[10] <- as.character(DATA[line, "Fold.Enrichment"])
                tmpSplit[11] <- as.character(DATA[line, "Bonferroni"])
                tmpSplit[12] <- as.character(DATA[line, "Benjamini_Yekutieli"])
                tmpSplit[13] <- as.character(DATA[line, "Benjamini_Hochberg"])
                if ( grepl("^\\D", tmpSplit[[sigColumnIndex]]) | as.numeric(tmpSplit[[sigColumnIndex]]) >= sigCutOff | as.numeric(tmpSplit[[10]]) < 1) {
                    # skip, do nothing
                    return(NULL)
                } else {
                    tmpTermKey <- paste0(tmpSplit[[2]], " | ", tmpSplit[[1]])
                    return(data.frame(
                        name=tmpTermKey,
                        pvalue=tmpSplit[[sigColumnIndex]]
                    ))
                }
            })
            tmpTerm2pvalue <- do.call(rbind.data.frame, term2pvalue)
            if(nrow(tmpTerm2pvalue) < 1){
                # If no rows, make blank .gct file
                setFileName <- gsub(".txt", "", tmp2[length(tmp2)])
                tryCatch({
                    OUTFILE <- file(paste0(outputDir, setFileName, ".gct"))
                    outputContent <- paste0("#1.2\n", 0, "\t", 0, "\nCASRN\tName")
                    writeLines(outputContent, OUTFILE)
                    close(OUTFILE)
                }, error=function(cond){
                    print("Error creating output file.")
                    print(cond)
                })
                return(NULL)
            }
            term2pvalue <- tmpTerm2pvalue$pvalue
            names(term2pvalue) <- tmpTerm2pvalue$name
            
            CASRN2TermMatrix_lv1 <- lapply(seq_len(nrow(DATA)), function(line){
                tmpSplit <- list(
                    as.character(DATA[line, "Category"]), 
                    as.character(DATA[line, "Term"]),
                    as.character(DATA[line, "Count"]),
                    as.character(DATA[line, "X."]), # original label is "%"
                    as.character(DATA[line, "PValue"]),
                    as.character(DATA[line, "CASRNs"]),
                    as.character(DATA[line, "List.Total"]),
                    as.character(DATA[line, "Pop.Hits"]),
                    as.character(DATA[line, "Pop.Total"]),
                    as.character(DATA[line, "Fold.Enrichment"]),
                    as.character(DATA[line, "Bonferroni"]),
                    as.character(DATA[line, "Benjamini_Yekutieli"]),
                    as.character(DATA[line, "Benjamini_Hochberg"])
                )
                if ( grepl("^\\D", tmpSplit[[sigColumnIndex]]) | as.numeric(tmpSplit[[sigColumnIndex]]) >= sigCutOff | as.numeric(tmpSplit[[10]]) < 1) {
                    # skip, do nothing
                } else {
                    tmpTermKey <- paste0(tmpSplit[[2]], " | ", tmpSplit[[1]])
                    CASRNs <- unlist(str_split(tmpSplit[[6]], ", "))
                    tmpTermKeyList <- vector("list", length(CASRNs))
                    CASRN2TermMatrix_lv1_tmp <- lapply(tmpTermKeyList, function(x) tmpTermKey)
                    names(CASRN2TermMatrix_lv1_tmp) <- CASRNs
                    return(CASRN2TermMatrix_lv1_tmp)
                }
            })
            #Get not null elements
            CASRN2TermMatrix_lv2 <- CASRN2TermMatrix_lv1[!vapply(CASRN2TermMatrix_lv1, is.null, FUN.VALUE=logical(1))]
            CASRN2TermMatrix_lv2 <- unlist(CASRN2TermMatrix_lv2, recursive=FALSE)
            # Merge same names
            CASRN2TermMatrix <- lapply(unique(names(CASRN2TermMatrix_lv2)), function(x) unlist(unname(CASRN2TermMatrix_lv2[names(CASRN2TermMatrix_lv2) %in% x])))
            names(CASRN2TermMatrix) <- unique(names(CASRN2TermMatrix_lv2))
            # Now create new output files
            setFileName <- gsub(".txt", "", tmp2[length(tmp2)])
            tryCatch({
                OUTFILE <- file(paste0(outputDir, setFileName, ".gct"))
                if(typeof(CASRN2TermMatrix) == "list"){
                    CASRNs <- names(CASRN2TermMatrix)
                }
                else {
                    CASRNs <- colnames(CASRN2TermMatrix)
                }
                tmpTermKeys <- names(term2pvalue)
                tmpTermKeys <- unique(tmpTermKeys)
                CASRNCount <- length(CASRNs)
                tmpTermKeyCount <- length(tmpTermKeys)
                outputContentHeader <- paste0("#1.2\n", CASRNCount, "\t", tmpTermKeyCount, "\nCASRN\tName")
                termKeys <- lapply(tmpTermKeys, function(tmpTermKey) paste0(tmpTermKey, " | ", term2pvalue[tmpTermKey]))
                termKeys <- paste0(paste0(termKeys, collapse="\t"))
                outputContentMatrix <- lapply(CASRNs, function(CASRN){
                    if (!is.null(CASRN2Name[[CASRN]])){ 
                        termNameOut <- CASRN2Name[[CASRN]]
                        if(nchar(termNameOut) > 30){ # If the term name is longer than 30 characters (arbitrary, can change later), truncate for readability on the heatmap images
                            termNameOut <- paste0(substring(termNameOut, 1, 30), "...")
                        }
                    }
                    casrnAndTerm <- paste0(CASRN, "\t", termNameOut)
                    termKeysInner <- lapply(tmpTermKeys, function(tmpTermKey){
                        if (tmpTermKey %in% CASRN2TermMatrix[[CASRN]]){
                            return("1")
                        } else {
                            return("0")
                        }
                    })
                    return(paste0(casrnAndTerm, "\t", paste0(termKeysInner, collapse="\t")))
                })
                outputContent <- paste0(outputContentHeader, "\t", termKeys, "\n", paste0(outputContentMatrix, collapse="\n"))
                writeLines(outputContent, OUTFILE) 
                close(OUTFILE)
            }, error=function(cond){
                print("Error creating .gct file.")
                print(cond)
            })
        }
    })
}

get_column_index <- function(columnType) {
    if (grepl("P", columnType, ignore.case=TRUE)) { 
        return(5)
    } else if(grepl("Bonferroni", columnType, ignore.case=TRUE)) { 
        return(11)
    } else if(grepl("BY", columnType, ignore.case=TRUE)) { 
        return(12)
    } else if(grepl("BH", columnType, ignore.case=TRUE)) {
        return(13)
    } else { 
        return("Error: wrong Signficance type. Use P, Bonferroni, BY, BH")
    }
}

# Create cluster input file
convert_gct_to_cluster_input_file <- function(gctFile, clusterInputFile, outputDir){
    tryCatch({
        GCTFILE <- read.delim(gctFile, sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, skip=2, fill=TRUE, check.names=FALSE)
        file.create(clusterInputFile)
        CLUSTER <- file(clusterInputFile)
        newHeader_tmp <- paste0(lapply(seq(3, length(colnames(GCTFILE))), function(i) "\t1"), collapse="")
        newHeader <- paste("UNIQID\tNAME\tGWEIGHT\tGORDER\t", paste0(colnames(GCTFILE)[seq(3, length(colnames(GCTFILE)))], collapse="\t"), "\nEWEIGHT\t\t\t", newHeader_tmp)
        GORDER <- nrow(GCTFILE) + 1
        newContent <- lapply(seq_len(nrow(GCTFILE)), function(i){
            line <- GCTFILE[i, ]
            tmpSplit <- unlist(str_split(line, "\t"))
            tmpSplit <- tmpSplit[nchar(tmpSplit) > 0]
            stringToRet <- ""
            if(!is.na(tmpSplit[1])){
                if(!is.null(tmpSplit[1]) & tmpSplit[1] != ""){
                    stringToRet <- paste0(tmpSplit[1], "\t", "", "\t1\t", GORDER, "\t", paste0(line[seq(3, length(line))], collapse="\t"))
                }
            }
            return(stringToRet)
        })
        write(paste0(newHeader, "\n", paste0(newContent, collapse="\n")), CLUSTER, append=TRUE)
        close(CLUSTER)
    }, error=function(cond){
        print("Error: could not generate .gct cluster file.")
        print(cond)
    })
}

# Create necessary subdirectories for use during clustering subroutine
create_sub_directory <- function(outputDir){
    tmp1 <- unlist(str_split(outputDir, "/"))
    tmp1 <- tmp1[nchar(tmp1) > 0]
    dirName <- paste0("/", tmp1, "/", collapse="")
    dir.create(dirName)
}

# Check if supplied GCT file is valid for processing
check_gct_contains_more_than_two_lines <- function(infile){
    lineCount <- 0
    lineCount <- tryCatch({
        INFILE <- read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, skip=2, fill=TRUE)
        lineCount <- nrow(INFILE)
        lineCount
    }, error=function(cond){
        print("Error reading .gct file.")
        print(cond)
        0
    })
    if(lineCount >= 2){
        return(1)
    } else {
        return(0)
    }
}

# Create DAVID Chart and Cluster files
create_david_chart_cluster <- function(baseDirName="", topTermLimit=10, mode="ALL", sigColumnName="P", sigCutoff=0.05, valueColumnName="P", enrichmentUUID){
    
    # Get pvalue type
    poolInput <- connQueue() # Connect to db
    queryPvalue <- sqlInterpolate(ANSI(), paste0("SELECT pvalue FROM transaction WHERE uuid='", enrichmentUUID, "';"), id="getTransactionPvalue")
    outpPvalue <- dbGetQuery(poolInput, queryPvalue)
    # poolClose(poolInput) # Close pool
    dbDisconnect(poolInput)
    PVALUE_TYPE <- outpPvalue[1, "pvalue"]
    
    # Get significance column (nominal pvalue or adjusted pvalue (BH-correction))
    sigColumnName <- "P"
    valueColumnName <- "P"
    if(PVALUE_TYPE == "nominal"){
        sigColumnName <- "P"
        valueColumnName <- "P"
    } else if(PVALUE_TYPE == "adjusted") {
        sigColumnName <- "BH"
        valueColumnName <- "BH"
    }
    sigColumnIndex <- get_column_index(sigColumnName) # 5=p-value, 12=BY p-value, 13=Benjamini_Hochberg
    valueColumnIndex <- get_column_index(valueColumnName) # 5=p-value, 12=BY p-value, 13=Benjamini_Hochberg
    
    # Get the corresponding directory names
    baseNameSplit <- unlist(str_split(baseDirName, "/"))
    baseNameSplit <- baseNameSplit[nchar(baseNameSplit) > 0] #remove "" elements in split list
    baseShortDirName <- ""
    if (baseNameSplit[length(baseNameSplit)] == ""){
        baseShortDirName <- paste0(baseNameSplit[length(baseNameSplit)-1], '/')
    } else {
        baseShortDirName <- paste0(baseNameSplit[length(baseNameSplit)], '/')
    }
    baseOutputDir <- paste0(baseDirName, '/gct/')
    if (!dir.exists(baseOutputDir)) { # Do nothing if directory already exists (i.e., we are regenerating the network)
        # else create the directory if this is the first time we are dealing with this data set
        print(paste0("No directory found for ", baseOutputDir, ". Creating..."))
        dir.create(baseOutputDir)
    }
    # Load term annotation data
    className2classID <- outpClasses$annoclassid
    names(className2classID) <- outpClasses$annoclassname
    classID2className <- outpClasses$annoclassname
    names(classID2className) <- outpClasses$annoclassid
    classID2annotationTerm2termUniqueID_lv1 <- mclapply(split(outpAnnoDetail, outpAnnoDetail$annoclassid), mc.cores=CORES, mc.silent=FALSE, function(x) split(x, x$annoterm))
    classID2annotationTerm2termUniqueID <- mclapply(classID2annotationTerm2termUniqueID_lv1, mc.cores=CORES, mc.silent=FALSE, function(x) lapply(x, function(y) y$annotermid))
    # Enumerate all possible directories
    process_variable_DAVID_CHART_directories(baseDirName, baseOutputDir, "", "", topTermLimit, mode, sigColumnName, sigCutoff, valueColumnName, className2classID, classID2annotationTerm2termUniqueID, enrichmentUUID)
    process_variable_DAVID_CLUSTER_directories(baseDirName, baseOutputDir, "", "", topTermLimit, mode, sigColumnName, sigCutoff, valueColumnName, className2classID, classID2annotationTerm2termUniqueID, enrichmentUUID)
}

process_variable_DAVID_CLUSTER_directories <- function(dirName, outputDir, extTag, additionalFileName, topTermLimit, mode, sigColumnName, sigCutoff, valueColumnName, className2classID, classID2annotationTerm2termUniqueID, enrichmentUUID){
    # Check the directory for any file
    infiles <- Sys.glob(paste0(dirName, "/*_Cluster.txt"))
    if (is.null(infiles[1])){
        return(FALSE)
    }
    # Define number of top clusters
    dir.create(outputDir)
    dirInputName <- dirName
    dirInputExpression <- paste0(dirInputName, "ExpressionData/")
    
    
    # Get pvalue type
    poolInput <- connQueue() # Connect to db
    queryPvalue <- sqlInterpolate(ANSI(), paste0("SELECT pvalue FROM transaction WHERE uuid='", enrichmentUUID, "';"), id="getTransactionPvalue")
    outpPvalue <- dbGetQuery(poolInput, queryPvalue)
    # poolClose(poolInput) # Close pool
    dbDisconnect(poolInput)
    PVALUE_TYPE <- outpPvalue[1, "pvalue"]
    # Get significance column (nominal pvalue or adjusted pvalue (BH-correction))
    sigColumnName <- "P"
    valueColumnName <- "P"
    sigDFIndex <- "PValue"
    if(PVALUE_TYPE == "nominal"){
        sigColumnName <- "P"
        valueColumnName <- "P"
        sigDFIndex <- "PValue"
    } else if(PVALUE_TYPE == "adjusted") {
        sigColumnName <- "BH"
        valueColumnName <- "BH"
        sigDFIndex <- "Benjamini_Hochberg"
    }
    
    sigColumnIndex <- get_column_index(sigColumnName) # 5=p-value, 12=BY p-value, 13=Benjamini_Hochberg
    valueColumnIndex <- get_column_index(valueColumnName) # 5=p-value, 12=BY p-value, 13=Benjamini_Hochberg
    
    summaryFileNameBase <- additionalFileName
    summaryFileNameExt <- extTag
    dirNameSplit <- unlist(str_split(dirName, "/"))
    if (!is.null(dirNameSplit[length(dirNameSplit)])) {
        summaryFileNameBase <- paste0(summaryFileNameBase, "Cluster_Top", topTermLimit, "_", mode, "__", sigColumnName, "_", sigCutoff, "_", valueColumnName)
        summaryFileNameExt <- paste0(summaryFileNameExt,  "Cluster_Top", topTermLimit, "_", mode, "__", sigColumnName, "_", sigCutoff, "_", valueColumnName)
    } else {
        summaryFileNameBase <- paste0(summaryFileNameBase, "Cluster_Top", topTermLimit, "_", mode, "__", sigColumnName, "_", sigCutoff, "_", valueColumnName)
        summaryFileNameExt <- paste0(summaryFileNameExt,  "Cluster_Top", topTermLimit, "_", mode, "__", sigColumnName, "_", sigCutoff, "_", valueColumnName)
    }

    # Load DAVID cluster files
    # Step1. Get the list of significant terms
    if(length(infiles) < 1){
        return(FALSE)
    }
    
    getSigTerms <- mclapply(infiles, mc.cores=CORES, mc.silent=FALSE, function(infile) {
        tmp1 <- unlist(str_split(infile, "/"))
        tmpNameSplit <- unlist(str_split(tmp1[length(tmp1)], "__Cluster.txt"))
        shortFileBaseName <- tmpNameSplit[1]
        originalSourceFile <- paste0(dirInputName, "/", shortFileBaseName, ".txt")
        lines <- data.frame()
        lines <- tryCatch({
            lines <- read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=FALSE, fill=TRUE, col.names=c("Category", "Term", "Count", "%", "PValue", "CASRNs", "List Total", "Pop Hits", "Pop Total", "Fold Enrichment", "Bonferroni", "Benjamini_Yekutieli", "Benjamini_Hochberg"))
            lines
        }, error=function(cond){
            print("Error reading cluster file.")
            print(cond)
            data.frame()
        })
        if(nrow(lines) < 1){
            return(FALSE)
        }
        tmpIDList <- lapply(seq_len(nrow(lines)), function(i){
            tmpSplit <- lines[i, ]
            if(grepl("^Annotation Cluster", tmpSplit[[1]])) {
                firstClusterIndex <- i + 2
                tmpSplitInner <- lines[firstClusterIndex, ]
                if (tmpSplitInner[[sigColumnIndex]] == "" | is.na(tmpSplitInner[[sigColumnIndex]]) | grepl("^\\D", tmpSplitInner[[sigColumnIndex]]) | as.numeric(tmpSplitInner[[sigColumnIndex]]) >= sigCutoff | as.numeric(tmpSplitInner[[10]]) < 1) {
                    return(NULL)
                } else {
                    return(paste0(tmpSplitInner[[1]], " | ", tmpSplitInner[[2]]))
                }
            }
        })
        # Get not null elements
        tmpIDList <- tmpIDList[!vapply(tmpIDList, is.null, FUN.VALUE=logical(1))]
        tmpIDList <- unlist(tmpIDList, recursive=FALSE)
        # Cut off entries over the limit
        if(length(tmpIDList) > as.double(topTermLimit)){
            tmpIDList <- tmpIDList[seq_len((as.double(topTermLimit)))]
        }
        tmp_ID2Term <- tmpIDList
        names(tmp_ID2Term) <- tmpIDList
        tmp_ID2Class <- unlist(lapply(tmpIDList, function(x) unlist(str_split(x, " \\| "))[1]))
        names(tmp_ID2Class) <- tmpIDList
        return(list(ID2Term=tmp_ID2Term, ID2Class=tmp_ID2Class))
    })
    getSigTermsFilteredTerms <- lapply(seq_len(length(getSigTerms)), function(i) getSigTerms[[i]]["ID2Term"])
    getSigTermsFilteredClasses <- lapply(seq_len(length(getSigTerms)), function(i) getSigTerms[[i]]["ID2Class"])
    ID2Term  <- unique(unlist(unname(unlist(getSigTermsFilteredTerms, recursive=FALSE)), recursive=FALSE))
    ID2Class <- unique(unlist(unname(unlist(getSigTermsFilteredClasses, recursive=FALSE)), recursive=FALSE))
    
    # remove NA
    ID2Term <- ID2Term[!vapply(ID2Term, is.na, FUN.VALUE=logical(1))]
    ID2Class <- ID2Class[!vapply(ID2Class, is.na, FUN.VALUE=logical(1))]
    # For cluster, we want to append the class name before the term as a given term may appear in multiple classes. If that happens, heatmap generation will be messed up.
    IDs <- ID2Term
    fileHeaderNames <- lapply(infiles, function(infile) {
        tmp1 <- unlist(str_split(infile, "/"))
        tmpNameSplit <- str_split(tmp1[length(tmp1)], "__Cluster.txt")
        shortFileBaseName <- tmpNameSplit[1]
        tmp2 <- unlist(str_split(tmp1[length(tmp1)], ".xls"))
        if(length(tmp2[1]) == length(tmp1[length(tmp1)])) {
            tmp2 <- unlist(str_split(tmp1[length(tmp1)], ".txt"))
        }
        tmp3 <- unlist(str_split(tmp2[1], "__Cluster"))
        return(tmp3[1])
    })
    mergedData <- lapply(infiles, function(infile) {
        tmp1 <- unlist(str_split(infile, "/"))
        tmpNameSplit <- str_split(tmp1[length(tmp1)], "__Cluster.txt")
        shortFileBaseName <- tmpNameSplit[1]
        tmp2 <- unlist(str_split(tmp1[length(tmp1)], ".xls"))
        if(length(tmp2[1]) == length(tmp1[length(tmp1)])) {
            tmp2 <- unlist(str_split(tmp1[length(tmp1)], ".txt"))
        }
        tmp3 <- unlist(str_split(tmp2[1], "__Cluster"))
        # Check term file and load
        # Also, enforce column data types, sometimes they get messed up when reading in
        DATA <- tryCatch(read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, fill=TRUE, skip=1, colClasses=c("character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character")), error=function(cond) {
            data.frame()
        })
        if(is.null(DATA)){
            return(NULL)
        }
        if(nrow(DATA) > 0){
            # remove "annotation cluster" lines
            DATA <- DATA %>% filter(!grepl("^Annotation Cluster", Category)) %>% filter(!grepl("^Category", Category))
            # get only unique entries
            DATA <- distinct(DATA)
            annotationNamesFull <- unlist(lapply(seq_len(nrow(DATA)), function(line) {
                tmpSplit <- lapply(DATA[line, ], function(lineItem) lineItem)
                return(paste0(tmpSplit[[1]], " | ", tmpSplit[[2]]))
            }))
            return(data.frame("Set"=tmp3[1], "Annotation"=annotationNamesFull, DATA, stringsAsFactors=FALSE))
        }
        return(NULL)
    })
    mergedData <- bind_rows(mergedData, .id="Label")
    mergedData <- split(mergedData, mergedData$Annotation)
    
    pvalueMatrix <- lapply(mergedData, function(x){
        pvalueMatrix_inner <- unlist(lapply(seq_len(length(x[[sigDFIndex]])), function(y) {
            if (x[y, sigDFIndex] == "" | is.null(x[y, sigDFIndex]) | is.null(x[y, "Fold.Enrichment"]) | grepl("^\\D", x[y, sigDFIndex]) | (mode == "ALL" & as.numeric(x[y, sigDFIndex]) >= sigCutoff) | as.numeric(x[y, "Fold.Enrichment"]) < 1) {
                return(NULL)
            }
            pvalueLog <- -1 * log10(as.double(x[y, sigDFIndex]))
            if(!is.finite(pvalueLog)){
                pvalueLog <- 500 # set to 500 if pvalue is effectively 0 (arbitrary)
            }
            return(pvalueLog)
        }))
        if(is.null(pvalueMatrix_inner)) {
            return(NULL)
        }
        names(pvalueMatrix_inner) <- unlist(lapply(seq_len(length(x[[sigDFIndex]])), function(y) {
            if (x[y, sigDFIndex] == "" | is.null(x[y, sigDFIndex]) | is.null(x[y, "Fold.Enrichment"]) | grepl("^\\D", x[y, sigDFIndex]) | (mode == "ALL" & as.numeric(x[y, sigDFIndex]) >= sigCutoff) | as.numeric(x[y, "Fold.Enrichment"]) < 1) {
                return(NULL)
            }
            return(x[y, "Set"])
        }))
        pvalueMatrix_inner <- pvalueMatrix_inner[!vapply(pvalueMatrix_inner, is.null, FUN.VALUE=logical(1))]
        return(pvalueMatrix_inner)
    })
    names(pvalueMatrix) <- names(mergedData)
    pvalueMatrix <- pvalueMatrix[!vapply(pvalueMatrix, is.null, FUN.VALUE=logical(1))]

    # Create a summary file
    summaryFileName <- paste0(summaryFileNameBase, "__ValueMatrix.txt")
    SUMMARY <- tryCatch({
        file.create(paste0(outputDir, summaryFileName))
        SUMMARY <- file(paste0(outputDir, summaryFileName))
        SUMMARY
    }, error=function(cond){
        print("Error creating summary file.")
        print(cond)
        NULL
    })
    if(is.null(SUMMARY)){
        NULL
    }
    
    fileHeaderNames <- sort_by_file_number(unlist(fileHeaderNames))
    writeToSummaryHeader <- paste0("GROUP\tID\tTerms\t", paste0(fileHeaderNames, collapse="\t"), "\n")
    
    # Get not null elements
    IDs <- IDs[!vapply(IDs, is.na, FUN.VALUE=logical(1))]
    IDs <- unique(IDs)
    
    writeToSummary <- lapply(IDs, function(ID) {
        IDSplit <- unlist(str_split(ID, " \\| "))
        writeToSummaryLabel <- paste0(IDSplit[1], "\t", ID, "\t", IDSplit[2])
        writeToSummary2 <- lapply(fileHeaderNames, function(header){
            if (!is.null(pvalueMatrix[[ID]])){
                if(header %in% names(pvalueMatrix[[ID]])) {
                    return(paste0(pvalueMatrix[[ID]][header]))
                } else {
                    return("0")
                }
            } else {
                return("#")
            }
        })
        writeToSummary2 <- paste0(writeToSummary2, collapse="\t")
        writeToSummary2 <- gsub("#", "", writeToSummary2)
        return(paste0(writeToSummaryLabel, "\t", writeToSummary2))
    })
    writeToSummary <- paste0(writeToSummary, collapse="\n")
    
    fcheck <- tryCatch({
        write(paste0(writeToSummaryHeader, writeToSummary), SUMMARY, append=TRUE)
        close(SUMMARY)
        TRUE
    }, error=function(cond){
        print("Error creating summary file.")
        print(cond)
        FALSE
    })
    if(fcheck == FALSE){
        return(NULL)
    }

    # Create a network summary file for Cluster
    forNetworkFile <- paste0(summaryFileNameBase, "__ValueMatrix.ForNet")
    NETWORK <- tryCatch({
        file.create(paste0(outputDir, forNetworkFile))
        NETWORK <- file(paste0(outputDir, forNetworkFile))
        NETWORK
    }, error=function(cond){
        print("Error creating summary file.")
        print(cond)
        NULL
    })
    if(is.null(NETWORK)){
        return(NULL)
    }
    writeToNetworkHeader <- paste0("GROUPID\tUID\tTerms\t", paste0(fileHeaderNames, collapse="\t"), "\n")
    writeToNetwork <- lapply(IDs, function(ID) {
        idTermSplits <- unlist(str_split(ID, " \\| "))
        tmpHashRef <- classID2annotationTerm2termUniqueID[[className2classID[[idTermSplits[1]]]]]
        writeToNetwork2 <- paste0(className2classID[[idTermSplits[1]]], "\t", tmpHashRef[[idTermSplits[2]]], "\t", idTermSplits[2])
        writeToNetwork_inner <- unname(unlist(lapply(fileHeaderNames, function(header) {
            if (!is.null(pvalueMatrix[[ID]])) {
                return(pvalueMatrix[[ID]][header])
            } else {
                return("#") # delete these later
            }
        })))
        writeToNetwork_inner <- paste0(writeToNetwork_inner, collapse="\t")
        writeToNetwork_inner <- gsub("#", "", writeToNetwork_inner, fixed=TRUE)
        writeToNetwork2 <- paste0(writeToNetwork2, "\t", writeToNetwork_inner)
    })
    writeToNetwork <- paste0(writeToNetwork, collapse="\n")
    fcheck <- tryCatch({
        write(paste0(writeToNetworkHeader, writeToNetwork), NETWORK, append=TRUE)
        close(NETWORK)
        TRUE
    }, error=function(cond){
        print("Error creating summmary file.")
        print(cond)
        FALSE
    })
    if(fcheck == FALSE){
        return(NULL)
    }
    
    # Create a gct file from ValueMatrix
    fcheck <- tryCatch({
        INFILE <- read.delim(paste0(outputDir, summaryFileNameBase, "__ValueMatrix.txt"), sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, fill=TRUE)
        file.create(paste0(outputDir, summaryFileNameBase, "__ValueMatrix.gct"))
        OUTFILE <- file(paste0(outputDir, summaryFileNameBase, "__ValueMatrix.gct"))
        headerLine <- paste0(colnames(INFILE), collapse="\t")
        headerSplit <- unlist(str_split(headerLine, "\t"))
        sampleCnt <- length(headerSplit) - 3
        geneCnt <- 0
        content <- ""
        if(nrow(INFILE) > 0){
            content <- lapply(seq_len(nrow(INFILE)), function(l) {
                line <- paste0(INFILE[l, ], collapse="\t")
                if (line == "") {
                    # Do nothing
                    return(NULL)
                } else {
                    tmpSplit <- unlist(str_split(line, "\t"))
                    tmpSplit <- unlist(lapply(seq_len((sampleCnt+3)), function(i) {
                        if(i >= 4){
                            if (tmpSplit[i] == "NA" | is.na(tmpSplit[i]) | length(tmpSplit[i]) < 1 | is.null(tmpSplit[i])) {
                                return(0)
                            } 
                        }
                        return(tmpSplit[i])
                    }))
                    tmpSplit <- tmpSplit[-1] #Shift
                    return(paste0(tmpSplit, collapse="\t"))
                }
            })
            content <- content[!vapply(content, is.null, FUN.VALUE=logical(1))]
            geneCnt <- length(content)
            content <- paste0(content, collapse="\n")
        }
        write(paste0("#1.2\n", geneCnt, "\t", sampleCnt, "\n", paste0(colnames(INFILE)[-1], collapse="\t"), "\n", content), OUTFILE, append=TRUE)
        close(OUTFILE)
        TRUE
    }, error=function(cond){
        print("Error creating .gct ValueMatrix file.")
        print(cond)
        FALSE
    })
    if(fcheck == FALSE){
        return(NULL)
    }
}

process_variable_DAVID_CHART_directories <- function(dirName, outputDir, extTag, additionalFileName, topTermLimit, mode, sigColumnName, sigCutoff, valueColumnName, className2classID, classID2annotationTerm2termUniqueID, enrichmentUUID){
    # Check the directory for any file
    infilesAll <- Sys.glob(paste0(dirName, "/*_Chart.txt"))
    if (is.null(infilesAll[1])){
        return(FALSE)
    }
    
    # Define number of top clusters
    dir.create(outputDir)
    dirInputName <- dirName
    dirInputExpression <- paste0(dirInputName, "ExpressionData/")
    
    # Get pvalue type
    poolInput <- connQueue() # Connect to db
    queryPvalue <- sqlInterpolate(ANSI(), paste0("SELECT pvalue FROM transaction WHERE uuid='", enrichmentUUID, "';"), id="getTransactionPvalue")
    outpPvalue <- dbGetQuery(poolInput, queryPvalue)
    # poolClose(poolInput) # Close pool
    dbDisconnect(poolInput)
    PVALUE_TYPE <- outpPvalue[1, "pvalue"]
    
    # Get significance column (nominal pvalue or adjusted pvalue (BH-correction))
    sigColumnName <- "P"
    valueColumnName <- "P"
    sigDFIndex <- "PValue"
    if(PVALUE_TYPE == "nominal"){
        sigColumnName <- "P"
        valueColumnName <- "P"
        sigDFIndex <- "PValue"
    } else if(PVALUE_TYPE == "adjusted") {
        sigColumnName <- "BH"
        valueColumnName <- "BH"
        sigDFIndex <- "Benjamini_Hochberg"
    }
    
    sigColumnIndex <- get_column_index(sigColumnName) # 5=p-value, 12=BY p-value, 13=Benjamini_Hochberg
    valueColumnIndex <- get_column_index(valueColumnName) # 5=p-value, 12=BY p-value, 13=Benjamini_Hochberg
    
    summaryFileNameBase <- additionalFileName
    summaryFileNameExt <- extTag
    dirNameSplit <- unlist(str_split(dirName, "/"))
    summaryFileNameBase <- paste0(summaryFileNameBase, "Chart_Top", topTermLimit, "_", mode, "__", sigColumnName, "_", sigCutoff, "_", valueColumnName)
    summaryFileNameExt <- paste0(summaryFileNameExt,  "Chart_Top", topTermLimit, "_", mode, "__", sigColumnName, "_", sigCutoff, "_", valueColumnName)
    
    # Load DAVID Chart files
    # Step0. Get only the files that actually have contents
    infiles <- lapply(infilesAll, function(infile) {
        infileDF <- data.frame()
        infileDF <- tryCatch({
            infileDF <- read.delim(infile, sep="\t", header=TRUE, comment.char="", fill=TRUE, stringsAsFactors=FALSE)
            infileDF
        }, error=function(cond){
            print("Error reading chart file.")
            print(cond)
            data.frame()
        })
        if(nrow(infileDF) < 1){
            return(NULL)
        }
        return(infile)
    })
    infiles <- infiles[!vapply(infiles, is.null, FUN.VALUE=logical(1))]
    if (length(infiles) < 1) {
        return (FALSE)
    }

    # Step1. Get the list of significant terms
    getSigTerms <- mclapply(infiles, mc.cores=CORES, mc.silent=FALSE, function(infile) {

        tmp1 <- unlist(str_split(infile, "/"))
        tmpNameSplit <- unlist(str_split(tmp1[length(tmp1)], "__Chart.txt"))
        shortFileBaseName <- tmpNameSplit[1]
        originalSourceFile <- paste0(dirInputName, "/", shortFileBaseName, ".txt")
        DATA <- data.frame()
        DATA <- tryCatch({
            DATA <- read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, fill=TRUE)
            DATA
        }, error=function(cond){
            print("No lines in input file.")
            print(cond)
            data.frame()
        })
        lines <- DATA
        
        if(nrow(lines) < 1){
            return(FALSE)
        }
        tmpIDList <- lapply(seq_len(nrow(lines)), function(i){
            tmpSplit <- lines[i, ]
            if (tmpSplit[[sigColumnIndex]] == "" | is.na(tmpSplit[[sigColumnIndex]]) | grepl("^\\D", tmpSplit[[sigColumnIndex]]) | tmpSplit[[sigColumnIndex]] >= sigCutoff | tmpSplit[[10]] < 1) {
                return(NULL)
            } else {
                return(paste0(tmpSplit[[1]], "|", tmpSplit[[2]]))
            }
            return(NULL)
        })
        #Get not null elements
        tmpIDList <- tmpIDList[!vapply(tmpIDList, is.null, FUN.VALUE=logical(1))]
        tmpIDList <- unlist(tmpIDList, recursive=FALSE)
        
        if(is.null(tmpIDList)){
            return(NULL)
        }
        
        # Cut off entries over the limit
        if(length(tmpIDList) > as.double(topTermLimit)){
            tmpIDList <- tmpIDList[seq_len(as.double(topTermLimit))]
        }
        tmp_ID2Term <- tmpIDList
        names(tmp_ID2Term) <- lapply(tmpIDList, function(x) unlist(str_split(x, "\\|"))[2])
        tmp_ID2Class <- unlist(lapply(tmpIDList, function(x) unlist(str_split(x, "\\|"))[1]))
        names(tmp_ID2Class) <- names(tmp_ID2Term)
        
        return(list(ID2Term=tmp_ID2Term, ID2Class=tmp_ID2Class))
    })
    
    getSigTermsFilteredTerms <- lapply(seq_len(length(getSigTerms)), function(i) getSigTerms[[i]]["ID2Term"])
    getSigTermsFilteredClasses <- lapply(seq_len(length(getSigTerms)), function(i) getSigTerms[[i]]["ID2Class"])
    ID2Term <- unlist(unname(unlist(getSigTermsFilteredTerms, recursive=FALSE)), recursive=FALSE)
    ID2Class <- unlist(unname(unlist(getSigTermsFilteredClasses, recursive=FALSE)), recursive=FALSE)
    # remove NA
    ID2Term <- ID2Term[!vapply(ID2Term, is.na, FUN.VALUE=logical(1))]
    ID2Class <- ID2Class[!vapply(ID2Class, is.na, FUN.VALUE=logical(1))]
    if (length(ID2Class) > 0){
        IDs <- paste0(ID2Class, " | ", names(ID2Class))
    } else {
        IDs <- list()
    }
    # Check term file and load
    fileHeaderNames <- lapply(infiles, function(infile) {
        tmp1 <- unlist(str_split(infile, "/"))
        tmpNameSplit <- str_split(tmp1[length(tmp1)], "__Chart.txt")
        shortFileBaseName <- tmpNameSplit[1]
        tmp2 <- unlist(str_split(tmp1[length(tmp1)], ".xls"))
        if(length(tmp2[1]) == length(tmp1[length(tmp1)])) {
            tmp2 <- unlist(str_split(tmp1[length(tmp1)], ".txt"))
        }
        tmp3 <- unlist(str_split(tmp2[1], "__Chart"))
        return(tmp3[1])
    })
    mergedData <- lapply(infiles, function(infile) {
        tmp1 <- unlist(str_split(infile, "/"))
        tmpNameSplit <- str_split(tmp1[length(tmp1)], "__Chart.txt")
        shortFileBaseName <- tmpNameSplit[1]
        tmp2 <- unlist(str_split(tmp1[length(tmp1)], ".xls"))
        if(length(tmp2[1]) == length(tmp1[length(tmp1)])) {
            tmp2 <- unlist(str_split(tmp1[length(tmp1)], ".txt"))
        }
        tmp3 <- unlist(str_split(tmp2[1], "__Chart"))
        # Check term file and load
        DATA <- data.frame()
        DATA <- tryCatch(read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, fill=TRUE), error=function(cond) {
            print("No lines in input file.")
            print(cond)
            data.frame()
        })
        if (nrow(DATA) > 0){
            annotationNamesFull <- unlist(lapply(seq_len(nrow(DATA)), function(line) {
                tmpSplit <- DATA[line, ]
                return(paste0(tmpSplit[[1]], " | ", tmpSplit[[2]]))
            }))
            return(data.frame("Set"=tmp3[1], "Annotation"=annotationNamesFull, DATA, stringsAsFactors=FALSE))
        }
        return(NULL)
    })
    mergedData <- bind_rows(mergedData, .id="Label")
    mergedData <- split(mergedData, mergedData$Annotation)
    pvalueMatrix <- lapply(mergedData, function(x){
        pvalueMatrix_inner <- unlist(lapply(seq_len(length(x[[sigDFIndex]])), function(y) {
            if (x[y, sigDFIndex] == "" | is.null(x[y, sigDFIndex]) | is.null(x[y, "Fold.Enrichment"]) | grepl("^\\D", x[y, sigDFIndex]) | (mode == "ALL" & x[y, sigDFIndex] >= sigCutoff) | x[y, "Fold.Enrichment"] < 1) {
                return(NULL)
            }
            pvalueLog <- -1 * log10(as.double(x[y, sigDFIndex]))
            if(!is.finite(pvalueLog)){
                pvalueLog <- 500 # set to 500 if pvalue is effectively 0 (arbitrary)
            }
            return(pvalueLog)
        }))
        if(is.null(pvalueMatrix_inner)) {
            return(NULL)
        }
        names(pvalueMatrix_inner) <- unlist(lapply(seq_len(length(x[[sigDFIndex]])), function(y) {
            if (x[y, sigDFIndex] == "" | is.null(x[y, sigDFIndex]) | is.null(x[y, "Fold.Enrichment"]) | grepl("^\\D", x[y, sigDFIndex]) | (mode == "ALL" & x[y, sigDFIndex] >= sigCutoff) | x[y, "Fold.Enrichment"] < 1) {
                return(NULL)
            }
            return(x[y, "Set"])
        }))
        pvalueMatrix_inner <- pvalueMatrix_inner[!vapply(pvalueMatrix_inner, is.null, FUN.VALUE=logical(1))]
        return(pvalueMatrix_inner)
    })
    names(pvalueMatrix) <- names(mergedData)
    pvalueMatrix <- pvalueMatrix[!vapply(pvalueMatrix, is.null, FUN.VALUE=logical(1))]
    
    # Create a summary file
    summaryFileName <- paste0(summaryFileNameBase, "__ValueMatrix.txt")
    SUMMARY <- tryCatch({
        file.create(paste0(outputDir, summaryFileName))
        SUMMARY <- file(paste0(outputDir, summaryFileName))
        SUMMARY
    }, error=function(cond){
        print("Error creating summary file.")
        print(cond)
        NULL
    })
    if(is.null(SUMMARY)){
        return(NULL)
    }
    fileHeaderNames <- unlist(fileHeaderNames)
    fileHeaderNames <- sort_by_file_number(fileHeaderNames)
    writeToSummaryHeader <- paste0("GROUP\tID\tTerms\t", paste0(fileHeaderNames, collapse="\t"), "\n")
    IDs <- unique(IDs)
    writeToSummary <- lapply(IDs, function(ID) {
        IDSplit <- unlist(str_split(ID, " \\| "))
        writeToSummaryLabel <- paste0(IDSplit[1], "\t", ID, "\t", IDSplit[2])
        # writeToSummaryLabel <- paste0(ID2Class[ID], "\t", ID)
        writeToSummary2 <- lapply(fileHeaderNames, function(header){
            if (!is.null(pvalueMatrix[[ID]])){
                if(header %in% names(pvalueMatrix[[ID]])) {
                    return(paste0(pvalueMatrix[[ID]][header]))
                } else {
                    return("0")
                }
            }
            return("#")
        })
        writeToSummary2 <- paste0(writeToSummary2, collapse="\t")
        writeToSummary2 <- gsub("#", "", writeToSummary2)
        return(paste0(writeToSummaryLabel, "\t", writeToSummary2))
    })
    writeToSummary <- paste0(writeToSummary, collapse="\n")
    fcheck <- tryCatch({
        write(paste0(writeToSummaryHeader, writeToSummary), SUMMARY, append=TRUE)
        close(SUMMARY)
        TRUE
    }, error=function(cond){
        print("Error creating summary file.")
        print(cond)
        FALSE
    })
    if(fcheck == FALSE){
        return(NULL)
    }
    
    # Create a network summary file for Chart
    forNetworkFile <- paste0(summaryFileNameBase, "__ValueMatrix.ForNet")
    NETWORK <- tryCatch({
        file.create(paste0(outputDir, forNetworkFile))
        NETWORK <- file(paste0(outputDir, forNetworkFile))
        NETWORK
    }, error=function(cond){
        print("Error creating summary file.")
        print(cond)
        NULL
    })
    if(is.null(NETWORK)){
        return(NULL)
    }
    writeToNetworkHeader <- paste0("GROUPID\tUID\tTerms\t", paste0(fileHeaderNames, collapse="\t"), "\n")
    writeToNetwork <- lapply(IDs, function(ID) {
        idTermSplits <- unlist(str_split(ID, " \\| "))
        tmpHashRef <- classID2annotationTerm2termUniqueID[[className2classID[[idTermSplits[1]]]]]
        writeToNetwork2 <- paste0(className2classID[[idTermSplits[1]]], "\t", tmpHashRef[[idTermSplits[2]]], "\t", idTermSplits[2])
        writeToNetwork_inner <- unname(unlist(lapply(fileHeaderNames, function(header) {
            if (!is.null(pvalueMatrix[[ID]])) {
                return(pvalueMatrix[[ID]][header])
            }
            return("#") # delete these later
        })))
        writeToNetwork_inner <- paste0(writeToNetwork_inner, collapse="\t")
        writeToNetwork_inner <- gsub("#", "", writeToNetwork_inner, fixed=TRUE)
        writeToNetwork2 <- paste0(writeToNetwork2, "\t", writeToNetwork_inner)
    })
    writeToNetwork <- paste0(writeToNetwork, collapse="\n")
    fcheck <- tryCatch({
        write(paste0(writeToNetworkHeader, writeToNetwork), NETWORK, append=TRUE)
        close(NETWORK)
        TRUE
    }, error=function(cond){
        print("Error creating summary file")
        print(cond)
        FALSE
    })
    if(fcheck == FALSE){
        return(NULL)
    }
    
    # Create a gct file from ValueMatrix
    fcheck <- tryCatch({
        INFILE <- read.delim(paste0(outputDir, summaryFileNameBase, "__ValueMatrix.txt"), sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, fill=TRUE)
        file.create(paste0(outputDir, summaryFileNameBase, "__ValueMatrix.gct"))
        OUTFILE <- file(paste0(outputDir, summaryFileNameBase, "__ValueMatrix.gct"))
        headerLine <- paste0(colnames(INFILE), collapse="\t")
        headerSplit <- unlist(str_split(headerLine, "\t"))
        sampleCnt <- length(headerSplit) - 3
        geneCnt <- 0
        content <- ""
        if(nrow(INFILE) > 0) {
            content <- lapply(seq_len(nrow(INFILE)), function(l) {
                line <- paste0(INFILE[l, ], collapse="\t")
                if (line == "") {
                    return(NULL)
                } else {
                    tmpSplit <- unlist(str_split(line, "\t"))
                    tmpSplit <- unlist(lapply(seq_len((sampleCnt+3)), function(i) {
                        if(i >= 4){
                            if (tmpSplit[i] == "NA" | is.na(tmpSplit[i]) | length(tmpSplit[i]) < 1 | is.null(tmpSplit[i])) {
                                return(0)
                            } 
                        }
                        return(tmpSplit[i])
                    }))
                    tmpSplit <- tmpSplit[-1] #Shift
                    return(paste0(tmpSplit, collapse="\t"))
                }
            })
            content <- content[!vapply(content, is.null, FUN.VALUE=logical(1))]
            geneCnt <- length(content)
            content <- paste0(content, collapse="\n")
        }
        write(paste0("#1.2\n", geneCnt, "\t", sampleCnt, "\n", paste0(colnames(INFILE)[-1], collapse="\t"), "\n", content), OUTFILE, append=TRUE)
        close(OUTFILE)
        TRUE
    }, error=function(cond){
        print("Error creating .gct ValueMatrix file.")
        print(cond)
        FALSE
    })
    if(fcheck == FALSE){
        return(NULL)
    }
}

# Sort by file numbers (this is for the situation where the set names are like <Set 1, Set 2, Set 3, etc.>)
sort_by_file_number <- function(originalArray) {
    if (!is.null(originalArray[1]) & grepl("\\w\\d+$", originalArray[1])) {
        # Number
        number2originalNames <- unlist(lapply(seq_len(length(originalArray)), function(name){
            if (grepl("\\w(\\d+)$", originalArray[name])) {
                return(name)
            }
            return(NULL)
        }))
        number2originalNames <- number2originalNames[!vapply(number2originalNames, is.null, FUN.VALUE=logical(1))]
        # Original name
        number2original <- unlist(lapply(originalArray, function(name){
            if (grepl("\\w(\\d+)$", name)) {
                return(name)
            }
            return(NULL)
        }))
        number2original <- number2original[!vapply(number2original, is.null, FUN.VALUE=logical(1))]
        sortedNumbers <- number2original[order(unlist(number2original), decreasing=FALSE)]
        if (length(sortedNumbers) != length(originalArray)) {
            return (originalArray)
        } else {
            return(sortedNumbers)
        }
    }
    return(originalArray)
}

# Perform enrichment analysis
perform_CASRN_enrichment_analysis <- function(CASRNRef, outputBaseDir, outfileBase, mappedCASRNsFromProcess, funCat2Selected, CASRN2funCatTerm, funCatTerm2CASRN, funCat2CASRNCount, funCat2termCount, funCatTerm2CASRNCount, pvalueThresholdToDisplay, similarityThreshold=0.50, initialGroupMembership, multipleLinkageThreshold, EASEThreshold, nodeCutoff=10, enrichmentUUID){
    # Get the name of the current input set
    setName <- outfileBase
    
    # Get pvalue type
    poolInput <- connQueue() # Connect to db
    queryPvalue <- sqlInterpolate(ANSI(), paste0("SELECT pvalue FROM transaction WHERE uuid='", enrichmentUUID, "';"), id="getTransactionPvalue")
    outpPvalue <- dbGetQuery(poolInput, queryPvalue)
    # poolClose(poolInput) # Close pool
    dbDisconnect(poolInput)
    PVALUE_TYPE <- outpPvalue[1, "pvalue"]
    # Get significance column (nominal pvalue or adjusted pvalue (BH-correction))
    sigDFIndex <- "PValue"
    if(PVALUE_TYPE == "nominal"){
        sigColumnName <- "P"
        sigDFIndex <- "PValue"
    } else if(PVALUE_TYPE == "adjusted") {
        sigColumnName <- "BH"
        sigDFIndex <- "Benjamini_Hochberg"
    }
    sigColumnIndex <- get_column_index(sigColumnName)

    # Define output file names
    outfileChart <- paste0(outputBaseDir, outfileBase, "__Chart.txt")
    outfileSimple <- paste0(outputBaseDir, outfileBase, "__ChartSimple.txt")
    outfileMatrix <- paste0(outputBaseDir, outfileBase, "__Matrix.txt")
    
    # Open file connections
    OUTFILE <- file(outfileChart)
    SIMPLE <- file(outfileSimple)
    MATRIX <- outfileMatrix
    
    # Create directories if they don't exist
    dir.create(paste0(outputBaseDir))
    
    # Initialize headers for Chart files
    chartHeader <- "Category\tTerm\tCount\t%\tPValue\tCASRNs\tList Total\tPop Hits\tPop Total\tFold Enrichment\tBonferroni\tBenjamini_Yekutieli\tBenjamini_Hochberg\n"
    simpleHeader <- ""
    if(PVALUE_TYPE == "nominal"){
        simpleHeader <- paste0("Category\tTerm\tCount\t%\tPValue\tFold Enrichment\n")
    } else if(PVALUE_TYPE == "adjusted") {
        simpleHeader <- paste0("Category\tTerm\tCount\t%\tPValue\tFold Enrichment\t", sigDFIndex, "\n")
    }
    # Calculate EASE score
    inputCASRNs <- CASRNRef
    inputCASRNsCount <- length(inputCASRNs)
    mappedCASRNs <- mappedCASRNsFromProcess # Among the CASRNs, use only those included in the full Tox21 list
    
    # Populate sigTerm2CASRNMatrix - this is a named list of each annotation (name) with all associated input CASRNs (list contents)
    sigTerm2CASRNMatrix <- funCatTerm2CASRN[names(funCat2Selected)] # filter selected annotation classes
    sigTerm2CASRNMatrix <- unlist(mclapply(names(sigTerm2CASRNMatrix), mc.cores=CORES, mc.silent=FALSE, function(inner_list){
        tmp_sigTerm2CASRNMatrix <- lapply(sigTerm2CASRNMatrix[[inner_list]], function(inner_casrns) inner_casrns[inner_casrns %in% mappedCASRNs]) # filter to just CASRNs in the input set
        names(tmp_sigTerm2CASRNMatrix) <- lapply(names(sigTerm2CASRNMatrix[[inner_list]]), function(x) paste0(inner_list, "|", x))
        return(tmp_sigTerm2CASRNMatrix)
    }), recursive=FALSE)
    sigTerm2CASRNMatrix <- sigTerm2CASRNMatrix[lapply(sigTerm2CASRNMatrix, length ) > 0]
    
    # localTermsList - number of annotations in each category
    localTermsList <- mclapply(names(funCat2Selected), mc.cores=CORES, mc.silent=FALSE, function(x){
        casrns_filtered <- CASRN2funCatTerm[mappedCASRNs]
        tmp_localTermsList <- lapply(casrns_filtered, function(inner_list) {
            if(length(inner_list[[x]]) < 1){
                return(NULL)
            }
            return(1)
        })
        return(length(tmp_localTermsList[!vapply(tmp_localTermsList, is.null, FUN.VALUE=logical(1))]))
    })
    names(localTermsList) <- names(funCat2Selected)
    funCat2SelectedProcessed_datArray <- funCatTerm2CASRN[names(funCat2Selected)] # filter selected annotation classes
    funCat2SelectedProcessed_datArray <- mclapply(names(funCat2Selected), mc.cores=CORES, mc.silent=FALSE, function(funCat) {
        funCatTerms <- funCatTerm2CASRN[[funCat]]
        targetTotalCASRNInFunCatCount <- length(unlist(lapply(mappedCASRNs, function(CASRN){
            if(funCat %in% names(CASRN2funCatTerm[[CASRN]])){
                return(CASRN)
            }
            return(NULL)
        })))
        tmp_datArray <- lapply(names(funCatTerms), function(term_inner){
            targetCASRNsRef <- unlist(lapply(mappedCASRNs, function(CASRN){
                if(CASRN %in% funCatTerm2CASRN[[funCat]][[term_inner]]){
                    return(CASRN)
                }
                return(NULL)
            }))
            targetCASRNCount <- length(targetCASRNsRef)
            
            # Calculate the EASE score
            if (targetCASRNCount > 1){
                
                np1 <- targetTotalCASRNInFunCatCount - 1
                n11 <- targetCASRNCount - 1
                npp <- funCat2CASRNCount[[funCat]]
                n1p <- funCatTerm2CASRNCount[[funCat]][[term_inner]]
                
                # skip any under-represented terms
                datArrayLine <- list(n11, (n1p - n11), (np1 - n11), (npp - n1p - np1 + n11))
                annoArrayLine <- list(funCat, term_inner, targetCASRNCount, round((targetCASRNCount / inputCASRNsCount * 100), digits=15), 1, paste(unlist(unname(vapply(targetCASRNsRef, paste, FUN.VALUE=character(1), collapse=", "))), collapse=', '), targetTotalCASRNInFunCatCount, n1p, npp, (targetCASRNCount / targetTotalCASRNInFunCatCount) / (n1p / npp))
                return(list(datarray=datArrayLine, annoarray=annoArrayLine))
            }
            return(NULL)
        })
        return(tmp_datArray[!vapply(tmp_datArray, is.null, FUN.VALUE=logical(1))])
    })
    funCat2SelectedProcessed_datArray <- funCat2SelectedProcessed_datArray[!vapply(funCat2SelectedProcessed_datArray, is.null, FUN.VALUE=logical(1))]
    funCat2SelectedProcessed_datArray <- unlist(funCat2SelectedProcessed_datArray, recursive=FALSE)
    
    datArray <- lapply(funCat2SelectedProcessed_datArray, function(x) x$datarray)
    annoArray <- lapply(funCat2SelectedProcessed_datArray, function(x) x$annoarray)

    # Save the data file -> create "RINPUT" but as a data frame here
    RINPUT_df <- do.call(rbind, datArray)
    # Handle if bad RINPUT file
    if(is.null(RINPUT_df)){
        # Write blank files to chart, simple, and matrix
        tryCatch({
            writeLines(chartHeader, OUTFILE)
            writeLines(simpleHeader, SIMPLE)
            writeLines(paste0(""), MATRIX)
            close(OUTFILE)
            close(SIMPLE)
            # Open and create a blank cluster file
            outfileCluster <- paste0(outputBaseDir, outfileBase, "__Cluster.txt")
            file.create(outfileCluster)
        }, error=function(cond){
            print("Error creating error files.")
            print(cond)
        })
        return("no annotations found for input")
    } else if(nrow(RINPUT_df) < 2){
        # Write blank files to chart, simple, and matrix
        tryCatch({
            writeLines(chartHeader, OUTFILE)
            writeLines(simpleHeader, SIMPLE)
            writeLines(paste0(""), MATRIX)
            close(OUTFILE)
            close(SIMPLE)
            # Open and create a blank cluster file
            outfileCluster <- paste0(outputBaseDir, outfileBase, "__Cluster.txt")
            file.create(outfileCluster)
        }, error=function(cond){
            print("Error creating error files.")
            print(cond)
        })
        return("no annotations found for input")
    }
    rownames(RINPUT_df) <- seq_len(nrow(RINPUT_df))
    colnames(RINPUT_df) <- c("X1", "X2", "X3", "X4")
    
    # Calculate results of the fisher test - P-value, Bonferroni, BY, and Benjamini_Hochberg
    # Bonferroni = bonferroni
    # Benjamini_Yekutieli = BY
    # Benjamini_Hochberg = fdr
    p.value <- apply(RINPUT_df, 1, function(x) fisher.test(matrix(unlist(x), nrow=2))$p.value)
    ROUTPUT <- data.frame(p.value=p.value, bonferroni=p.adjust(p.value, method="bonferroni"), by=p.adjust(p.value, method="BY"), fdr=p.adjust(p.value, method="fdr"))
    # Load the output 
    ROutputData <- apply(ROUTPUT, 1, function(line){
        if (length(line) > 0) {
            ROutputLineSplit <- list(p.value=line[['p.value']], bonferroni=line[['bonferroni']], by=line[['by']], fdr=line[['fdr']])
            return(ROutputLineSplit)
        }
        return(NULL)
    })
    ROutputData <- ROutputData[!vapply(ROutputData, is.null, FUN.VALUE=logical(1))]
    
    # Integrate ROutput into the main hashes/arrays
    # Error checking/handling for the case the number of lines are different between @annoArray and @ROutputData
    if(length(annoArray) != length(ROutputData)) {
        # Write blank files to chart, simple, and matrix
        tryCatch({
            writeLines(chartHeader, OUTFILE)
            writeLines(simpleHeader, SIMPLE)
            writeLines(paste0(""), MATRIX)
            close(OUTFILE)
            close(SIMPLE)
            # Open and create a blank cluster file
            outfileCluster <- paste0(outputBaseDir, outputBaseDir, "__Cluster.txt")
            file.create(outfileCluster)
        }, error=function(cond){
            print("Error creating error files.")
            print(cond)
        })
        # Initialize db connection pool
        poolStatus <- connQueue()
        # Set step flag for each set name
        query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=-1 WHERE uuid='", enrichmentUUID, "' AND setname='", setName, "';"), id="fetchStatus")
        outp <- dbExecute(poolStatus, query)
        # Check if no errors
        query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
        requestCancel <- dbGetQuery(poolStatus, query)
        # Close pool
        # poolClose(poolStatus)
        dbDisconnect(poolStatus)
        if(nrow(requestCancel) > 0){
            print(paste0("Canceling request: ", enrichmentUUID))
            return("request canceled")
        }
        return("problem calculating annotation representation for given input")
    }

    # Remove empty elements in annoArray
    annoArray <- annoArray[lengths(annoArray) > 0L]
    annoArray <- lapply(seq_len(length(annoArray)), function(annoArrayIndex){
        return(data.frame(
            category=annoArray[[annoArrayIndex]][[1]], # annotation category
            term=annoArray[[annoArrayIndex]][[2]], # annotation term name
            count=as.numeric(annoArray[[annoArrayIndex]][[3]]), # count
            percent=as.numeric(annoArray[[annoArrayIndex]][[4]]), # %
            PValue=ROutputData[[annoArrayIndex]][["p.value"]], # update the p-value
            casrn=annoArray[[annoArrayIndex]][[6]], # casrn list
            listtotal=as.numeric(annoArray[[annoArrayIndex]][[7]]), # list total
            pophits=as.numeric(annoArray[[annoArrayIndex]][[8]]), # pop hits
            poptotal=as.numeric(annoArray[[annoArrayIndex]][[9]]), # pop total
            foldenrichment=as.numeric(annoArray[[annoArrayIndex]][[10]]), # fold enrichment
            Bonferroni=ROutputData[[annoArrayIndex]][["bonferroni"]], # Bonferroni
            Benjamini_Yekutieli=ROutputData[[annoArrayIndex]][["by"]], # Benjamini_Yekutieli
            Benjamini_Hochberg=ROutputData[[annoArrayIndex]][["fdr"]] # Benjamini_Hochberg
        ))
    })
    annoArray <- do.call(rbind.data.frame, annoArray)
    
    term2Contents <- unlist(apply(annoArray, 1, function(x) {
        if(length(x) > 0){
            tmp <- list(c(
                x[["category"]],
                x[["term"]],
                x[["count"]],
                x[["percent"]],
                x[["PValue"]],
                x[["casrn"]],
                x[["listtotal"]],
                x[["pophits"]],
                x[["poptotal"]],
                x[["foldenrichment"]],
                x[["Bonferroni"]],
                x[["Benjamini_Yekutieli"]],
                x[["Benjamini_Hochberg"]]
            ))
            
            names(tmp) <- paste0(x[["category"]], "|", x[["term"]])
            return(tmp)
        } else {
            return(NULL)
        }
    }), recursive=FALSE)
    
    term2Pvalue <- unlist(apply(annoArray, 1, function(x) {
        if(length(x) > 0){
            tmp <- list(c(as.numeric(x[[sigDFIndex]])))
            names(tmp) <- paste0(x[["category"]], "|", x[["term"]])
            return(tmp)
        } else {
            return(NULL)
        }
    }), recursive=FALSE)
    
    # Sort by the p-values across multiple funCat
    sortedFunCatTerms <- term2Pvalue[order(unlist(term2Pvalue), decreasing=FALSE)]
    sortedFunCatTermsCount <- length(sortedFunCatTerms)
    sortedFunCatTerms_CHART <-sortedFunCatTerms # save to temp variable
    
    # Write blank files to chart if no items
    if(length(sortedFunCatTerms_CHART) < 1){
        tryCatch({
            writeLines(chartHeader, OUTFILE)
            close(OUTFILE)
        }, error=function(cond){
            print("Error creating error files.")
            print(cond)
        })
    } else {
        tryCatch({
            # Write to chart File
            writeToChart <- unlist(lapply(names(sortedFunCatTerms_CHART), function(funCatTerm) paste0(term2Contents[[funCatTerm]], collapse='\t')))
            writeToChart <- writeToChart[!vapply(writeToChart, is.null, FUN.VALUE=logical(1))]
            writeLines(paste0(chartHeader, paste0(writeToChart, collapse="\n")), OUTFILE)
            close(OUTFILE)
        }, error=function(cond){
            print("Error: could not write to Chart file.")
            print(cond)
        })
    }
    
    tryCatch({
        # Write to simple chart file
        sortFunCatProcess <- lapply(names(sortedFunCatTerms_CHART), function(funCatTerm){ 
            tmpSplit <- term2Contents[[funCatTerm]]
            if (as.numeric(tmpSplit[[10]]) > 1){
                if(PVALUE_TYPE == "nominal"){
                    return(list(tmpSplit[[1]], tmpSplit[[2]], tmpSplit[[3]], tmpSplit[[4]], tmpSplit[[5]], tmpSplit[[10]]))
                } else if(PVALUE_TYPE == "adjusted") {
                    return(list(tmpSplit[[1]], tmpSplit[[2]], tmpSplit[[3]], tmpSplit[[4]], tmpSplit[[5]], tmpSplit[[10]], tmpSplit[[sigColumnIndex]]))
                }
            }
            return(NULL)
        })
        
        sortFunCatProcess <- do.call(rbind, sortFunCatProcess)
        sortFunCatProcess <- data.frame(sortFunCatProcess, stringsAsFactors=FALSE)
        rownames(sortFunCatProcess) <- seq_len(nrow(sortFunCatProcess))
        
        if(PVALUE_TYPE == "nominal"){
            colnames(sortFunCatProcess) <- c("Category", "Term", "Count", "Percent", "PValue", "Fold Enrichment")
        } else if(PVALUE_TYPE == "adjusted") {
            colnames(sortFunCatProcess) <- c("Category", "Term", "Count", "Percent", "PValue", "Fold Enrichment", sigDFIndex)
        }
        
        # filter by pvalue (nominal or adjusted)
        if(sigDFIndex == "PValue"){
            sortFunCatProcess <- sortFunCatProcess[sortFunCatProcess$PValue < 0.05, ]
        } else if(sigDFIndex == "Benjamini_Hochberg"){
            sortFunCatProcess <- sortFunCatProcess[sortFunCatProcess$Benjamini_Hochberg < 0.05, ]
        }
        
        sortFunCatProcessNames <- unique(sortFunCatProcess$Category)
        sortFunCatProcess <- lapply(sortFunCatProcessNames, function(x) sortFunCatProcess[sortFunCatProcess$Category == x, ])
        names(sortFunCatProcess) <- sortFunCatProcessNames
        sortFunCatProcess <- lapply(sortFunCatProcess, function(x){
            tmp <- x
            if(sigDFIndex == "PValue"){
                tmp <- arrange(tmp, PValue)
            } else if(sigDFIndex == "Benjamini_Hochberg"){
                tmp <- arrange(tmp, Benjamini_Hochberg)
            }
            tmp <- x %>% slice(seq_len(nodeCutoff))
            return(tmp)
        }) # trim off anything per category above the Top N cutoff
        
        sortFunCatProcess <- do.call(rbind, sortFunCatProcess)
        sortFunCatProcess <- as.data.frame(sortFunCatProcess)
        
        if(sigDFIndex == "PValue"){
            sortFunCatProcess$PValue <- as.numeric(as.character(sortFunCatProcess$PValue))
            sortFunCatProcess <-sortFunCatProcess[order(sortFunCatProcess$PValue), ]
        } else if(sigDFIndex == "Benjamini_Hochberg"){
            sortFunCatProcess$Benjamini_Hochberg <- as.numeric(as.character(sortFunCatProcess$Benjamini_Hochberg))
            sortFunCatProcess <-sortFunCatProcess[order(sortFunCatProcess$Benjamini_Hochberg), ]
        }
        
        # Write blank files to simple if no items
        if(length(sortFunCatProcess) < 1){
            tryCatch({
                writeLines(simpleHeader, SIMPLE)
                close(SIMPLE)
            }, error=function(cond){
                print("Error creating error files.")
                print(cond)
            })
        } else {
            sortFunCatProcess <- apply(sortFunCatProcess, 1, function(x) paste0(x, collapse="\t"))
            writeLines(paste0(simpleHeader, paste0(sortFunCatProcess, collapse="\n")), SIMPLE)
            close(SIMPLE)
        }
    }, error=function(cond){
        print("Error: could not write to Chart Simple file.")
        print(cond)
    })
    
    # Write to matrix file
    tryCatch({
        # Create matrix header w/ column names
        sortedHeaderTerms <- unlist(unique(names(sigTerm2CASRNMatrix)))[order(unlist(unique(names(sigTerm2CASRNMatrix))), decreasing=FALSE)]
        matrixHeader <- paste0(sortedHeaderTerms, collapse='\t')
        tmp_matrixPrintToFile <- expand.grid(mappedCASRNs, sortedHeaderTerms) %>% arrange("Var1")
        
        # Split matrix into groups by CASRN
        tmp_mat_split <- split(tmp_matrixPrintToFile, mappedCASRNs)
        
        # Check if term is correlated with CASRN for n CASRNs at the same time, where n is the number of CORES available. (parallel processing)
        tmp_mat_split <- mclapply(tmp_mat_split, mc.cores=CORES, mc.silent=FALSE, function(split_set){
            split_casrns <- as.character(unique(split_set[["Var1"]]))
            split_terms <- as.character(unique(split_set[["Var2"]]))
            tmp_matrixPrintToFile <- apply(split_set, 1, function(mat_row){ 
                if(mat_row[["Var1"]] %in% sigTerm2CASRNMatrix[[mat_row[["Var2"]]]]){
                    return(1)
                }
                return(0)
            })
            matrixPrintToFile <- matrix(tmp_matrixPrintToFile, length(split_casrns), length(split_terms))
            # Add CASRN column to left side of matrix
            matrixPrintToFile <- cbind(split_casrns, matrixPrintToFile)
            colnames(matrixPrintToFile) <- c("CASRN", unlist(split_terms))
            return(matrixPrintToFile)
        })
        
        matrixPrintToFile <- rbind.fill.matrix(tmp_mat_split)
        
        # Write blank files to chart if no items
        if(length(matrixPrintToFile) < 1){
            tryCatch({
                fwrite(data.frame(), file=MATRIX, row.names=FALSE, col.names=TRUE, sep="\t")
            }, error=function(cond){
                print("Error creating error files.")
                print(cond)
            })
        } else {
            # Write matrix to file
            fwrite(matrixPrintToFile, file=MATRIX, row.names=FALSE, col.names=TRUE, sep="\t")
        }
    }, error=function(cond){
        print("Error: could not write to Matrix file.")
        print(cond)
    })

    # Perform functional term clustering
    # Calculate enrichment score
    res <- "success"
    tryCatch({
        res <- kappa_cluster(outputBaseDir=outputBaseDir, outfileBase=outfileBase, sortedFunCatTerms=sortedFunCatTerms, sigTerm2CASRNMatrix=sigTerm2CASRNMatrix, sortedFunCatTermsCount=sortedFunCatTermsCount, inputCASRNsCount=inputCASRNsCount, similarityThreshold=similarityThreshold, initialGroupMembership=initialGroupMembership, multipleLinkageThreshold=multipleLinkageThreshold, EASEThreshold=EASEThreshold, term2Pvalue=term2Pvalue, term2Contents=term2Contents, enrichmentUUID=enrichmentUUID, inputCASRNs=inputCASRNs)
    }, error=function(cond){
        print("Error: could not write to Cluster file.")
        print(cond)
        res <- "success"
    })
    return(res)
}

kappa_cluster <- function(x, overlap=0.5, outputBaseDir=NULL, outfileBase=NULL, sortedFunCatTerms=NULL, sigTerm2CASRNMatrix=NULL, sortedFunCatTermsCount=NULL, inputCASRNsCount=NULL, similarityThreshold=NULL, initialGroupMembership=NULL, multipleLinkageThreshold=0.5, EASEThreshold=NULL, term2Pvalue=NULL, term2Contents=NULL, enrichmentUUID=NULL, inputCASRNs=NULL){
    # Perform functional term clustering
    # Update status file
    # Connect to DB to get status info
    poolStatus <- connQueue()
    # Set step flag for each set name
    query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=3 WHERE uuid='", enrichmentUUID, "' AND setname='", outfileBase, "' AND step<>-1;"), id="fetchStatus")
    outp <- dbExecute(poolStatus, query)
    # Check if no errors
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
    requestCancel <- dbGetQuery(poolStatus, query)
    # Close pool
    # poolClose(poolStatus)
    dbDisconnect(poolStatus)
    if(nrow(requestCancel) > 0){
        print(paste0("Canceling request: ", enrichmentUUID))
        return("request canceled")
    }
    
    # Get pvalue type
    poolInput <- connQueue() # Connect to db
    queryPvalue <- sqlInterpolate(ANSI(), paste0("SELECT pvalue FROM transaction WHERE uuid='", enrichmentUUID, "';"), id="getTransactionPvalue")
    outpPvalue <- dbGetQuery(poolInput, queryPvalue)
    # poolClose(poolInput) # Close pool
    dbDisconnect(poolInput)
    
    PVALUE_TYPE <- outpPvalue[1, "pvalue"]
    
    # Get significance column (nominal pvalue or adjusted pvalue (BH-correction))
    sigDFIndex <- "PValue"
    if(PVALUE_TYPE == "nominal"){
        sigDFIndex <- "PValue"
    } else if(PVALUE_TYPE == "adjusted") {
        sigDFIndex <- "Benjamini_Hochberg"
    }
    
    # Step#1: Calculate kappa score
    posTermCASRNCount <- lapply(names(sortedFunCatTerms), function(funCatTerm) length(sigTerm2CASRNMatrix[[funCatTerm[[1]]]]))
    names(posTermCASRNCount) <- names(sortedFunCatTerms)
    sortedFunCatTermsPValues <- sortedFunCatTerms
    sortedFunCatTerms <- names(sortedFunCatTerms)
    
    termpair2kappaOverThreshold <- expand.grid(seq_len(sortedFunCatTermsCount - 1), seq_len(sortedFunCatTermsCount), stringsAsFactors=FALSE)
    termpair2kappaOverThreshold <- termpair2kappaOverThreshold %>% filter(termpair2kappaOverThreshold$Var1 < termpair2kappaOverThreshold$Var2)
    
    # TODO: this pairwise comparison is inefficient and slow 
    termpair2kappaOverThreshold <- mclapply(seq_len(nrow(termpair2kappaOverThreshold)), mc.cores=CORES, mc.silent=FALSE, function(term_row) {
        term1 <- termpair2kappaOverThreshold[term_row, "Var1"] # get the first term in the pariwise comparison
        term2 <- termpair2kappaOverThreshold[term_row, "Var2"] # get the second term in the pariwise comparison
        posTerm1Total <- posTermCASRNCount[[sortedFunCatTerms[term1]]] # number of CASRNs associated with first annotation
        posTerm2Total <- posTermCASRNCount[[sortedFunCatTerms[term2]]] # number of CASRNs associated with second annotation
        negTerm1Total <- inputCASRNsCount - posTerm1Total # number of CASRNs NOT associated with first annotation (note that the total is inputCASRNsCount not the mapped total)
        negTerm2Total <- inputCASRNsCount - posTerm2Total # number of CASRNs NOT associated with second annotation (note that the total is inputCASRNsCount not the mapped total)
        
        # Get number of chemicals that are shared or not for term1 and term2
        tmpMat1 <- sigTerm2CASRNMatrix[[sortedFunCatTerms[term1]]]
        tmpMat2 <- sigTerm2CASRNMatrix[[sortedFunCatTerms[term2]]]
        sharedTerms <- intersect(tmpMat1, tmpMat2) # TODO ~1 min on large set - any way to speed this up? short time, but will add up over millions of iterations
        term1term2 <- length(sharedTerms)
        term1only <- length(tmpMat1) - length(sharedTerms)
        term2only <- length(tmpMat2) - length(sharedTerms)
        term1term2Non <- inputCASRNsCount - term1term2 - term1only - term2only
        
        # Calculate the kappa score - http://david.abcc.ncifcrf.gov/content.jsp?file=linear_search.html
        Oab <- (term1term2 + term1term2Non) / inputCASRNsCount
        Aab <- ((posTerm1Total * posTerm2Total) + (negTerm1Total * negTerm2Total)) / (inputCASRNsCount * inputCASRNsCount)
        if (Aab != 1) {
            Kappa <- round((Oab - Aab) / (1 - Aab), 2)
            if (Kappa > similarityThreshold) {
                ijFrame <- list(iTerm=paste0(sortedFunCatTerms[term2]), jTerm=paste0(sortedFunCatTerms[term1]))
                names(ijFrame) <- c(sortedFunCatTerms[term1], sortedFunCatTerms[term2])
                return(ijFrame)
            }
            return(NULL)
        }
        return(NULL)
    })
    termpair2kappaOverThreshold <- unlist(termpair2kappaOverThreshold[!vapply(termpair2kappaOverThreshold, is.null, FUN.VALUE=logical(1))], recursive=FALSE)
    if(length(termpair2kappaOverThreshold) < 1) {
        return("problem calculating kappa values for associated annotations")
    }
    termpair2kappaOverThreshold <- lapply(sortedFunCatTerms, function(term) unname(unlist(termpair2kappaOverThreshold[names(termpair2kappaOverThreshold) == term])))
    names(termpair2kappaOverThreshold) <- sortedFunCatTerms
    sortedFunCatTermsCount <- length(sortedFunCatTerms)
    
    # Step#2: Create qualified initial seeding groups
    # Each term could form a initial seeding group (initial seeds) as long as it has close relationships (kappa > 0.35 or any designated number) with more than > 2 or any designated number of other members. 
    # Update status file
    # Connect to DB to get status info
    poolStatus <- connQueue()
    # Set step flag for each set name
    query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=4 WHERE uuid='", enrichmentUUID, "' AND setname='", outfileBase, "' AND step<>-1;"), id="fetchStatus")
    outp <- dbExecute(poolStatus, query)
    # Check if no errors
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
    requestCancel <- dbGetQuery(poolStatus, query)
    # Close pool
    # poolClose(poolStatus)
    dbDisconnect(poolStatus)
    if(nrow(requestCancel) > 0){
        print(paste0("Canceling request: ", enrichmentUUID))
        return("request canceled")
    }
    
    qualifiedSeeds <- mclapply(seq_len(sortedFunCatTermsCount), mc.cores=CORES, mc.silent=FALSE, function(i){
        # Seed condition #1: initial group membership
        if (!is.null(termpair2kappaOverThreshold[[sortedFunCatTerms[i]]]) & length(termpair2kappaOverThreshold[[sortedFunCatTerms[i]]]) >= (initialGroupMembership - 1)) {
            term2s <- c(sortedFunCatTerms[i], unlist(termpair2kappaOverThreshold[[sortedFunCatTerms[i]]]))
            totalPairs <- length(term2s)*(length(term2s)-1)/2
            passedPair <- 0
            for (n in seq(1, length(term2s) - 1)){
                for (m in seq(n + 1, length(term2s))){
                    if(term2s[m] %in% termpair2kappaOverThreshold[[term2s[n]]]){
                        passedPair <- passedPair + 1
                    }
                }
            }
            over_percentage <- passedPair / totalPairs
            if(over_percentage > multipleLinkageThreshold){
                return(term2s)
            }
            return(NULL)
        }
        return(NULL)
    })
    ml <- qualifiedSeeds[!vapply(qualifiedSeeds, is.null, FUN.VALUE=logical(1))]
    
    # Step 2.5 - Sort qualified seeds by EASE score before merging
    pValueDF <- data.frame(PValue=unlist(unname(sortedFunCatTermsPValues)), stringsAsFactors=FALSE)
    rownames(pValueDF) <- names(sortedFunCatTermsPValues)
    mlSort <- unlist(lapply(ml, function(y) calculate_Enrichment_Score(y, pValueDF)))
    mlIDs <- unlist(lapply(ml, function(y) UUIDgenerate()))
    names(ml) <- mlIDs
    mlSortDF <- data.frame(id=mlIDs, enrichment_score=mlSort, stringsAsFactors=FALSE)
    mlSortDF <- mlSortDF[order(as.numeric(mlSortDF$enrichment_score), decreasing=TRUE), ] # Sort by EASE score in desc. order
    ml <- lapply(mlSortDF$id, function(x) ml[[x]])
    
    #  Step#3: Iteratively merge qualifying seeds
    # Update status file
    # Connect to DB to get status info
    poolStatus <- connQueue()
    # Set step flag for each set name
    query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=5 WHERE uuid='", enrichmentUUID, "' AND setname='", outfileBase, "' AND step<>-1;"), id="fetchStatus")
    outp <- dbExecute(poolStatus, query)
    # Check if no errors
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
    requestCancel <- dbGetQuery(poolStatus, query)
    # Close pool
    # poolClose(poolStatus)
    dbDisconnect(poolStatus)
    if(nrow(requestCancel) > 0){
        print(paste0("Canceling request: ", enrichmentUUID))
        return("request canceled")
    }
    # Merge terms to generate final groups
    res <- merge_term(ml, overlap, multipleLinkageThreshold)
    # Return null if we can't generate any clusters
    if (length(res) < 1){
        return("no clusters could be generated for the given input settings")
    }
    names(res) <- lapply(res, function(cluster) UUIDgenerate()) # assign random uuids to clusters for future reference
    
    # Step#4: Calculate enrichment score and print out the results
    # Update status file
    # Connect to DB to get status info
    poolStatus <- connQueue()
    # Set step flag for each set name
    query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=6 WHERE uuid='", enrichmentUUID, "' AND setname='", outfileBase, "' AND step<>-1;"), id="fetchStatus")
    outp <- dbExecute(poolStatus, query)
    # Check if no errors
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
    requestCancel <- dbGetQuery(poolStatus, query)
    # Close pool
    # poolClose(poolStatus)
    dbDisconnect(poolStatus)
    if(nrow(requestCancel) > 0){
        print(paste0("Canceling request: ", enrichmentUUID))
        return("request canceled")
    }
    
    # Calculate enrichment scores for result (final) clusters
    es <- lapply(res, function(y) calculate_Enrichment_Score(y, pValueDF))
    names(es) <- names(res)
    es <- es[order(unlist(es), decreasing=TRUE)]
    
    # Put results into dataframe
    clusterDF <- data.frame(cluster_num=seq_len(length(es)), cluster_name=names(es), enrichment_score=unlist(es), stringsAsFactors=FALSE)
    clusterDF$enrichment_score <- unlist(lapply(es, function(score) { # round EASE scores
        if(score < 1){
            formatC(score, format="e", digits=2, drop0trailing=TRUE)
        } else {
            formatC(score, format="f", digits=2, drop0trailing=TRUE)
        }
    }))
    
    # Define cluster file path
    outfileCluster <- paste0(outputBaseDir, outfileBase, "__Cluster.txt")
    CLUSTER <- file(outfileCluster)
    # Write to cluster file
    writeToClusterFile <- unlist(apply(clusterDF, 1, function(clusterRow){
        if(as.numeric(clusterRow["enrichment_score"]) > EASEThreshold){
            clusterScore <- paste0("Annotation Cluster ", clusterRow["cluster_num"], "\tEnrichment Score: ", clusterRow["enrichment_score"])
            clusterHeader <- paste0("Category\tTerm\tCount\t%\tPValue\tCASRNs\tList Total\tPop Hits\tPop Total\tFold Enrichment\tBonferroni\tBenjamini_Yekutieli\tBenjamini_Hochberg")
            clusterContentsOrdered <- do.call(rbind.data.frame, lapply(res[[clusterRow["cluster_name"]]], function(termName) term2Contents[[termName]]))
            colnames(clusterContentsOrdered) <- c("Category", "Term", "Count", "%", "PValue", "CASRNs", "List Total", "Pop Hits", "Pop Total", "Fold Enrichment", "Bonferroni", "Benjamini_Yekutieli", "Benjamini_Hochberg")
            clusterContentsOrdered$PValue <- format(clusterContentsOrdered$PValue, scientific=FALSE)
            clusterContentsOrdered$Bonferroni <- format(clusterContentsOrdered$Bonferroni, scientific=FALSE)
            clusterContentsOrdered$Benjamini_Yekutieli <- format(clusterContentsOrdered$Benjamini_Yekutieli, scientific=FALSE)
            clusterContentsOrdered$Benjamini_Hochberg <- format(clusterContentsOrdered$Benjamini_Hochberg, scientific=FALSE)
            clusterContentsOrdered <- clusterContentsOrdered[order(clusterContentsOrdered[[sigDFIndex]], decreasing=FALSE),]
            clusterContentsOrdered <- apply(clusterContentsOrdered, 1, function(clusterContentRow) paste0(clusterContentRow, collapse="\t"))
            clusterContents <- paste0(clusterContentsOrdered, collapse="\n")
            return(paste0(clusterScore, "\n", clusterHeader, "\n", clusterContents, "\n"))
        }
        return(NULL)
    }))
    writeToClusterFile <- writeToClusterFile[!vapply(writeToClusterFile, is.null, FUN.VALUE=logical(1))]
    if(length(writeToClusterFile) > 0){
        writeLines(writeToClusterFile, CLUSTER)
    }
    close(CLUSTER)
    return("success")
}

merge_term <- function(ml, overlap, multipleLinkageThreshold){
    res <- vector("list", length=length(ml))
    resIndex <- 0
    while(length(ml) > 0){
        resIndex <- resIndex + 1
        currentSeed <- 1
        curr <- ml[[currentSeed]]
        ml <- ml[setdiff(seq_len(length(ml)), currentSeed)]
        while(TRUE){
            bestovl <- 0
            bestindex <- NULL
            for(i in seq_len(length((ml)))){
                ovl <- (2 * length(intersect(curr, ml[[i]]))) / (length(curr) + length(ml[[i]]))
                if(ovl > multipleLinkageThreshold){
                    if(bestovl < ovl){
                        bestovl <- ovl
                        bestindex <- i
                    }
                }
            }
            if(bestovl == 0){
                break
            } else {
                curr <- union(curr, ml[[bestindex]])
                ml <- ml[setdiff(seq_len(length(ml)), bestindex)]
            }
        }
        res[[resIndex]] <- curr
    }
    res <- res[!vapply(res, is.null, FUN.VALUE=logical(1))]
    return(res)
}

calculate_Enrichment_Score <- function(x, df){
    pvalue <- df[x, "PValue"]
    tmp <- data.frame(name=x, pvalue=pvalue)
    esp <- unlist(lapply(pvalue, function(y) ifelse(y == 0, 16, (-log(y) / log(10)))))
    return(sum(esp) / length(x))
}

# Fetch all annotations in the Tox21Enricher database for given CASRNs.
getAnnotationsFunc <- function(enrichmentUUID="-1", annoSelectStr=fullAnnoClassStr, nodeCutoff=10) {
    # Connect to db
    poolInput <- connQueue()
    # Get begin time for request
    beginTime <- Sys.time()
    # Set begin time in transaction table
    query <- sqlInterpolate(ANSI(), paste0("UPDATE transaction SET timestamp_started='", beginTime, "' WHERE uuid='", enrichmentUUID, "';"), id="updateTransactionStart")
    outp <- dbExecute(poolInput, query)
    # Close pool
    # poolClose(poolInput)
    dbDisconnect(poolInput)
    
    # Get list of selected annotations
    annoSelect <- unlist(str_split(annoSelectStr, "=checked,"), recursive=FALSE)
    # Get input sets
    inDir <- paste0(APP_DIR, IN_DIR, enrichmentUUID) # Directory for input files for enrichment set
    outDir <- paste0(APP_DIR, OUT_DIR, enrichmentUUID) # Directory for output files for enrichment set
    inputSets <- Sys.glob(paste0(inDir, "/*"))
    annotationMatrix <- mclapply(inputSets, mc.cores=CORES, mc.silent=FALSE, function(infile){
        # Get set name
        setNameSplit <- unlist(str_split(infile, "/"))
        setNameSplit2 <- unlist(str_split(setNameSplit[length(setNameSplit)], "\\."))
        setName <- setNameSplit2[1]
        
        # Read input files
        input <- tryCatch({
            read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=FALSE)
        }, error=function(e){
            return(NULL)
        })
        if(length(input) < 1) {
            return(NULL)
        }
        
        # Get just the CASRNs
        inputCASRNs <- input[, 1]
        
        # Read status file and update step
        # Connect to DB to get status info
        poolStatus <- connQueue()
        # Set step flag for each set name
        query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=2 WHERE uuid='", enrichmentUUID, "' AND setname='", setName, "' AND step<>-1;"), id="fetchStatus")
        outp <- dbExecute(poolStatus, query)
        # Check if no errors
        query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
        requestCancel <- dbGetQuery(poolStatus, query)
        # Close pool
        # poolClose(poolStatus)
        dbDisconnect(poolStatus)
        if(nrow(requestCancel) > 0){
            print(paste0("Canceling request: ", enrichmentUUID))
            return(return("request canceled"))
        }
        
        # Get the corresponding annotations for each input CASRN
        annotations <- lapply(inputCASRNs, function(CASRN){
            # Connect to db
            poolMatrix <- conn()
            # Grab matrix
            queryMatrix <- sqlInterpolate(ANSI(), paste0("
                SELECT
                    concat(ac.annoclassname , '\t', ad.annoterm) as annotation
                FROM
                    annotation_detail ad,
                    annotation_class ac,
                    term2casrn_mapping tcm,
                    chemical_detail cd
                WHERE
                    tcm.annoclassid = ac.annoclassid
                AND tcm.annotermid = ad.annotermid
                AND tcm.casrnuid = cd.casrnuid
                AND cd.casrn='", CASRN, "';"
            ))
            
            outpMatrix <- tryCatch({
                dbGetQuery(poolMatrix, queryMatrix)
            }, error=function(e){
                print(e)
                return(NULL)
            })
            # poolClose(poolMatrix)
            dbDisconnect(poolMatrix)
            if(nrow(outpMatrix) > 0){
                return(outpMatrix$annotation)
            }
            return(NULL)
        })
        # Set list names to CASRNs
        names(annotations) <- inputCASRNs
        
        # Read status file and update step
        # Connect to DB to get status info
        poolStatus <- connQueue()
        # Set step flag for each set name
        query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=3 WHERE uuid='", enrichmentUUID, "' AND setname='", setName, "' AND step<>-1;"), id="fetchStatus")
        outp <- dbExecute(poolStatus, query)
        # Check if no errors
        query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
        requestCancel <- dbGetQuery(poolStatus, query)
        # Close pool
        # poolClose(poolStatus)
        dbDisconnect(poolStatus)
        if(nrow(requestCancel) > 0){
            print(paste0("Canceling request: ", enrichmentUUID))
            return("request canceled")
        }

        # Create output files for each CASRN in the Set
        individualMatrix <- lapply(inputCASRNs, function(CASRN){
            print(paste0("Creating output file at: ", paste0(outDir, "/", setName, "__", CASRN, ".txt")))
            tmp_fetchedTerms <- tryCatch({
                file.create(paste0(outDir, "/", setName, "__", CASRN, ".txt"))
                OUTPUT <- file(paste0(outDir, "/", setName, "__", CASRN, ".txt"))
                outAnnotationList <- annotations[[CASRN]]
                # Write annotations to output file
                if(!is.null(outAnnotationList)){
                    writeLines(paste0(outAnnotationList, collapse="\n"), OUTPUT)
                } else { # blank file if no matches
                    writeLines("No matching annotations.", OUTPUT)
                }
                close(OUTPUT)
                return(outAnnotationList)
            }, error=function(cond){
                print("Error creating annotation file.")
                print(cond)
                return(NULL)
            })
            return(tmp_fetchedTerms)
        })
        # Set list names to CASRNs
        # Merge class and annotation names
        individualMatrix <- lapply(individualMatrix, function(x) fixedInnerList <- unlist(lapply(x, function(y) gsub("\t", "\\|", y))))
        names(individualMatrix) <- inputCASRNs
        combinedMatrix <- unlist(unname(individualMatrix), recursive=FALSE)
        
        # Read status file and update step
        # Connect to DB to get status info
        poolStatus <- connQueue()
        # Set step flag for each set name
        query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=4 WHERE uuid='", enrichmentUUID, "' AND setname='", setName, "' AND step<>-1;"), id="fetchStatus")
        outp <- dbExecute(poolStatus, query)
        # Check if no errors
        query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
        requestCancel <- dbGetQuery(poolStatus, query)
        # Close pool
        # poolClose(poolStatus)
        dbDisconnect(poolStatus)
        if(nrow(requestCancel) > 0){
            print(paste0("Canceling request: ", enrichmentUUID))
            return(FALSE)
        }
        
        # Create matrix file for all CASRNs in set
        tmp_MatrixOut <- tryCatch({
            file.create(paste0(outDir, "/", setName, "__FullMatrix.txt"))
            MATRIX <- file(paste0(outDir, "/", setName, "__FullMatrix.txt"))
            matrixHeader <- paste0(combinedMatrix, collapse='\t')
            matrixOutput <- paste0("CASRN\t", matrixHeader, "\n\n")
            matrixOutputFull <- lapply(inputCASRNs, function(CASRN){
                matrixOutput <- paste0(matrixOutput, CASRN)
                matrixOutputLine <- lapply(combinedMatrix, function(matrixItem){
                    if (matrixItem %in% individualMatrix[[CASRN]]){
                        return("1")
                    }
                    else{
                        return("0")
                    } 
                })
                return(paste0(CASRN, "\t", paste0(matrixOutputLine, collapse="\t")))
            })
            # Write to matrix file
            writeLines(paste0(matrixOutput, paste0(matrixOutputFull, collapse="\n")), MATRIX)
            close(MATRIX)
            return(TRUE)
        }, error=function(cond){
            print("Error creating annotation matrix file.")
            print(cond)
            return(NULL)
        })
        return(tmp_MatrixOut)
    })
    
    # Read status file and update step
    # Connect to DB to get status info
    poolStatus <- connQueue()
    # Set step flag for each set name
    query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=5 WHERE uuid='", enrichmentUUID, "' AND step<>-1;"), id="fetchStatus")
    outp <- dbExecute(poolStatus, query)
    # Check if no errors
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
    requestCancel <- dbGetQuery(poolStatus, query)
    # Close pool
    # poolClose(poolStatus)
    dbDisconnect(poolStatus)
    if(nrow(requestCancel) > 0){
        print(paste0("Canceling request: ", enrichmentUUID))
        return("request canceled")
    }
    
    annotationMatrix <- annotationMatrix[!vapply(annotationMatrix, is.null, FUN.VALUE=logical(1))]
    if(length(annotationMatrix) < 1){
        # Return error message
        return("no lines available in any input file: cannot fetch annotations")
    }
    
    # zip result files
    if(dir.exists(inDir) & dir.exists(outDir)){
        system2("cp", paste0("-r ", inDir, "/. ", outDir))
        inFilesToDelete <- Sys.glob(paste0(inDir, "/*"))
        inFilesToDelete <- unlist(lapply(inFilesToDelete, function(inFileToDelete) {
            tmp <- unlist(str_split(inFileToDelete, "/"))
            return(paste0(outDir, "/", tmp[length(tmp)]))
        }))
        system2("cd", paste0(outDir, "/ ; zip -r9X ./tox21enricher_", enrichmentUUID, ".zip ./*"))
        system2("rm", paste0(inFilesToDelete, collapse=" "))
    } else { # Return with error if did not complete. Do not update in database
        return("problem creating result files for request") 
    }
    
    # Connect to db
    poolUpdate <- connQueue()
    # Get ending time
    finishTime <- Sys.time()
    
    # Set finish time in transaction table
    query <- sqlInterpolate(ANSI(), paste0("UPDATE transaction SET timestamp_finished='", finishTime, "' WHERE uuid='", enrichmentUUID, "';"), id="transactionUpdateFinished")
    outp <- dbExecute(poolUpdate, query)
    
    # Close pool
    # poolClose(poolUpdate)
    dbDisconnect(poolUpdate)
    return("success")
}

# Re-run enrichment request in queue
queueResubmit <- function(res, req, mode="", enrichmentUUID="-1", annoSelectStr=fullAnnoClassStr, nodeCutoff=10, setNames){
    # Clean up queue
    if(DELETE_TIME != -1){
        future({
            queueCleanup()
        }, seed=TRUE)
    }
    
    future({
        # Connect to DB to get status info
        poolStatus <- connQueue()
        # Set lock for current request so it won't be reprocessed
        query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET lock=1 WHERE uuid='", enrichmentUUID, "';"), id="setLock")
        outp <- dbExecute(poolStatus, query)
        # Get status entry for corresponding queue entry
        query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM status WHERE uuid='", enrichmentUUID, "';"), id="fetchStatus")
        outp <- dbGetQuery(poolStatus, query)
        statusFiles <- outp
        # Read status entry(ies) and change flag to signify enrichment has started
        if(nrow(statusFiles) > 0){
            lapply(seq_len(nrow(statusFiles)), function(i){
                tmpSetName <- statusFiles[i, "setname"]
                query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=1 WHERE uuid='", enrichmentUUID, "' AND setname='", tmpSetName, "' AND step<>-1;"), id="fetchStatus")
                outp <- dbExecute(poolStatus, query)
            })
        }
        # Check if no errors
        query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
        requestCancel <- dbGetQuery(poolStatus, query)
        # Close pool
        # poolClose(poolStatus)
        dbDisconnect(poolStatus)
        if(nrow(requestCancel) > 0){
            return("request canceled")
        }
        # Perform enrichment analysis or fetch relevant annotations
        status_code <- 500
        tryCatch({
            if(mode == "annotation") {
                status_code <- getAnnotationsFunc(enrichmentUUID=enrichmentUUID, annoSelectStr=annoSelectStr, nodeCutoff=nodeCutoff)
            } else { # else query R API server 
                status_code <- performEnrichment(enrichmentUUID=enrichmentUUID, annoSelectStr=annoSelectStr, nodeCutoff=nodeCutoff)
            } 
        }, error=function(cond){
            status_code <- "unknown error"
        })
        
        # Connect to DB to set appropriate finishing flags
        poolFinished <- connQueue()
        # Check if request was canceled
        query <- sqlInterpolate(ANSI(), paste0("SELECT cancel FROM queue WHERE uuid='", enrichmentUUID, "';"), id="checkCancel")
        outp <- dbGetQuery(poolFinished, query)
        cancelValue <- outp[1, "cancel"]
        if(cancelValue == 1){
            status_code <- -1
        }
        # Upon success
        if (status_code == "success"){
            # Set flag for queue
            query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET finished=1 WHERE uuid='", enrichmentUUID, "';"), id="deleteEntries")
            outp <- dbExecute(poolFinished, query)
        } else if (status_code == -1) {
            print(paste0("Request canceled for ", enrichmentUUID, ". Deleting..."))
            # Set flag for queue
            query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET finished=1 WHERE uuid='", enrichmentUUID, "';"), id="deleteEntries")
            outp <- dbExecute(poolFinished, query)
        } else { # Else, generate error
            print(paste0("Error performing enrichment: ", status_code, " : "))
            # Set error message in queue table
            query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET error='Error performing enrichment. Please try again.' WHERE uuid='", enrichmentUUID, "';"), id="addErrorMsg")
            outp <- dbExecute(poolFinished, query)
            # Set flag for queue
            query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET finished=1 WHERE uuid='", enrichmentUUID, "';"), id="deleteEntries")
            outp <- dbExecute(poolFinished, query)
        }
        # Release lock
        query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET lock=0 WHERE uuid='", enrichmentUUID, "';"), id="setLock")
        outp <- dbExecute(poolFinished, query)
        # Close pool
        # poolClose(poolFinished)
        dbDisconnect(poolFinished)
    }, seed=TRUE)
    return(TRUE)
}

# Launch enrichment requests for each unfinished transaction here:
mclapply(unlockedTransactions, mc.cores=CORES, mc.silent=FALSE, function(x){
    poolRequests <- connQueue()
    query <- sqlInterpolate(ANSI(), paste0("SELECT original_mode, uuid, annotation_selection_string, cutoff, original_names FROM transaction WHERE uuid='", x, "' AND delete=0 AND cancel=0;"), id="resubmit")
    outp <- dbGetQuery(poolRequests, query)
    # poolClose(poolRequests)
    dbDisconnect(poolRequests)
    if(nrow(outp) > 0){
        x_mode <- outp[1, "original_mode"]
        x_uuid <- outp[1, "uuid"]
        x_anno <- outp[1, "annotation_selection_string"]
        x_node <- outp[1, "cutoff"]
        x_sets <- unlist(str_split(outp[1, "original_names"], "\\|"))
        x_sets <- paste0(unlist(lapply(x_sets, function(setName) unlist(str_split(setName, "__"))[2])), collapse="\n")
        queueResubmit(mode=x_mode, enrichmentUUID=x_uuid, annoSelectStr=x_anno, nodeCutoff=x_node, setNames=x_sets)
    }
})

print("! Ready to accept connections.")

#* @apiTag "Queue" Endpoints that deal with populating, reading, and working with the API's asynchronous request queue system. (INTERNAL USE ONLY)
#* @apiTag "Client-API Connectivity" Endpoints that deal with initializing and establishing connections between the Shiny app and the Plumber API. (INTERNAL USE ONLY)
#* @apiTag "Chemical Input" Endpoints that deal with properly formatting and validating the chemical input strings from requests. (INTERNAL USE ONLY)
#* @apiTag "File Generation and Handling" Endpoints that deal with creating, serving, and compressing files generated by the API. (INTERNAL USE ONLY)
#* @apiTag "Results Handling" Endpoints that deal with serving request result files back to clients. (INTERNAL USE ONLY)
#* @apiTag "Client Functionality" Endpoints that allow a user to submit requests by directly querying the API without having to use the Shiny application.

## SECTION 1: DEALING WITH THE QUEUE

#* Place enrichment request in queue (internal use only).
#* @tag "Queue"
#* @param mode Mode of request:<br><ul><li><b>casrn</b> - enrich from user-provided list of CASRNs</li><li><b>substructure</b> - shared substructures search</li><li><b>similarity</b> - Tanimoto similarity search</li><li><b>annotation</b> - view list of annotations associated with input chemicals</li></ul>
#* @param enrichmentUUID Unique ID assigned to request. Automatically generated by Tox21Enricher.
#* @param annoSelectStr String of all annotation classes to be used in request.
#* @param nodeCutoff Cutoff value that determines the top N annotations represented from each class in the enrichment results.
#* @param setNames Names used internally by Tox21Enricher for enrichment.
#* @post /queue
queue <- function(res, req, mode="", enrichmentUUID="-1", annoSelectStr=fullAnnoClassStr, nodeCutoff=10, setNames){
    # Validate input
    if(mode != "casrn" & mode != "substructure" & mode != "similarity" & mode != "annotation"){
        res$status <- 400
        return("Error: Invalid value for argument 'mode'.")
    }
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", enrichmentUUID)){
        res$status <- 400
        return("Error: Incorrect format for argument 'enrichmentUUID'.")
    }
    if(!grepl("([A-Za-z0-9]+=checked,)+", annoSelectStr)){
        res$status <- 400
        return("Error: Incorrect format for argument 'annoSelectStr'.")
    } else {
        annotationList <- getAnnotationListInternal()
        # Check if all annotations are valid
        cleanedAnnoList <- unlist(str_split(annoSelectStr, "=checked,"))
        cleanedAnnoList <- cleanedAnnoList[nchar(cleanedAnnoList) > 0]
        errorAnnotationClasses <- lapply(cleanedAnnoList, function(x){
            if(!(x %in% annotationList)){
                return(x)
            }
            return(NULL)
        })
        errorAnnotationClasses <- errorAnnotationClasses[!vapply(errorAnnotationClasses, is.null, FUN.VALUE=logical(1))]
        if(length(errorAnnotationClasses) > 0){
            res$status <- 400
            return(paste0("Error: Invalid annotation class(es): ", paste0(errorAnnotationClasses, collapse=", "), ". Use '", API_PROTOCOL, API_ADDR, "/getAnnotationList' to see a list of valid annotation classes."))
        }
    }
    if(is.na(as.double(nodeCutoff))){
        res$status <- 400
        return("Error: Incorrect value for argument 'nodeCutoff'. Must be between 1 and 50 (inclusive).")
    } else if(as.double(nodeCutoff) < 1 | as.double(nodeCutoff) > 50){
        res$status <- 400
        return("Error: Incorrect value for argument 'nodeCutoff'. Must be between 1 and 50 (inclusive).")
    }
    if(!grepl("[A-Za-z0-9]+((\\n)+[A-Za-z0-9]+)*(\\n)*", setNames)){
        res$status <- 400
        return("Error: Incorrect format for argument 'setNames'.")
    }
    
    # Connect to db
    poolQueue <- connQueue()
    # Update database with "queue" entry
    query <- sqlInterpolate(ANSI(), paste0("INSERT INTO queue(mode, uuid, annoselectstr, cutoff) VALUES('", mode, "', '", enrichmentUUID, "', '", annoSelectStr, "', ", nodeCutoff, ");"), id="createQueueEntry")
    outp <- dbExecute(poolQueue, query)
    # Update database with corresponding status entries
    setNamesSplit <- unlist(str_split(setNames, "\n"))
    if(length(setNamesSplit) > 0){
        lapply(setNamesSplit, function(x){
            query <- sqlInterpolate(ANSI(), paste0("INSERT INTO status(step, uuid, setname) VALUES(0, '", enrichmentUUID, "', '", x, "') ;"), id="createStatusEntry")
            outp <- dbExecute(poolQueue, query) 
        })
    }
    # Close pool
    # poolClose(poolQueue)
    dbDisconnect(poolQueue)
    
    # Clean up queue
    if(DELETE_TIME != -1){
        future({
            queueCleanup()
        }, seed=TRUE)
    }

    # future({
        # Connect to DB to get status info
        poolStatus <- connQueue()
        # Set lock for current request so it won't be reprocessed
        query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET lock=1 WHERE uuid='", enrichmentUUID, "';"), id="setLock")
        outp <- dbExecute(poolStatus, query)
        # Get status entry for corresponding queue entry
        query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM status WHERE uuid='", enrichmentUUID, "';"), id="fetchStatus")
        outp <- dbGetQuery(poolStatus, query)
        statusFiles <- outp
        # Read status entry(ies) and change flag to signify enrichment has started
        if(nrow(statusFiles) > 0){
            lapply(seq_len(nrow(statusFiles)), function(i){
                tmpSetName <- statusFiles[i, "setname"]
                query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=1 WHERE uuid='", enrichmentUUID, "' AND setname='", tmpSetName, "' AND step<>-1;"), id="fetchStatus")
                outp <- dbExecute(poolStatus, query)
            })
        }
        # Check if no errors
        query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
        requestCancel <- dbGetQuery(poolStatus, query)
        # Close pool
        # poolClose(poolStatus)
        dbDisconnect(poolStatus)
        if(nrow(requestCancel) > 0){
            return("request canceled")
        }
        
        # Perform enrichment analysis or fetch relevant annotations
        status_code <- "unknown error"
        tryCatch({
            if(mode == "annotation") {
                status_code <- getAnnotationsFunc(enrichmentUUID=enrichmentUUID, annoSelectStr=annoSelectStr, nodeCutoff=nodeCutoff)
            } else { # else query R API server 
                status_code <- performEnrichment(enrichmentUUID=enrichmentUUID, annoSelectStr=annoSelectStr, nodeCutoff=nodeCutoff)
            } 
        }, error=function(cond){
            print(cond)
            status_code <- "unknown error"
        })
        
        # Connect to DB to set appropriate finishing flags
        poolFinished <- connQueue()
        
        # Check if request was canceled
        query <- sqlInterpolate(ANSI(), paste0("SELECT cancel FROM queue WHERE uuid='", enrichmentUUID, "';"), id="checkCancel")
        outp <- dbGetQuery(poolFinished, query)
        
        cancelValue <- outp[1, "cancel"]
        if(cancelValue == 1){
            status_code <- "request canceled"
        }
        
        # Upon success
        if (status_code == "success"){
            # Set flag for queue
            query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET finished=1 WHERE uuid='", enrichmentUUID, "';"), id="deleteEntries")
            outp <- dbExecute(poolFinished, query)
        } else if (status_code == -1) {
            print(paste0("Request canceled for ", enrichmentUUID, ". Deleting..."))
            # Set flag for queue
            query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET finished=1 WHERE uuid='", enrichmentUUID, "';"), id="deleteEntries")
            outp <- dbExecute(poolFinished, query)
        } else { # Else, generate error file for reference
            print(paste0("Error performing enrichment on ", enrichmentUUID))
            # Set error message in queue table
            query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET error='Error performing enrichment: ", status_code, ". Please check your input settings and try again.' WHERE uuid='", enrichmentUUID, "';"), id="addErrorMsg")
            outp <- dbExecute(poolFinished, query)
            # Set flag for queue
            query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET finished=1 WHERE uuid='", enrichmentUUID, "';"), id="deleteEntries")
            outp <- dbExecute(poolFinished, query)
        }
        
        # Release lock
        query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET lock=0 WHERE uuid='", enrichmentUUID, "';"), id="setLock")
        outp <- dbExecute(poolFinished, query)
        dbDisconnect(poolFinished)
    # }, seed=TRUE)
    return(TRUE)
}

#* Create entry for request in transaction table (internal use only).
#* @tag "Queue"
#* @param originalMode Special variable for determining if request has been re-enriched.
#* @param mode Mode of request:<br><ul><li><b>casrn</b> - enrich from user-provided list of CASRNs</li><li><b>substructure</b> - shared substructures search</li><li><b>similarity</b> - Tanimoto similarity search</li><li><b>annotation</b> - view list of annotations associated with input chemicals</li></ul>
#* @param uuid Unique ID assigned to request. Automatically generated by Tox21Enricher.
#* @param annoSelectStr String of all annotation classes to be used in request.
#* @param cutoff Cutoff value that determines the top N annotations represented from each class in the enrichment results.
#* @param input Special variable related to input for enrichment.
#* @param casrnBox Special variable related to input for enrichment.
#* @param originalNames Special variable for determining if request has been re-enriched.
#* @param reenrich Special variable for determining if request has been re-enriched.
#* @param color Special variable related to input for enrichment.
#* @param timestampPosted Special variable related to input for enrichment.
#* @param reenrichFlag Special variable for determining if request has been re-enriched.
#* @post /createTransaction
createTransaction <- function(res, req, originalMode="", mode="", uuid="-1", annoSelectStr=fullAnnoClassStr, cutoff=10, input="", casrnBox="", originalNames="none", reenrich="", color="", timestampPosted="", reenrichFlag=FALSE, pvalueType="nominal"){
    # Validate input
    if(originalMode != "similarity" & originalMode != "substructure" & originalMode != "casrn" & originalMode != "annotation"){
        res$status <- 400
        return("Error: Invalid value for argument 'originalMode'.")
    }
    if(mode != "similarity" & mode != "substructure" & mode != "casrn" & mode != "annotation"){
        res$status <- 400
        return("Error: Invalid value for argument 'mode'.")
    }
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", uuid)){
        res$status <- 400
        return("Error: Incorrect format for argument 'uuid'.")
    }

    # Get list of all annotation classes in database
    annotationList <- getAnnotationListInternal()
    # Set annotations to default if missing
    if(annoSelectStr == ""){
        annoSelectStr <- paste0(paste0(annotationList, collapse="=checked,"), "=checked,")
    }
    if(!grepl("([A-Za-z0-9]+=checked,)+", annoSelectStr)){
        res$status <- 400
        return("Error: Incorrect format for argument 'annoSelectStr'.")
    } else {
        # Check if all annotations are valid
        cleanedAnnoList <- unlist(str_split(annoSelectStr, "=checked,"))
        cleanedAnnoList <- cleanedAnnoList[nchar(cleanedAnnoList) > 0]
        errorAnnotationClasses <- lapply(cleanedAnnoList, function(x){
            if(!(x %in% annotationList)){
                return(x)
            }
            return(NULL)
        })
        errorAnnotationClasses <- errorAnnotationClasses[!vapply(errorAnnotationClasses, is.null, FUN.VALUE=logical(1))]
        if(length(errorAnnotationClasses) > 0){
            res$status <- 400
            return(paste0("Error: Invalid annotation class(es): ", paste0(errorAnnotationClasses, collapse=", "), ". Use '", API_PROTOCOL, API_ADDR, "/getAnnotationList' to see a list of valid annotation classes."))
        }
    }
    # Check if we can coerce cutoff to number
    if(typeof(cutoff) == "character"){
        cutoff <- as.numeric(cutoff)
    }
    if(is.na(cutoff)){
        res$status <- 400
        return("Error: Cutoff value is not a number.")
    }
    # Check if cutoff is not a number
    if(!is.numeric(cutoff)){
        res$status <- 400
        return("Error: Cutoff value is not a number.")
    }
    # Check if cutoff is not an integer
    if(cutoff %% 1 != 0){
        res$status <- 400
        return("Error: Cutoff value is not an integer.")
    }
    # Check if cutoff is out of range
    if(cutoff < 1 | cutoff > 50){
        res$status <- 400
        return("Error: Incorrect value for argument 'cutoff'. Must be between 1 and 50 (inclusive).")
    }
    # Check if user inputs any bad chars
    if(grepl("'|\"|;|--", casrnBox)){
        res$status <- 400
        return("Error: Invalid value for argument 'casrnBox'.")
    }
    if(grepl("'|\"|;|--", originalNames)){
        res$status <- 400
        return("Error: Invalid value for argument 'originalNames'.")
    }
    if(grepl("'|\"|;|--", reenrich)){ 
        res$status <- 400
        return("Error: Invalid value for argument 'reenrich'.")
    }
    inputSets <- unlist(str_split(input, "\\|"))
    for(i in inputSets){
        if(!grepl("([0-9]+-[0-9]+-[0-9]+)|(NOCAS_[0-9]+)__[A-Za-z0-9]+", i)){
            res$status <- 400
            return("Error: Invalid value for argument 'input'.")
        }
    }
    inputColors <- unlist(str_split(color, "\\|"))
    for(i in inputColors){
        if(!grepl("[A-Za-z0-9]+__rgb\\([0-9]+, [0-9]+, [0-9]+\\)", i)){
            res$status <- 400
            return("Error: Invalid value for argument 'color'.")
        }
    }
    if(!grepl("2[0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]", timestampPosted)){
        res$status <- 400
        return("Error: Invalid value for argument 'timestampPosted'.")
    }
    if(typeof(reenrichFlag) == "character"){
        reenrichFlag <- tolower(reenrichFlag)
    }
    if(reenrichFlag != TRUE & reenrichFlag != FALSE & reenrichFlag != "true" & reenrichFlag != "false"){
        res$status <- 400
        return("Error: Incorrect value for argument 'reenrichFlag'.")
    } else {
        # format reenrich flag
        if(reenrichFlag == TRUE){
            reenrichFlag <- 1
        } else {
            reenrichFlag <- 0
        }
    }

    # Connect to db
    poolTransaction <- connQueue()
    # Update database with transaction entry
    query <- sqlInterpolate(ANSI(), paste0("INSERT INTO transaction(original_mode, mode, uuid, annotation_selection_string, cutoff, input, casrn_box, original_names, reenrich, reenrich_flag, colors, timestamp_posted, pvalue) VALUES('", originalMode, "', '", mode, "', '", uuid, "', '", annoSelectStr, "', '", cutoff, "', '", input, "', '", casrnBox, "', '", originalNames, "', '", reenrich, "', '", reenrichFlag, "', '", color, "', '", timestampPosted, "', '", pvalueType, "');"), id="createTransactionEntry")
    outp <- dbExecute(poolTransaction, query)
    
    # Close pool
    dbDisconnect(poolTransaction)
    return(TRUE)
}

#* Load details for the selected transaction (internal use only).
#* @tag "Queue"
#* @param uuid Unique ID assigned to request. Automatically generated by Tox21Enricher.
#* @get /getTransactionDetails
getTransactionDetails <- function(res, req, uuid="none"){
    # Validate input
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", uuid)){
        res$status <- 400
        return("Error: Incorrect format for argument 'uuid'.")
    }
    # Connect to db
    poolTransaction <- connQueue()
    # Update database with transaction entry
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM transaction WHERE uuid='", uuid, "';"), id="loadTransaction")
    outp <- dbGetQuery(poolTransaction, query)
    # Close pool
    # poolClose(poolTransaction)
    dbDisconnect(poolTransaction)
    return(outp)
}

#* Get queue position for given enrichment request (internal use only).
#* @tag "Queue"
#* @param transactionId Unique ID assigned to request. Automatically generated by Tox21Enricher.
#* @param mode Mode of request:<br><ul><li><b>casrn</b> - enrich from user-provided list of CASRNs</li><li><b>substructure</b> - shared substructures search</li><li><b>similarity</b> - Tanimoto similarity search</li><li><b>annotation</b> - view list of annotations associated with input chemicals</li></ul>
#* @get /getQueuePos
getQueuePos <- function(res, req, transactionId="-1", mode="init"){
    # Validate input
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", transactionId)){
        res$status <- 400
        return("Error: Incorrect format for argument 'transactionId'.")
    }
    if(mode != "init" & mode != "casrn" & mode != "substructure" & mode != "similarity" & mode != "annotation"){
        res$status <- 400
        return("Error: Invalid value for argument 'mode'.")
    }
    
    # Connect to db
    poolUpdate <- connQueue()
    # Get status entries for request
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM status WHERE uuid='", transactionId, "';"), id="fetchStatusStep")
    outp <- dbGetQuery(poolUpdate, query)
    statusFiles <- outp[, "step"]
    names(statusFiles) <- outp[, "setname"]
    # Check if request has completed w/ errors overall (failed completely)
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE error IS NOT NULL AND uuid='", transactionId, "';"), id="fetchQueuePosition")
    outp <- dbGetQuery(poolUpdate, query)
    if(nrow(outp) > 0) {
        return(paste0("<div class=\"text-danger\">Failed: ", outp$error, "</div>"))
    }
    # Check if request has completed
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE finished=0 AND uuid='", transactionId, "';"), id="fetchQueuePosition")
    outp <- dbGetQuery(poolUpdate, query)
    if(nrow(outp) < 1) {
        return("Complete!")
    }
    # Get queue position of request
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE finished=0 AND error IS NULL AND cancel=0;"), id="fetchQueuePosition")
    outp <- dbGetQuery(poolUpdate, query)
    # Close pool
    # poolClose(poolUpdate)
    dbDisconnect(poolUpdate)
    
    if(nrow(outp) > 0){
        # Sort by index number
        outp <- outp[order(outp$index), ]
        queuePos <- lapply (seq_len(nrow(outp)), function(i) {
            if(outp[i, "uuid"] == transactionId){
                return(i)
            }
            return(NULL)
        })
        queuePos <- queuePos[!vapply(queuePos, is.null, FUN.VALUE=logical(1))][1]
        # Determine step for each set:
        statusList <- NULL
        if(mode != "annotation"){ # if enrichment mode
            statusList <- lapply(statusFiles, function(statusFileUpdate){
                # waiting in queue
                if(statusFileUpdate < 1){
                    if(statusFileUpdate == -1){#error
                        return("Ran into error.")
                    }
                    return(paste0("Waiting in queue position: ", queuePos, "."))
                } else if(statusFileUpdate == 1){
                    return("(Step 1/4): Processing input file(s).")
                } else if(statusFileUpdate == 2){
                    return("(Step 2/4): Creating chart and matrix files.")
                } else if(statusFileUpdate == 3){
                    return("(Step 3/4): Clustering (Step 1/4) - calculating kappa score.")
                } else if(statusFileUpdate == 4){
                    return("(Step 3/4): Clustering (Step 2/4) - creating qualified initial seeding groups.")
                } else if(statusFileUpdate == 5){
                    return("(Step 3/4): Clustering (Step 3/4) - merging qualified seeds.")
                } else if(statusFileUpdate == 6){
                    return("(Step 3/4): Clustering (Step 4/4) - calculating enrichment score.")
                } else if(statusFileUpdate == 7){
                    return("(Step 4/4): Creating .gct files.")
                }
                return("Loading...")
            })
            names(statusList) <- names(statusFiles)
        } else { # if fetch annotations mode
            statusList <- lapply(statusFiles, function(statusFileUpdate){
                # waiting in queue
                if(statusFileUpdate < 1){
                    if(statusFileUpdate == -1){#error
                        return("Ran into error.")
                    }
                    return(paste0("Waiting in queue position: ", queuePos, "."))
                } else if(statusFileUpdate == 1){
                    return("(Step 1/4): Processing input file(s).")
                } else if(statusFileUpdate == 2){
                    return("(Step 2/4): Fetching annotations.")
                } else if(statusFileUpdate == 3){
                    return("(Step 3/4): Creating annotations list file(s).")
                } else if(statusFileUpdate == 4){
                    return("(Step 4/4): Creating annotation matrix file.")
                }
                return("Loading...")
            })
            names(statusList) <- names(statusFiles)
        }
        if(length(statusList) > 0){
            statusListToReturn <- lapply(seq_len(length(statusList)), function(i) paste0(names(statusList)[i], ": \t", statusList[i]))
            statusListToReturn <- paste0(statusListToReturn, collapse="\n")
            return(statusListToReturn) 
        } else {
            return("Complete!")
        }
    }
    return("Complete!")
}

#* Get transaction data for given enrichment request (internal use only).
#* @tag "Queue"
#* @param transactionId Unique ID assigned to request. Automatically generated by Tox21Enricher.
#* @get /getPrevSessionData
getPrevSessionData <- function(res, req, transactionId="-1"){
    # Validate input
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", transactionId)){
        res$status <- 400
        return("Error: Incorrect format for argument 'transactionId'.")
    }
    
    # Connect to db
    poolSessionData <- connQueue()

    # Get transaction data of request
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM transaction WHERE uuid='", transactionId, "';"), id="fetchTransactionData")
    outp <- dbGetQuery(poolSessionData, query)
    # Close pool
    # poolClose(poolSessionData)
    dbDisconnect(poolSessionData)
    if(nrow(outp) > 0){
        return(list(
            "original_mode"=outp[1, "original_mode"],
            "mode"=outp[1, "mode"],
            "annotation_selection_string"=outp[1, "annotation_selection_string"],
            "cutoff"=outp[1, "cutoff"],
            "input"=outp[1, "input"],
            "casrn_box"=outp[1, "casrn_box"],
            "original_names"=outp[1, "original_names"],
            "reenrich"=outp[1, "reenrich"],
            "reenrich_flag"=outp[1, "reenrich_flag"],
            "colors"=outp[1, "colors"]
        ))
    }
    return(list())
}

#* Check if enrichment process has terminated for given request (internal use only).
#* @tag "Queue"
#* @param transactionId Unique ID assigned to request. Automatically generated by Tox21Enricher.
#* @get /isRequestFinished
isRequestFinished <- function(res, req, transactionId="-1"){
    # Validate input
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", transactionId)){
        res$status <- 400
        return("Error: Incorrect format for argument 'transactionId'.")
    }
    
    # Connect to db
    poolQueue <- connQueue()
    # Get status entries for request
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", transactionId, "';"), id="fetchStatusStep")
    outp <- dbGetQuery(poolQueue, query)
    # Close pool
    # poolClose(poolQueue)
    dbDisconnect(poolQueue)
    finished <- outp[1, "finished"]
    if(is.na(finished)){
        return(-1)
    }
    if(finished == 1){
        return(1)
    }
    return(0)
}

#* Check if error file exists for given request (internal use only).
#* @tag "Queue"
#* @param transactionId Unique ID assigned to request. Automatically generated by Tox21Enricher.
#* @get /hasError
hasError <- function(res, req, transactionId="-1"){
    # Validate input
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", transactionId)){
        res$status <- 400
        return("Error: Incorrect format for argument 'transactionId'.")
    }
    
    # Connect to db
    poolQueue <- connQueue()
    # Get status entries for request
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", transactionId, "' AND error IS NOT NULL;"), id="fetchError")
    outp <- dbGetQuery(poolQueue, query)
    # Close pool
    # poolClose(poolQueue)
    dbDisconnect(poolQueue)
    if(nrow(outp) > 0){
        return(outp[1, "error"])
    }
    return(FALSE)
}

#* Cancel enrichment process for given UUID (internal use only).
#* @tag "Queue"
#* @param transactionId Unique ID assigned to request. Automatically generated by Tox21Enricher.
#* @get /cancelEnrichment
cancelEnrichment <- function(res, req, transactionId="-1"){
    # Validate input
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", transactionId)){
        res$status <- 400
        return("Error: Incorrect format for argument 'transactionId'.")
    }
    
    # Connect to db
    poolCancel <- connQueue()
    # Update database to show that request was canceled
    query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET cancel=1, finished=1 WHERE uuid='", transactionId, "';"), id="cancelEnrichment")
    outp <- dbExecute(poolCancel, query)
    query <- sqlInterpolate(ANSI(), paste0("UPDATE transaction SET cancel=1 WHERE uuid='", transactionId, "';"), id="cancelEnrichment")
    outp <- dbExecute(poolCancel, query)
    # Close pool
    # poolClose(poolCancel)
    dbDisconnect(poolCancel)
    return(TRUE)
}

#* Check if a given request has been canceled for given UUID (internal use only).
#* @tag "Queue"
#* @param transactionId Unique ID assigned to request. Automatically generated by Tox21Enricher.
#* @get /isCancel
isCancel <- function(res, req, transactionId="-1"){
    # Validate input
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", transactionId)){
        res$status <- 400
        return("Error: Incorrect format for argument 'transactionId'.")
    }
    
    # Connect to db
    poolCancel <- connQueue()
    # Update database to show that request was canceled
    query <- sqlInterpolate(ANSI(), paste0("SELECT cancel FROM transaction WHERE uuid='", transactionId, "';"), id="isCancel")
    outp <- dbGetQuery(poolCancel, query)
    # Close pool
    # poolClose(poolCancel)
    dbDisconnect(poolCancel)
    cancelStatus <- 1
    if(nrow(outp) == 1){
        cancelStatus <- outp[1, "cancel"]
    }
    return(cancelStatus)
}

## SECTION 2: REQUIRED BY CLIENT APPLICATION INITIALIZATION

#* Ping API
#* @tag "Client-API Connectivity"
#* @get /ping
ping <- function(res, req){
    return(TRUE)
}

#* Get delete time for old transaction (internal use only).
#* @tag "Client-API Connectivity"
#* @get /getDeleteTime
getDeleteTime <- function(res, req){
    return(DELETE_TIME)
}

#* Calculate cookie expiry date for previous transaction and fetch additional request information (internal use only).
#* @tag "Client-API Connectivity"
#* @param transactionId Unique ID assigned to request. Automatically generated by Tox21Enricher.
#* @get /getAdditionalRequestInfo
getAdditionalRequestInfo <- function(res, req, transactionId="-1"){
    # Validate input
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", transactionId)){
        res$status <- 400
        return("Error: Incorrect format for argument 'transactionId'.")
    }
    
    # Connect to db
    poolExp <- connQueue()
    expQuery <- sqlInterpolate(ANSI(), paste0("SELECT original_mode, mode, cutoff, casrn_box, timestamp_posted, timestamp_started, timestamp_finished FROM transaction WHERE uuid='", transactionId, "';"), id="getAnnotationClasses")
    expOutp <- dbGetQuery(poolExp, expQuery)
    # Close pool
    # poolClose(poolExp)
    dbDisconnect(poolExp)
    # Calculate cookie expiry date and add to DF
    expDate <- as.POSIXct(expOutp[, "timestamp_posted"], format="%Y-%m-%d %H:%M:%S", tz="UTC") + (60 * 60 * CLEANUP_TIME)
    fullRequestInfo <- expOutp
    fullRequestInfo["expiry_date"] <- expDate
    return(fullRequestInfo)
}

#* Get cleanup time for old transaction. This number is the number of hours that a transaction will live for on the server. This is used when saving cookie expiry dates on the client as well as the cutoff time to delete in the queue cleanup script. (internal use only).
#* @tag "Client-API Connectivity"
#* @get /getCleanupTime
getCleanupTime <- function(res, req){
    return(CLEANUP_TIME)
}

#* Get the maximum number of simultaneous input sets that may be submitted in a request (internal use only).
#* @tag "Client-API Connectivity"
#* @get /getInputMax
getInputMax <- function(res, req){
    return(INPUT_MAX)
}

#* Get list of annotation classes/types (internal use only).
#* @tag "Client-API Connectivity"
#* @get /getAnnotations
getAnnotations <- function(res, req){
    # Connect to db
    poolAnnotations <- conn()
    annoClassQuery <- sqlInterpolate(ANSI(), "SELECT annoclassname, annotype, annodesc, numberoftermids FROM annotation_class;", id="getAnnotationClasses")
    annoClassOutp <- dbGetQuery(poolAnnotations, annoClassQuery)
    rownames(annoClassOutp) <- seq_len(nrow(annoClassOutp))
    # Close pool
    # poolClose(poolAnnotations)
    dbDisconnect(poolAnnotations)
    return(annoClassOutp)
}

#* Get total number of requests (internal use only).
#* @tag "Client-API Connectivity"
#* @get /getTotalRequests
getTotalRequests <- function(res, req){
    # Connect to db
    poolTotal <- connQueue()
    totalQuery <- sqlInterpolate(ANSI(), "SELECT uuid, timestamp_started, timestamp_finished FROM transaction WHERE cancel=0;", id="getTotalEnrichment")
    totalOutp <- dbGetQuery(poolTotal, totalQuery)
    # Close pool
    # poolClose(poolTotal)
    dbDisconnect(poolTotal)
    
    # Extract current year
    currentDate <- unlist(str_split(Sys.time(), " "))[1]
    # yyyy-mm-dd format
    currentYear <- unlist(str_split(currentDate, "-"))[1]
    # Get all finished requests for the month
    finishedRequests <- totalOutp[, "timestamp_finished"]
    finishedRequests <- unlist(lapply(finishedRequests, function(x){
        if(!is.na(x)){
            xDate <- unlist(str_split(x, " "))[1]
            xYear <- unlist(str_split(xDate, "-"))[1]
            if(!is.na(xYear) & xYear == currentYear){
                return(x)
            }
        }
        return(NULL)
    }))
    finishedRequests <- finishedRequests[!vapply(finishedRequests, is.null, FUN.VALUE=logical(1))]
    monthTotal <- length(finishedRequests)
    return(monthTotal)
}

## SECTION 3: PREPARING CHEMICAL DATA FOR ENRICHMENT

#* Get data for provided SMILES string (substructure) (internal use only).
#* @tag "Chemical Input"
#* @param input Input string, either SMILES or CASRN (if re-enriching).
#* @param reenrich Boolean value to let the API know if this is a re-enrichment or not.
#* @get /searchBySubstructure
searchBySubstructure <- function(res, req, input){
    # Validate input
    if(grepl("'|\"|;|--", input)){
        res$status <- 400
        return("Error: Invalid value for argument 'input'.")
    }
    # Connect to db
    poolSubstructure <- conn()
    # Sanitize input, convert InChI strings to SMILES
    if (grepl("InChI=", input, fixed=TRUE)) {
        input <- convertInchi(inchi=input)
    }
    substructureOutp <- NULL
    substructureQuery <- sqlInterpolate(ANSI(), paste0("SELECT * FROM mols_2 WHERE m @> CAST('", input, "' AS mol);"), id="substructureResults")
    trySubstructure <- tryCatch({
        substructureOutp <- dbGetQuery(poolSubstructure, substructureQuery)
        substructureOutp$m <- unlist(lapply(substructureOutp$m, function(x) paste0(x))) # coerce mol column to string. If left as "pq_mol" data type, R wont' know how to send this in a json back to the client.
        TRUE
    }, error=function(cond){
        FALSE
    })
    if(!trySubstructure){ # if error occurs when converting to mol
        return(list())
    }
    # Close pool
    # poolClose(poolSubstructure)
    dbDisconnect(poolSubstructure)
    return(substructureOutp)
}

#* Get data for provided SMILES string (similarity) (internal use only).
#* @tag "Chemical Input"
#* @param input SMILES input string.
#* @param threshold Tanimoto similarity threshold.
#* @get /searchBySimilarity
searchBySimilarity <- function(res, req, input="", threshold=0.50){
    # Validate input
    if(grepl("'|\"|;|--", input)){
        res$status <- 400
        return("Error: Invalid value for argument 'input'.")
    }
    if(is.na(as.numeric(threshold))){
        res$status <- 400
        return("Error: Invalid value for argument 'threshold'.")
    } else {
        if(as.numeric(threshold) < 0.02 | as.numeric(threshold) > 1.00){
            res$status <- 400
            return("Error: Invalid value for argument 'threshold'. Must be between 0.02 and 1.00 (inclusive).")
        }
    }
    
    # Connect to db
    poolSimilarity <- conn()
    # Sanitize input, convert InChI strings to SMILES
    if (grepl("InChI=", input, fixed=TRUE)) {
        input <- convertInchi(inchi=input)
    }
    # Set Tanimoto threshold for similarity cutoff
    queryTanimoto <- sqlInterpolate(ANSI(), paste0("set rdkit.tanimoto_threshold=", threshold, ";"), id="tanimotoResults")
    similarityQuery <- sqlInterpolate(ANSI(), paste0("SELECT * FROM get_mfp2_neighbors('", input, "');"), id="similarityResults")
    similarityOutp <- NULL
    trySimilarity <- tryCatch({
        outpTanimoto <- dbExecute(poolSimilarity, queryTanimoto)
        similarityOutp <- dbGetQuery(poolSimilarity, similarityQuery)
        similarityOutp$m <- unlist(lapply(similarityOutp$m, function(x) paste0(x))) # coerce mol column to string. If left as "pq_mol" data type, R won't know how to send this in a json back to the client.
        TRUE
    }, error=function(cond){
        FALSE
    })
    if(!trySimilarity){ # if error occurs when converting to mol
        return(list())
    }
    # Close pool
    # poolClose(poolSimilarity)
    dbDisconnect(poolSimilarity)
    return(similarityOutp)
}

#* Get additional data for provided CASRN (internal use only).
#* @tag "Chemical Input"
#* @param input CASRN input string.
#* @get /getCasrnData
getCasrnData <- function(res, req, input){
    if(!grepl("([0-9]+-[0-9]+-[0-9]+)|(NOCAS_[0-9]+)", input)){
        res$status <- 400
        return("Error: Incorrect format for argument 'input'.")
    }
    
    # Connect to db
    poolCasrn <- conn()
    casrnQuery <- sqlInterpolate(ANSI(), paste0("SELECT iupac_name, smiles, dtxsid, dtxrid, mol_formula, mol_weight, inchis, inchikey, cid, testsubstance_chemname FROM chemical_detail WHERE CASRN LIKE '", input, "';"), id="casrnResults")
    casrnOutp <- dbGetQuery(poolCasrn, casrnQuery)
    # Close pool
    # poolClose(poolCasrn)
    dbDisconnect(poolCasrn)
    return(casrnOutp)
}

#* Detect if submitted chemical contains any reactive groups (internal use only).
#* @tag "Chemical Input"
#* @param input SMILES input string.
#* @get /getReactiveGroups
getReactiveGroups <- function(res, req, input="-1"){
    # Validate input
    if(grepl("'|\"|;|--", input)){
        res$status <- 400
        return("Error: Invalid value for argument 'input'.")
    }
    has_nitrile <- 0
    has_isocyanate <- 0
    has_aldehyde <- 0
    has_epoxide <- 0
    # Connect to db
    poolReactive <- conn()
    # Convert InChI to SMILES if present in input
    if(grepl("^InChI=", input)){
        input <- inchiToSmiles(inchi=input)
    }
    # Nitrile
    reactiveQuery <- sqlInterpolate(ANSI(), paste0("select * from mol_from_smiles('", input, "') WHERE mol_from_smiles('", input, "') @> mol_from_smarts('[NX1]#[CX2]');"), id="getReactiveGroups")
    reactiveOutp <- dbGetQuery(poolReactive, reactiveQuery)
    if(nrow(reactiveOutp) > 0){
        has_nitrile <- 1
    }
    # Isocyanate
    reactiveQuery <- sqlInterpolate(ANSI(), paste0("select * from mol_from_smiles('", input, "') WHERE mol_from_smiles('", input, "') @> mol_from_smarts('[NX2]=[CX2]=[OX1]');"), id="getReactiveGroups")
    reactiveOutp <- dbGetQuery(poolReactive, reactiveQuery)
    if(nrow(reactiveOutp) > 0){
        has_isocyanate <- 1
    }
    # Aldehyde
    reactiveQuery <- sqlInterpolate(ANSI(), paste0("select * from mol_from_smiles('", input, "') WHERE mol_from_smiles('", input, "') @> mol_from_smarts('[CX3H1](=O)[#6]');"), id="getReactiveGroups")
    reactiveOutp <- dbGetQuery(poolReactive, reactiveQuery)
    if(nrow(reactiveOutp) > 0){
        has_aldehyde <- 1
    }
    # Epoxide
    reactiveQuery <- sqlInterpolate(ANSI(), paste0("select * from mol_from_smiles('", input, "') WHERE mol_from_smiles('", input, "') @> mol_from_smarts('[OX2]1[CX4][CX4]1');"), id="getReactiveGroups")
    reactiveOutp <- dbGetQuery(poolReactive, reactiveQuery)
    if(nrow(reactiveOutp) > 0){
        has_epoxide <- 1
    }
    # Put reactive groups into string to pass back to client
    reactiveGroups <- paste0(has_nitrile, ",", has_isocyanate, ",", has_aldehyde, ",", has_epoxide)
    # Close pool
    # poolClose(poolReactive)
    dbDisconnect(poolReactive)
    return(reactiveGroups)
}

#* Convert provided InChI string to SMILES (internal use only).
#* @tag "Chemical Input"
#* @param inchi InChI input string.
#* @get /inchiToSmiles
inchiToSmiles <- function(res, req, inchi){
    # Validate input
    if(grepl("'|\"|;|--", inchi)){
        res$status <- 400
        return("Error: Invalid value for argument 'inchi'.")
    }
    
    # Connect to db
    poolInchi <- conn()
    inchiQuery <- sqlInterpolate(ANSI(), paste0("SELECT smiles FROM chemical_detail WHERE inchis='", inchi, "';"), id="convertInchi")
    inchiOutp <- dbGetQuery(poolInchi, inchiQuery)
    # Close pool
    # poolClose(poolInchi)
    dbDisconnect(poolInchi)
    return(inchiOutp[[1]])
}

# Same as above, but done from within the internal Tox21Enricher substructure/similarity searches. we don't want to do this asynchronously because it is just a function and not an API endpoint.
convertInchi <- function(res, req, inchi){
    # Connect to db
    poolInchi <- conn()
    inchiQuery <- sqlInterpolate(ANSI(), paste0("SELECT smiles FROM chemical_detail WHERE inchis='", inchi, "';"), id="convertInchi")
    inchiOutp <- dbGetQuery(poolInchi, inchiQuery)
    # Close pool
    # poolClose(poolInchi)
    dbDisconnect(poolInchi)
    return(inchiOutp[[1]])
}

#* Use RDKit to generate chemical structure images from a list of molecules (internal use only).
#* @tag "Chemical Input"
#* @param input Newline-delimited list of SMILES.
#* @get /getStructureImages
getStructureImages <- function(res, req, input){
    # Validate input
    tmpSplit <- unlist(str_split(input, "\n"))
    for(i in tmpSplit){
        if(grepl("'|\"|;|--", i)){
            res$status <- 400
            return("Error: Invalid value for argument 'input'.")
        }
    }
    # Connect to db
    poolSvg <- conn()
    structures <- lapply(tmpSplit, function(x){
        tmpSplit2 <- unlist(str_split(x, "__"))
        m <- tmpSplit2[2]
        svgQuery <- sqlInterpolate(ANSI(), paste0("SELECT mol_to_svg(CAST('", m, "' AS mol));"), id="generateStructures")
        svgOutp <- dbGetQuery(poolSvg, svgQuery)
        return(svgOutp[1, "mol_to_svg"])
    })
    names(structures) <- lapply(tmpSplit, function(x){
        tmpSplit2 <- unlist(str_split(x, "__"))
        return(tmpSplit2[1])
    })
    # Close pool
    # poolClose(poolSvg)
    dbDisconnect(poolSvg)
    return(structures)
}

#* Get a list of CASRNs with reactive structure warnings from database (internal use only).
#* @tag "Chemical Input"
#* @param input Prepared input string generated by Tox21Enricher.
#* @get /getStructureWarnings
getStructureWarnings <- function(res, req, input=""){
    # Validate input
    inputSets <- unlist(str_split(input, "\\|"))
    for(i in inputSets){
        if(!grepl("([0-9]+-[0-9]+-[0-9]+)|(NOCAS_[0-9]+)__[A-Za-z0-9]+", i)){
            res$status <- 400
            return("Error: Invalid value for argument 'input'.")
        }
    }
    # Connect to db
    poolWarn <- conn()
    inputSets <- unique(unlist(lapply(inputSets, function(x) unlist(str_split(x, "__"))[1])))
    warnQuery <- sqlInterpolate(ANSI(), paste0("SELECT casrn, cyanide, isocyanate, aldehyde, epoxide FROM mols_2 WHERE ", paste0("casrn='", inputSets, "'", collapse=" OR "), ";"), id="getWarnings")
    warnOutp <- dbGetQuery(poolWarn, warnQuery)
    # Close pool
    # poolClose(poolWarn)
    dbDisconnect(poolWarn)
    return(warnOutp)
}

## SECTION 4: FILE CREATION AND SERVING

#* Serve text file to client (internal use only).
#* @tag "File Generation and Handling"
#* @serializer contentType list(type="application/text")
#* @param transactionId Unique ID assigned to request. Automatically generated by Tox21Enricher.
#* @param filename Name of file to download.
#* @param subDir Directory of file to download.
#* @get /serveFileText
serveFileText <- function(res, req, transactionId="-1", filename, subDir){
    # Validate input
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", transactionId)){
        res$status <- 400
        return("Error: Incorrect format for argument 'transactionId'.")
    }
    if(grepl("\\.\\.|~|\\\\|\\/", filename)){ # filesystem snooping protection
        res$status <- 400
        return("Error: Invalid value for argument 'filename'.")
    }
    if(subDir == "Input"){
        subDir <- IN_DIR
    } else if(subDir == "Output"){
        subDir <- OUT_DIR
    } else {
        res$status <- 400
        return("Error: Invalid value for argument 'subDir'.")
    }
    
    outDir <- paste0(APP_DIR, subDir, "/", transactionId, "/")
    fileToServe <- paste0(outDir, filename)
    readBin(fileToServe, "raw", n=file.info(fileToServe)$size)
}

#* Check if randomly-generated UUID already exists. This situation should be extremely rare, but not impossible. (internal use only).
#* @tag "File Generation and Handling"
#* @get /checkId
checkId <- function(res, req){
    outDir <- paste0(APP_DIR, "/", OUT_DIR)
    transactions <- Sys.glob(paste0(outDir, "*"))
    transactionId <- UUIDgenerate()
    fullIDs <- unlist(transactions, recursive=FALSE)
    fullIDs <- unlist(lapply(fullIDs, function(x){
        # Get just the ID
        return(unlist(str_split(x, OUT_DIR))[2])
    }))
    while(transactionId %in% fullIDs){
        # Regenerate UUID
        transactionId <- UUIDgenerate()
    }
    return(transactionId)
}

#* Create input files on filesystem (internal use only).
#* @tag "File Generation and Handling"
#* @param transactionId Unique ID assigned to request. Automatically generated by Tox21Enricher.
#* @param enrichmentSets User's input sets.
#* @param setNames Input set names.
#* @param mode Mode of request:<br><ul><li><b>casrn</b> - enrich from user-provided list of CASRNs</li><li><b>substructure</b> - shared substructures search</li><li><b>similarity</b> - Tanimoto similarity search</li><li><b>annotation</b> - view list of annotations associated with input chemicals</li></ul>
#* @param nodeCutoff Cutoff value that determines the top N annotations represented from each class in the enrichment results.
#* @param annoSelectStr String of all annotation classes to be used in request.
#* @post /createInput
createInput <- function(res, req, transactionId="-1", enrichmentSets, setNames, mode, nodeCutoff=10, annoSelectStr){
    # Validate input
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", transactionId)){
        res$status <- 400
        return("Error: Incorrect format for argument 'transactionId'.")
    }
    tmpSets <- unlist(str_split(enrichmentSets, "\\n"))
    for(i in tmpSets){
        if(!grepl("([0-9]+-[0-9]+-[0-9]+)|(NOCAS_[0-9]+)__[A-Za-z0-9]+", i)){
            res$status <- 400
            return("Error: Invalid value for argument 'enrichmentSets'.")
        }
    }
    if(!grepl("[A-Za-z0-9]+((\\n)+[A-Za-z0-9]+)*(\\n)*", setNames)){
        res$status <- 400
        return("Error: Incorrect format for argument 'setNames'.")
    }
    if(mode != "casrn" & mode != "substructure" & mode != "similarity" & mode != "annotation"){
        res$status <- 400
        return("Error: Invalid value for argument 'mode'.")
    }
    if(is.na(as.double(nodeCutoff))){
        res$status <- 400
        return("Error: Incorrect value for argument 'nodeCutoff'. Must be between 1 and 50 (inclusive).")
    } else if(as.double(nodeCutoff) < 1 | as.double(nodeCutoff) > 50){
        res$status <- 400
        return("Error: Incorrect value for argument 'nodeCutoff'. Must be between 1 and 50 (inclusive).")
    }
    if(!grepl("([A-Za-z0-9]+=checked,)+", annoSelectStr)){
        res$status <- 400
        return("Error: Incorrect format for argument 'annoSelectStr'.")
    } else {
        annotationList <- getAnnotationListInternal()
        # Check if all annotations are valid
        cleanedAnnoList <- unlist(str_split(annoSelectStr, "=checked,"))
        cleanedAnnoList <- cleanedAnnoList[nchar(cleanedAnnoList) > 0]
        errorAnnotationClasses <- lapply(cleanedAnnoList, function(x){
            if(!(x %in% annotationList)){
                return(x)
            }
            return(NULL)
        })
        errorAnnotationClasses <- errorAnnotationClasses[!vapply(errorAnnotationClasses, is.null, FUN.VALUE=logical(1))]
        if(length(errorAnnotationClasses) > 0){
            res$status <- 400
            return(paste0("Error: Invalid annotation class(es): ", paste0(errorAnnotationClasses, collapse=", "), ". Use '", API_PROTOCOL, API_ADDR, "/getAnnotationList' to see a list of valid annotation classes."))
        }
    }
    # Get enrichment sets from sent string
    enrichmentSetNames <- unlist(str_split(setNames, "\n"))
    enrichmentSetsSplit <- unlist(str_split(enrichmentSets, "\n"))
    enrichmentSets <- lapply(enrichmentSetNames, function(x){
        innerList <- unlist(lapply(enrichmentSetsSplit, function(casrn){
            tmpSplit <- unlist(str_split(casrn, "__"))
            if(tmpSplit[2] == x) {
                return(tmpSplit[1])
            } else {
                return(NULL)
            }
        }))
        innerList <- innerList[!vapply(innerList, is.null, FUN.VALUE=logical(1))]
    })
    names(enrichmentSets) <- enrichmentSetNames
    # Create input directory
    inDir <- paste0(APP_DIR, IN_DIR, transactionId, "/")
    dir.create(inDir)
    # Create output directory
    outDir <- paste0(APP_DIR, OUT_DIR, transactionId, "/")
    dir.create(outDir)
    # Connect to db
    poolInput <- conn()
    # Create input set txt for enrichment
    lapply(names(enrichmentSets), function(i) {
        tryCatch({
            outString <- ""
            # Fetch chemical name from database to match to CASRN
            fetchedNames <- unlist(lapply(enrichmentSets[[i]], function(j) {
                query <- sqlInterpolate(ANSI(), paste0("SELECT testsubstance_chemname FROM chemical_detail WHERE CASRN LIKE '", j, "';"), id=j)
                outp <- dbGetQuery(poolInput, query)
                if (dim(outp)[1] > 0 & dim(outp)[2] > 0) {
                    return(paste0(j, "\t", outp))
                } else {
                    return(paste0("err__", j))
                }
            }))
            # Initialize list to store good CASRNS
            goodCasrns <- fetchedNames[vapply(fetchedNames, function(x){
                if(grepl("^err__", x)){
                    return(FALSE)
                }
                return(TRUE)
            }, FUN.VALUE=logical(1))]
            # Initialize list to store error CASRNS
            errorCasrns <- fetchedNames[!vapply(fetchedNames, function(x){
                if(grepl("^err__", x)){
                    return(FALSE)
                }
                return(TRUE)
            }, FUN.VALUE=logical(1))]
            
            # Format fetched data to print to file
            outString <- paste0(goodCasrns, collapse="\n")
            # Strip "err__" from beginning of CASRNs
            errorCasrnsFormatted <- unlist(lapply(errorCasrns, function(x) unlist(str_split(x, "err__"))[2]))
            errString <- paste0(errorCasrnsFormatted, collapse="\n")
            # Only write if there are matching chemicals (don't create input files for empty sets)
            if(nchar(outString) > 0){
                inFile <- file(paste0(inDir, i, ".txt"))
                writeLines(outString, inFile)
                close(inFile) 
            }
            # If errors exist, create error CASRNs file
            if(nchar(errString) > 0){
                errFile <- file(paste0(outDir,  i, "__ErrorCASRNs.txt"))
                writeLines(errString, errFile)
                close(errFile) 
            }
        }, error=function(cond){
            print("Error creating input files for request.")
            print(cond)
        })
    })
    # Close pool
    # poolClose(poolInput)
    dbDisconnect(poolInput)
}

#* Returns list of sets that are valid/existing for a given transaction (internal use only).
#* @tag "File Generation and Handling"
#* @param transactionId Unique ID assigned to request. Automatically generated by Tox21Enricher.
#* @get /getInputSets
getInputSets <- function(res, req, transactionId="-1"){
    # Validate input
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", transactionId)){
        res$status <- 400
        return("Error: Incorrect format for argument 'transactionId'.")
    }
    inputDir <- paste0(APP_DIR, IN_DIR)
    inputFilesList <- Sys.glob(paste0(inputDir, transactionId, "/*.txt"))
    inputFilesList <- unlist(lapply(inputFilesList, function(x){
        x_lv1 <- gsub(paste0(inputDir, transactionId, "/"), "", x)
        x_lv2 <- gsub(".txt", "", x_lv1)
    }))
    return(inputFilesList)
}

#* Check if result files exist in Input/Output directories for given request (internal use only).
#* @tag "File Generation and Handling"
#* @param transactionId Unique ID assigned to request. Automatically generated by Tox21Enricher.
#* @get /exists
exists <- function(res, req, transactionId="-1"){
    # Validate input
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", transactionId)){
        res$status <- 400
        return("Error: Incorrect format for argument 'transactionId'.")
    }
    # Check if DB records exist
    # Connect to db
    poolExists <- connQueue()
    # Update database with transaction entry
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM transaction WHERE uuid='", transactionId, "';"), id="loadTransaction")
    outp <- dbGetQuery(poolExists, query)
    # Close pool
    # poolClose(poolExists)
    dbDisconnect(poolExists)
    if(nrow(outp) < 1){
        return(-1) # case: no record in database
    }
    # else, check if result files exist
    outDir <- paste0(APP_DIR, OUT_DIR)
    checkIfOutFile <- paste0(outDir, transactionId, "/tox21enricher_", transactionId, ".zip")
    if(file.exists(checkIfOutFile)){
        return(1) # case: both database record and result files exist
    } else {
        return(0) # case: record in database but missing result files on filesystem
    }
}

## SECTION 5: DEALING WITH ENRICHMENT RESULTS

#* Get results for given directory (internal use only).
#* @tag "Results Handling"
#* @param transactionId Unique ID assigned to request. Automatically generated by Tox21Enricher.
#* @setName Optional: if supplied, is the set name to fetch from.
#* @get /getResults
getResults <- function(res, req, transactionId, setName="###"){
    # Validate input
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", transactionId)){
        res$status <- 400
        return("Error: Incorrect format for argument 'transactionId'.")
    }
    
    inDir <- paste0(APP_DIR, IN_DIR, transactionId, "/")
    outDir <- paste0(APP_DIR, OUT_DIR, transactionId, "/")
    setFiles <- NULL
    if(setName == "###") {
        setFilesOut <- Sys.glob(paste0(outDir, "*"))
        setFilesIn <- Sys.glob(paste0(inDir, "*"))
        # Format input set filenames
        setFilesIn <- lapply(setFilesIn, function(x){
            tmpStr <- gsub(".txt", "", x)
            return(paste0(tmpStr, "__Input.txt"))
        })
        setFiles <- append(setFilesIn, setFilesOut)
    } else {
        # Validate input
        if(!grepl("[A-Za-z0-9]+", setName)){
            res$status <- 400
            return("Error: Incorrect format for argument 'SetName'.")
        }
        
        setFilesOut <- Sys.glob(paste0(outDir, setName, "*"), dirmark=FALSE) 
        setFilesOut <- unlist(lapply(setFilesOut, function(fileName){
            tmpSplit <- unlist(str_split(fileName, outDir))
            return(paste0(OUT_DIR, transactionId, "/", tmpSplit[2]))
        }))
        setFilesIn <- Sys.glob(paste0(inDir, setName, "*"), dirmark=FALSE) 
        setFilesIn <- unlist(lapply(setFilesIn, function(fileName){
            tmpSplit <- unlist(str_split(fileName, inDir))
            return(paste0(IN_DIR, transactionId, "/", tmpSplit[2]))
        }))
        setFiles <- append(setFilesIn, setFilesOut)
    }
    # Add zip file
    setFiles <- append(setFiles, paste0(OUT_DIR, transactionId, "/tox21enricher_", transactionId, ".zip"))
    return(setFiles)
}

#* Read gct files for a given request (internal use only).
#* @tag "Results Handling"
#* @param transactionId Unique ID assigned to request. Automatically generated by Tox21Enricher.
#* @param cutoff Cutoff value that determines the top N annotations represented from each class in the enrichment results.
#* @param mode "chart" or "cluster."
#* @get /readGct
readGct <- function(res, req, transactionId="-1", cutoff=10, mode, set="Set1"){
    # Validate input
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", transactionId)){
        res$status <- 400
        return("Error: Incorrect format for argument 'transactionId'.")
    }
    if(is.na(as.double(cutoff))){
        res$status <- 400
        return("Error: Incorrect value for argument 'cutoff'. Must be between 1 and 50 (inclusive).")
    } else if(as.double(cutoff) < 1 | as.double(cutoff) > 50){
        res$status <- 400
        return("Error: Incorrect value for argument 'cutoff'. Must be between 1 and 50 (inclusive).")
    }
    if(mode != "set" & mode != "chart" & mode != "cluster") {
        res$status <- 400
        return("Error: Invalid value for argument 'mode'.")
    }
    if(!grepl("[A-Za-z0-9]+", set)) {
        res$status <- 400
        return("Error: Invalid value for argument 'set'.")
    }
    
    outDir <- paste0(APP_DIR, OUT_DIR, transactionId, "/")
    gctFile <- NULL

    if(mode == "chart"){
        if(!file.exists(paste0(outDir, "gct/Chart_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.gct")) & !file.exists(paste0(outDir, "gct/Chart_Top", cutoff, "_ALL__BH_0.05_BH__ValueMatrix.gct"))){
            return(NULL)
        }
        tryReadChart <- tryCatch({
            if(file.exists(paste0(outDir, "gct/Chart_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.gct"))){
                gctFile <- read.delim(paste0(outDir, "gct/Chart_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.gct"), skip=2, header=TRUE, sep="\t", row.names=1, comment.char="", fill=FALSE, colClasses=c("Terms"="NULL"), check.names=FALSE)
            } else if(file.exists(paste0(outDir, "gct/Chart_Top", cutoff, "_ALL__BH_0.05_BH__ValueMatrix.gct"))){
                gctFile <- read.delim(paste0(outDir, "gct/Chart_Top", cutoff, "_ALL__BH_0.05_BH__ValueMatrix.gct"), skip=2, header=TRUE, sep="\t", row.names=1, comment.char="", fill=FALSE, colClasses=c("Terms"="NULL"), check.names=FALSE)
            }
            TRUE
        }, error=function(cond){
            FALSE
        })
        if(!tryReadChart){
            return(NULL)
        }
    } else if(mode == "cluster"){
        if(!file.exists(paste0(outDir, "gct/Cluster_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.gct")) & !file.exists(paste0(outDir, "gct/Cluster_Top", cutoff, "_ALL__BH_0.05_BH__ValueMatrix.gct"))){
            return(NULL)
        }
        tryReadCluster <- tryCatch({
            if(file.exists(paste0(outDir, "gct/Cluster_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.gct"))){
                gctFile <- read.delim(paste0(outDir, "gct/Cluster_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.gct"), skip=2, header=TRUE, sep="\t", row.names=1, comment.char="", fill=FALSE, colClasses=c("Terms"="NULL"), check.names=FALSE)
            } else if(file.exists(paste0(outDir, "gct/Cluster_Top", cutoff, "_ALL__BH_0.05_BH__ValueMatrix.gct"))){
                gctFile <- read.delim(paste0(outDir, "gct/Cluster_Top", cutoff, "_ALL__BH_0.05_BH__ValueMatrix.gct"), skip=2, header=TRUE, sep="\t", row.names=1, comment.char="", fill=FALSE, colClasses=c("Terms"="NULL"), check.names=FALSE)
            }
            TRUE
        }, error=function(cond){
            FALSE
        })
        if(!tryReadCluster){
            return(NULL)
        }
    } else { # per set
        if(!file.exists(paste0(outDir, "gct_per_set/", set, "__Chart.gct"))){
            return(NULL)
        }
        tryReadChart <- tryCatch({
            gctFile <- read.delim(paste0(outDir, "gct_per_set/", set, "__Chart.gct"), skip=2, header=TRUE, sep="\t", row.names=1, comment.char="", fill=FALSE, colClasses=c("Name"="NULL"), check.names=FALSE)
            TRUE
        }, error=function(cond){
            FALSE
        })
        if(!tryReadChart){
            return(NULL)
        }
    }
    if(nrow(gctFile) < 1){
        return(NULL)
    }
    return(gctFile)
}

#* Return Chart Simple file for given request and input set (internal use only).
#* @tag "Results Handling"
#* @param transactionId Unique ID assigned to request. Automatically generated by Tox21Enricher.
#* @param enrichmentSets Prepared string including the names of the enrichment sets generated by Tox21Enricher.
#* @get /getBargraph
getBargraph <- function(res, req, transactionId="-1", enrichmentSets){
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", transactionId)){
        res$status <- 400
        return("Error: Incorrect format for argument 'transactionId'.")
    }
    if(!grepl("[A-Za-z0-9]+(__[A-Za-z0-9]+)*", enrichmentSets)){
        res$status <- 400
        return("Error: Invalid value for argument 'enrichmentSets'.")
    }
    # format enrichment sets
    enrichmentSets <- unlist(str_split(enrichmentSets, "__"))
    # define results directory
    baseDirName <- paste0(APP_DIR, OUT_DIR, transactionId, "/")
    # Get all Chart Simple files
    allChartFiles <- Sys.glob(paste0(baseDirName, "*__ChartSimple.txt"))
    # Extract set names from available files
    names(allChartFiles) <- lapply(allChartFiles, function(x){
        tmp <- unlist(str_split(x, paste0(transactionId, "/")))[2]
        return(unlist(str_split(tmp, "__"))[1])
    })
    bgChart <- lapply(enrichmentSets, function(setName){
        if(setName %in% names(allChartFiles)){
            chartFile <- allChartFiles[setName]
            bgChartTmp <- NULL
            if(!file.exists(chartFile)){
                return(list())
            }
            tryReadFile <- tryCatch({
                bgChartTmp <- read.delim(chartFile, header=TRUE, sep="\t", comment.char="", fill=FALSE)
                TRUE
            }, error=function(cond){
                FALSE
            })
            if(!tryReadFile){
                return(list())
            }
            
            # Reformat P-Values to character vectors so they don't get truncated when being converted to json upon response
            bgChartTmp["PValue"] <- lapply(bgChartTmp["PValue"], function(x) as.character(x))
            if("Bonferroni" %in% colnames(bgChartTmp)){
                bgChartTmp["Bonferroni"] <- lapply(bgChartTmp["Bonferroni"], function(x) as.character(x))
            }
            if("Benjamini_Yekutieli" %in% colnames(bgChartTmp)){
                bgChartTmp["Benjamini_Yekutieli"] <- lapply(bgChartTmp["Benjamini_Yekutieli"], function(x) as.character(x))
            }
            if("Benjamini_Hochberg" %in% colnames(bgChartTmp)){
                bgChartTmp["Benjamini_Hochberg"] <- lapply(bgChartTmp["Benjamini_Hochberg"], function(x) as.character(x))
            }
            return(bgChartTmp)
        }
        return(list())
    })
    return(bgChart)
}

#* Generate interactive visNetwork for given request and input set (internal use only).
#* @tag "Results Handling"
#* @param transactionId Unique ID assigned to request. Automatically generated by Tox21Enricher.
#* @param cutoff Cutoff value that determines the top N annotations represented from each class in the enrichment results.
#* @param mode "chart" or "cluster."
#* @param input Prepared string with the current input set generated by Tox21Enricher.
#* @param qval Q-value for use in network generation.
#* @get /getNetwork
getNetwork <- function(res, req, transactionId="-1", cutoff, mode, input, qval=0.05){
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", transactionId)){
        res$status <- 400
        return("Error: Incorrect format for argument 'transactionId'.")
    }
    if(is.na(as.double(cutoff))){
        res$status <- 400
        return("Error: Incorrect value for argument 'cutoff'. Must be between 1 and 50 (inclusive).")
    } else if(as.double(cutoff) < 1 | as.double(cutoff) > 50){
        res$status <- 400
        return("Error: Incorrect value for argument 'cutoff'. Must be between 1 and 50 (inclusive).")
    }
    if(mode != "chart" & mode != "cluster"){
        res$status <- 400
        return("Error: Invalid value for argument 'mode'.")
    }
    if(!grepl("[A-Za-z0-9]+(#[A-Za-z0-9]+)*", input)){
        res$status <- 400
        return("Error: Invalid value for argument 'input'.")
    }
    if(is.na(as.numeric(qval))){
        res$status <- 400
        return("Error: Invalid value for q-value.")
    }
    if(!is.finite(as.numeric(10^-(as.numeric(qval))))){
        res$status <- 400
        return("Error: Invalid value for q-value.")
    }
    
    input <- unlist(str_split(input, "#"))
    baseDirName <- paste0(APP_DIR, OUT_DIR, transactionId, "/")
    chartForNetFile <- NULL
    # First, read files from /gct/ directory
    if(mode == "chart") {
        if(!file.exists(paste0(baseDirName, "/gct/Chart_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.ForNet")) & !file.exists(paste0(baseDirName, "/gct/Chart_Top", cutoff, "_ALL__BH_0.05_BH__ValueMatrix.ForNet"))){
            return(NULL)
        }
        tryReadChart <- tryCatch({
            if(file.exists(paste0(baseDirName, "/gct/Chart_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.ForNet"))){
                chartForNetFile <- read.delim(paste0(baseDirName, "/gct/Chart_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.ForNet"), sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, fill=TRUE)
            } else if(file.exists(paste0(baseDirName, "/gct/Chart_Top", cutoff, "_ALL__BH_0.05_BH__ValueMatrix.ForNet"))){
                chartForNetFile <- read.delim(paste0(baseDirName, "/gct/Chart_Top", cutoff, "_ALL__BH_0.05_BH__ValueMatrix.ForNet"), sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, fill=TRUE)
            }
            TRUE
        }, error=function(cond){
            FALSE
        })
        if(!tryReadChart){
            return(NULL)
        }
    } else { # Cluster
        if(!file.exists(paste0(baseDirName, "/gct/Cluster_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.ForNet")) & !file.exists(paste0(baseDirName, "/gct/Cluster_Top", cutoff, "_ALL__BH_0.05_BH__ValueMatrix.ForNet"))){
            return(NULL)
        }
        tryReadChart <- tryCatch({
            if(file.exists(paste0(baseDirName, "/gct/Cluster_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.ForNet"))){
                chartForNetFile <- read.delim(paste0(baseDirName, "/gct/Cluster_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.ForNet"), sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, fill=TRUE)
            } else if(file.exists(paste0(baseDirName, "/gct/Cluster_Top", cutoff, "_ALL__BH_0.05_BH__ValueMatrix.ForNet"))){
                chartForNetFile <- read.delim(paste0(baseDirName, "/gct/Cluster_Top", cutoff, "_ALL__BH_0.05_BH__ValueMatrix.ForNet"), sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, fill=TRUE)
            }
            TRUE
        }, error=function(cond){
            FALSE
        })
        if(!tryReadChart){
            return(NULL)
        }
    }
    if(is.null(chartForNetFile)) {
        return(NULL)
    }
    chartNetworkUIDs <- unlist(mclapply(seq_len(nrow(chartForNetFile)), mc.cores=CORES, mc.silent=FALSE, function(x){
        include <- lapply(input, function(y) {
            i <- gsub("\\s+", ".", y)
            if(i %in% names(chartForNetFile)){
                if(!is.na(chartForNetFile[x, i])) {
                    return(TRUE)
                }
                return(NULL)
            }
            return(NULL)
        })
        include <- include[!vapply(include, is.null, FUN.VALUE=logical(1))]
        include <- include[!vapply(include, is.na, FUN.VALUE=logical(1))]
        if(length(include) > 0) {
            return(paste0(chartForNetFile[x, "UID"], collapse=",")) # If significant to multiple input sets
        }
        return(NULL)
    }))
    chartNetworkUIDs <- chartNetworkUIDs[!vapply(chartNetworkUIDs, is.null, FUN.VALUE=logical(1))]
    
    # If no UIDs, return empty list to trigger error handling on client
    if(length(chartNetworkUIDs) < 1) {
        return(list())
    }
    # Create placeholder string for querying database
    termsStringPlaceholder <- paste0(chartNetworkUIDs, collapse=",")
    # Connect to db
    poolNetwork <- conn()
    # Query database
    queryNetwork <- sqlInterpolate(ANSI(), paste0(
        "SELECT p.*, a.annoterm as name1, b.annoterm as name2, ac.annoclassname as class1, bc.annoclassname as class2, ac.baseurl as url1, bc.baseurl as url2
        FROM annoterm_pairwise p
        LEFT JOIN annotation_detail a 
        ON p.term1uid=a.annotermid
        LEFT JOIN annotation_detail b 
        ON p.term2uid=b.annotermid
        LEFT JOIN annotation_class ac 
        ON a.annoclassid=ac.annoclassid
        LEFT JOIN annotation_class bc 
        ON b.annoclassid=bc.annoclassid
        WHERE p.term1uid IN (", termsStringPlaceholder, ") AND p.term2uid IN (", termsStringPlaceholder, ") AND p.qvalue <", 10^-(as.numeric(qval)), ";"
    ), id="addToDb")
    outpNetwork <- dbGetQuery(poolNetwork, queryNetwork)
    dbDisconnect(poolNetwork)
    return(outpNetwork)
}

#* Get lists of chemicals that are associated with the annotations displayed in the nodes in the network (internal use only).
#* @tag "Results Handling"
#* @param termFrom Annotation for "from" node in network.
#* @param termTo Annotation for "to" node in network.
#* @param classFrom Annotation class for "from" node in network.
#* @param classTo Annotation class for "to" node in network.
#* @get /getNodeChemicals
getNodeChemicals <- function(res, req, termFrom, termTo, classFrom, classTo){
    if(!grepl("[A-Za-z0-9_]+", classFrom)){
        res$status <- 400
        return("Error: Invalid value for argument 'classFrom'.")
    } else {
        if(!(classFrom %in% names(funCatTerm2CASRN))){
            res$status <- 400
            return("Error: Class not found for argument 'classFrom'.")
        }
    }
    if(!grepl("[A-Za-z0-9_]+", classTo)){
        res$status <- 400
        return("Error: Invalid value for argument 'classTo'.")
    } else {
        if(!(classTo %in% names(funCatTerm2CASRN))){
            res$status <- 400
            return("Error: Class not found for argument 'classTo'.")
        }
    }
    if(!(termFrom %in% names(funCatTerm2CASRN[[classFrom]]))){
        res$status <- 400
        return(paste0("Error: Term '", termFrom, "' not found in class ", classFrom, "."))
    }
    if(!(termTo %in% names(funCatTerm2CASRN[[classTo]]))){
        res$status <- 400
        return(paste0("Error: Term '", termTo, "' not found in class ", classTo, "."))
    }
    # Connect to db
    poolNode <- conn()
    # Get internal ID number of From annotation
    nodeQuery <- sqlInterpolate(ANSI(), paste0("
        SELECT DISTINCT
            ad.annotermid
        FROM
            annotation_detail ad,
            annotation_class ac,
            term2casrn_mapping tcm
            
        WHERE
            tcm.annotermid = ad.annotermid
        AND tcm.annoclassid = ac.annoclassid
        AND ac.annoclassname = '", classFrom, "'
        AND ad.annoterm = '", termFrom, "'
    ;" ), id="getFromID")
    
    nodeOutp <- dbGetQuery(poolNode, nodeQuery)
    fromID <- nodeOutp
    
    # Get internal ID number of To annotation
    nodeQuery <- sqlInterpolate(ANSI(), paste0("
        SELECT DISTINCT
            ad.annotermid
        FROM
            annotation_detail ad,
            annotation_class ac,
            term2casrn_mapping tcm
            
        WHERE
            tcm.annotermid = ad.annotermid
        AND tcm.annoclassid = ac.annoclassid
        AND ac.annoclassname = '", classTo, "'
        AND ad.annoterm = '", termTo, "'
    ;" ), id="getToID")
    
    nodeOutp <- dbGetQuery(poolNode, nodeQuery)
    toID <- nodeOutp
    # Get list of casrns associated with From annotation
    nodeQuery <- sqlInterpolate(ANSI(), paste0("
        SELECT
            cd.casrn
        FROM
            term2casrn_mapping tcm,
            chemical_detail cd
        WHERE
            tcm.casrnuid = cd.casrnuid
        AND tcm.annotermid = ", fromID, "
    ;" ), id="getCasrnsFrom")
    nodeOutp <- dbGetQuery(poolNode, nodeQuery)
    casrnsFrom <- nodeOutp
    
    # Get list of casrns associated with To annotation
    nodeQuery <- sqlInterpolate(ANSI(), paste0("
        SELECT
            cd.casrn
        FROM
            term2casrn_mapping tcm,
            chemical_detail cd
        WHERE
            tcm.casrnuid = cd.casrnuid
        AND tcm.annotermid = ", toID, "
    ;" ), id="getCasrnsTo")
    
    nodeOutp <- dbGetQuery(poolNode, nodeQuery)
    casrnsTo <- nodeOutp
    
    dbDisconnect(poolNode)
    return(list(casrnsFrom=casrnsFrom[, "casrn"], casrnsTo=casrnsTo[, "casrn"]))
}

#* Get link from database to view additional info for a selected node in the network (internal use only).
#* @tag "Results Handling"
#* @param term Annotation depicted in selected node.
#* @param class Annotation class for annotation depicted in selected node.
#* @get /getNodeDetails
getNodeDetails <- function(res, req, class){
    if(!grepl("[A-Za-z0-9_]+", class)){
        res$status <- 400
        return("Error: Incorrect format for argument 'class'.")
    }
    # Connect to db
    poolNode <- conn()
    # Get annotation detail link
    nodeQuery <- sqlInterpolate(ANSI(), paste0( "SELECT baseurl FROM annotation_class WHERE annoclassname='", class, "';" ), id="getNodeDetailsFromClass")
    nodeOutp <- dbGetQuery(poolNode, nodeQuery)
    baseurl <- nodeOutp
    # Close pool
    # poolClose(poolNode)
    dbDisconnect(poolNode)
    return(baseurl)
}

#* Get colors from database to display nodes in the network (internal use only).
#* @tag "Results Handling"
#* @get /getNodeColors
getNodeColors <- function(res, req){
    # Connect to db
    poolNode <- conn()
    # Get annotation detail link
    nodeQuery <- sqlInterpolate(ANSI(), paste0( "SELECT annoclassname, networkcolor FROM annotation_class;" ), id="getNodeColors")
    nodeOutp <- dbGetQuery(poolNode, nodeQuery)
    nodeColors <- nodeOutp
    # Close pool
    # poolClose(poolNode)
    dbDisconnect(poolNode)
    return(nodeColors)
}

## SECTION 6: API CLIENT ENDPOINTS FOR OPERATING IN NO-GUI MODE

#* Get list of all annotations in the database.
#* @tag "Client Functionality"
#* @get /getAnnotationList
getAnnotationList <- function(res, req){
    return(getAnnotationListInternal())
}

#* Post input set for enrichment analysis.
#* @tag "Client Functionality"
#* @param mode The mode to use when performing enrichment analysis.<ul><li><b>casrn</b> = CASRN input.</b></li><li><b>substructure</b> = SMILES or InChI input.</li><li><b>similarity</b> = SMILES or InChI input.</li><li><b>annotation</b> = View annotations.</li></ul>
#* @param input Comma-separated string of chemicals for this enrichment process.
#* @param annotations Comma-separated string of all enabled annotations for this enrichment process.
#* @param cutoff Node cutoff value - integers only between 1 - 50.
#* @param tanimoto Tanimoto threshold for similarity search (default = 0.5).
#* @post /submit
submit <- function(res, req, mode="", input="", annotations="", cutoff=10, tanimoto=0.5) {
    # Check if mode is missing
    if(is.null(mode) | mode == ""){
        res$status <- 400
        return("Error: No mode specified. ('casrn', 'substructure', 'similarity', or 'annotation')")
    }
    # Check if mode value is invalid
    if(mode != "casrn" & mode != "substructure" & mode != "similarity" & mode != "annotation"){
        res$status <- 400
        return("Error: Invalid value for argument 'mode'.")
    }
    
    # Check if input is missing
    if(is.null(input) | input == ""){
        res$status <- 400
        return("Error: No input supplied.")
    }
    # Check if user inputs any bad chars
    if(grepl("'|\"|;|--", input)){
        res$status <- 400
        return("Error: Invalid value for argument 'input'.")
    }
    
    # Check if we can coerce cutoff to number
    if(typeof(cutoff) == "character"){
        cutoff <- as.numeric(cutoff)
    }
    if(is.na(cutoff)){
        res$status <- 400
        return("Error: Cutoff value is not a number.")
    }
    # Check if cutoff is not a number
    if(!is.numeric(cutoff)){
        res$status <- 400
        return("Error: Cutoff value is not a number.")
    }
    # Check if cutoff is not an integer
    if(cutoff %% 1 != 0){
        res$status <- 400
        return("Error: Cutoff value is not an integer.")
    }
    # Check if cutoff is out of range
    if(cutoff < 1 | cutoff > 50){
        res$status <- 400
        return("Error: Cutoff value must be between 1 and 50 inclusive.")
    }
    
    # Check if we can coerce Tanimoto to number
    if(typeof(tanimoto) == "character"){
        tanimoto <- as.numeric(tanimoto)
    }
    if(is.na(tanimoto)){
        res$status <- 400
        return("Error: Tanimoto threshold is not a number.")
    }
    # Check if Tanimoto is not a number
    if(!is.numeric(tanimoto)){
        res$status <- 400
        return("Error: Tanimoto threshold is not a number.")
    }
    # Check if Tanimoto is out of range
    if(tanimoto < 0.02 | tanimoto > 1.00){
        res$status <- 400
        return("Error: Tanimoto threshold must be between 0.02 and 1.00 inclusive.")
    }
    
    # Paths
    inBaseDir <- paste0(APP_DIR, "/", IN_DIR)
    outBaseDir <- paste0(APP_DIR, "/", OUT_DIR)
    # Generate UUID for enrichment process (return this to user later)
    transactionId <- UUIDgenerate()
    # Check if UUID already exists. If so, regenerate UUID. This should be very rare but this is here just in case
    transactions <- Sys.glob(paste0(outBaseDir, "*"))
    while(transactionId %in% transactions) {
        transactionId <- UUIDgenerate()
    }
    # Ignore Tanimoto threshold value if not similarity
    if(mode != "similarity"){
        tanimoto <- 0.5
    }
    
    # Get list of all annotation classes in database
    annotationList <- getAnnotationListInternal()
    # Set annotations to default if missing
    if(annotations == ""){
        annotations <- paste0(annotationList, collapse=",")
    }
    # Check if all annotations are valid
    errorAnnotationClasses <- lapply(unlist(str_split(annotations, ",")), function(x){
        if(!(x %in% annotationList)){
            return(x)
        }
        return(NULL)
    })
    errorAnnotationClasses <- errorAnnotationClasses[!vapply(errorAnnotationClasses, is.null, FUN.VALUE=logical(1))]
    if(length(errorAnnotationClasses) > 0){
        res$status <- 400
        return(paste0("Error: Invalid annotation class(es): ", paste0(errorAnnotationClasses, collapse=", "), ". Use '", API_PROTOCOL, API_ADDR, "/getAnnotationList' to see a list of valid annotation classes."))
    }
    # Put annotations into correct form (annotation selection string with =checked)
    annotations <- gsub(",", "=checked,", annotations)
    
    # Open main pool
    pool <- conn()
    # Validate & prepare input
    finalInput <- NULL
    errorCasrns <- list()
    setNames <- list()
    # 1) strip horizontal whitespace and condense multiple newlines
    casrnValidatedInput <- gsub(" ", "", input)
    casrnValidatedInput <- gsub("\\t", "", casrnValidatedInput)
    casrnValidatedInput <- gsub("\\n+", "\n", casrnValidatedInput)
    inputList <- unlist(str_split(casrnValidatedInput, "\n"))
    # Remove empty items from input list
    inputList <- unlist(lapply(inputList, function(x){
        if(nchar(x) > 0){
            return(x)
        } else {
            return(NULL)
        }
    }))
    inputList <- inputList[!vapply(inputList, is.null, FUN.VALUE=logical(1))]
    
    # If SMILES/InChI input, do the following:
    if(mode == "similarity" | mode == "substructure") {
        # First, check if we have any InChIs, and convert to SMILES
        inputList <- unlist(lapply(seq_len(length(inputList)), function(i){
            if (grepl("InChI=", inputList[i])) {
                queryInchi <- sqlInterpolate(ANSI(), paste0("SELECT smiles FROM chemical_detail WHERE inchis='", inputList[i], "';"), id="smilesResults")
                outpInchi <- dbGetQuery(pool, queryInchi)
                if(length(outpInchi) > 0){
                    return(outpInchi[[1]])
                } else {
                    return(NULL)
                }
            } 
            return(inputList[i])
        }))
        inputList <- inputList[!vapply(inputList, is.null, FUN.VALUE=logical(1))]
        
        # Build CASRN input from SMILES/InChI
        setNameIndex <- 1
        finalInput <- lapply(inputList, function(i){
            query <- ""
            if(mode == "similarity"){
                query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM get_mfp2_neighbors('", i, "');"))
            } else { # substructure
                query <- sqlInterpolate(ANSI(), paste0("SELECT casrn FROM mols_2 WHERE m @> CAST('", i, "' AS mol);"))  
            }
            outp <- dbGetQuery(pool, query)
            if(length(outp) > 0){ 
                currentSet <- paste0("Set", setNameIndex)
                fetchedCASRNs <- lapply(seq_len(nrow(outp)), function(index){
                    casrn <- paste0(outp[index, "casrn"])
                    return(casrn)
                })
                setNameIndex <<- setNameIndex + 1
                return(unlist(fetchedCASRNs))
            } else {
                setNameIndex <<- setNameIndex + 1
                return(NULL)
            }
        })
        names(finalInput) <- lapply(seq_len(length(finalInput)), function(x){
            return(paste0("#Set", x))
        })
        finalInput <- finalInput[!vapply(finalInput, is.null, FUN.VALUE=logical(1))]
        # Set setnames to pass to function
        setNames <- paste0(names(finalInput), collapse="\n")
        casrnValidatedInput <- lapply(seq_len(length(finalInput)), function(x){
            return(paste0(names(finalInput)[x], "\n", paste0(finalInput[[x]], collapse="\n")))
        })
        casrnValidatedInput <- paste0(casrnValidatedInput, collapse="\n")
    }
    # 2) check if of the form ###-###-### or setname
    casrnValidatedInput <- unlist(str_split(casrnValidatedInput, "\n"))
    casrnValidatedInput <- unlist(lapply(casrnValidatedInput, function(x){
        if(nchar(x) > 0){
            return(x)
        } else {
            return(NULL)
        }
    }))
    casrnValidatedInput <- casrnValidatedInput[!vapply(casrnValidatedInput, is.null, FUN.VALUE=logical(1))]
    errorCasrns <- lapply(seq_len(length(casrnValidatedInput)), function(i){
        if(!grepl("^#[A-Za-z0-9]+|[0-9]+-[0-9]+-[0-9]+|(NOCAS_[0-9]+)", casrnValidatedInput[i], ignore.case=TRUE)) {
            return(i)
        }
        return(NULL)
    })
    errorCasrns <- errorCasrns[!vapply(errorCasrns, is.null, FUN.VALUE=logical(1))]
    
    # If there are errors
    if(length(errorCasrns) > 0){
        # Close DB connection
        # poolClose(pool)
        dbDisconnect(pool)
        return(paste0("Error: Incorrect CASRN or set name formatting on input line(s): ", paste0(errorCasrns, collapse=", "), ". Please check your input and try again.")) 
    }
    # 3) check if missing first set name, if you are using set names
    usingSetNames <- FALSE
    usingSetNamesCheck <- lapply(seq_len(length(casrnValidatedInput)), function(i) {
        if(grepl("^#[A-Za-z0-9]+", casrnValidatedInput[i], ignore.case=TRUE)) { # Detect if we are using set names
            return(TRUE)
        }
        return(NULL)
    })
    usingSetNamesCheck <- usingSetNamesCheck[!vapply(usingSetNamesCheck, is.null, FUN.VALUE=logical(1))]
    if(length(usingSetNamesCheck) > 0){
        usingSetNames <- TRUE
    }
    if(!grepl("^#[A-Za-z0-9]+", casrnValidatedInput[1], ignore.case=TRUE) & usingSetNames) {
        # Close DB connection
        # poolClose(pool)
        dbDisconnect(pool)
        return(paste0("Error: It appears you are using set names but have not provided a name for the first input set. Please check your input and try again."))
    }
    
    # Check if duplicate set names
    setNamesList <- unlist(lapply(seq_len(length(casrnValidatedInput)), function(i) {
        if(grepl("^#[A-Za-z0-9]+", casrnValidatedInput[i], ignore.case=TRUE)) { # Detect if we are using set names
            return(casrnValidatedInput[i])
        }
        return(NULL)
    }))
    setNamesList <- setNamesList[!vapply(setNamesList, is.null, FUN.VALUE=logical(1))]
    setNamesDuplicate <- as.data.frame(table(setNamesList)) %>% filter(Freq > 1)
    if(nrow(setNamesDuplicate) > 0) { # if we have duplicate names, display error message
        # Close DB connection
        # poolClose(pool)
        dbDisconnect(pool)
        return(paste0("Error: Duplicate set names are not allowed: ", paste0(setNamesDuplicate$setNamesList, collapse=", ")))
    }
    
    # Set setnames to pass to function for casrn input
    if(mode == "casrn" | mode == "annotation"){
        casrnSets <- lapply(seq_len(length(casrnValidatedInput)), function(i){
            if(grepl("^#[A-Za-z0-9]+", casrnValidatedInput[i], ignore.case=TRUE)) { # Detect if we are using set names
                return(casrnValidatedInput[i])
            } else {
                return(NULL)
            }
        })
        casrnSets <- casrnSets[!vapply(casrnSets, is.null, FUN.VALUE=logical(1))]
        setNames <- paste0(casrnSets, collapse="\n")
    }
    # Put casrnValidatedInput back into a form we can pass to the createInput function
    currentSet <- NULL
    casrnValidatedInput <- lapply(seq_len(length(casrnValidatedInput)), function(i){
        if(grepl("^#[A-Za-z0-9]+", casrnValidatedInput[i], ignore.case=TRUE)) { # Detect if we are using set names
            currentSet <<- gsub("#", "", casrnValidatedInput[i])
            return(NULL)
        } else {
            return(paste0(casrnValidatedInput[i], "__", currentSet))
        }
    })
    casrnValidatedInput <- casrnValidatedInput[!vapply(casrnValidatedInput, is.null, FUN.VALUE=logical(1))]
    casrnValidatedInput <- paste0(casrnValidatedInput, collapse="\n")
    # Remove pound symbol from set names
    setNames <- lapply(setNames, function(x) gsub("#", "", x))
    setNames <- paste0(setNames, collapse="\n")
    # Create input file in queue
    createInput(transactionId=transactionId, enrichmentSets=casrnValidatedInput, setNames=setNames, mode=mode, nodeCutoff=cutoff, annoSelectStr=annotations)
    queue(mode=mode, enrichmentUUID=transactionId, annoSelectStr=annotations, nodeCutoff=cutoff, setNames=setNames)
    # Close DB connection
    # poolClose(pool)
    dbDisconnect(pool)
    return(transactionId)
}

#* Download enrichment results for a given UUID.
#* @tag "Client Functionality"
#* @serializer contentType list(type="application/zip")
#* @param id The UUID of the enrichment process to download.
#* @get /download
download <- function(res, req, id="-1") {
    # Validate input
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", id)){
        res$status <- 400
        return("Error: Incorrect format for argument 'id'.")
    }
    # async
    future_promise({
        fName <- paste0(APP_DIR, OUT_DIR, id, "/tox21enricher_", id, ".zip")
        if(file.exists(fName)){
            readBin(fName, 'raw', n=file.info(fName)$size)  
        } else {
            return(paste0("The supplied request ", id, " does not exist or has not completed yet. Please try again later."))
        }
    })
}

#* Check if enrichment results exist for a given UUID (i.e., check is the request has completed).
#* @tag "Client Functionality"
#* @param id The UUID of the enrichment process to check.
#* @get /isComplete
function(id="-1", res) {
    # Validate input
    if(!grepl("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", id)){
        res$status <- 400
        return("Error: Incorrect format for argument 'id'.")
    }
    # async
    future_promise({
        fName <- paste0(APP_DIR, OUT_DIR, id, "/tox21enricher_", id, ".zip")
        # Returns 1 if completed, 0 if not.
        if(file.exists(fName)){
            return(1)  
        } else {
            return(0)
        }
    })
}
# performEnrichment.R
###############################################################
# API for Tox21 Enricher                                      #
# developed by Junguk Hur and                                 #               
# Parker Combs (parker.combs@und.edu)                         #
###############################################################
#* @apiTitle Tox21 Enricher API
#* @apiDescription This is the Tox21 Enricher API. 
#* @apiContact list(name="Parker Combs", url="parker.combs@und.edu")
#* @apiVersion 1.0

library(config)
library(DBI)
library(future)
library(ggplot2)
library(httr)
library(parallel)
library(plyr)
library(pool)
library(promises)
library(reticulate)
library(rjson)
library(RPostgres)
library(stringr)
library(tidyverse)
library(uuid)

## Code to run on startup, not part of API endpoints

# Load params from config file
tox21config <- config::get("tox21enricher")
tox21queue <- config::get("tox21enricher-queue")
APP_VERSION <- tox21config$appversion
APP_DIR <- tox21config$appdir
IN_DIR <- tox21config$indir
OUT_DIR <- tox21config$outdir
PYTHON_DIR <- tox21config$python
CORES <- tox21config$cores
CLEANUP_TIME <- tox21queue$cleanupTime
DELETE_TIME <- tox21queue$deleteTime
INPUT_MAX <- tox21queue$inputMax
if(INPUT_MAX > 16){ # Tox21 Enricher only supports a max of 16 concurrent input sets and a minimum of 1 set.
    print("Warning: Found an inputMax value exceeding 16 in config.yml. Tox21 Enricher only supports a maximum of 16 sets. INPUT_MAX will be set to 16.")
    INPUT_MAX <- 16
} else if(INPUT_MAX < 1){
    print("Warning: Found an inputMax value less than 1 in config.yml. Tox21 Enricher only supports a minimum of 1 set. INPUT_MAX will be set to 1.")
    INPUT_MAX <- 1
}

# Source Python and define function from Python file
Sys.setenv(RETICULATE_PYTHON=PYTHON_DIR)
use_python(PYTHON_DIR)
source_python("calcReactiveGroups.py")

print(paste0("! Tox21 Enricher Plumber API version ", APP_VERSION, "."))
print("! Ready to accept connections.")

## API endpoints

## SECTION 1: DEALING WITH THE QUEUE

#* Place enrichment request in queue
#* @param mode
#* @param enrichmentUUID
#* @param annoSelectStr
#* @param nodeCutoff
#* @get /queue
queue <- function(mode="", enrichmentUUID="-1", annoSelectStr="MESH=checked,PHARMACTIONLIST=checked,ACTIVITY_CLASS=checked,ADVERSE_EFFECT=checked,INDICATION=checked,KNOWN_TOXICITY=checked,MECH_LEVEL_1=checked,MECH_LEVEL_2=checked,MECH_LEVEL_3=checked,MECHANISM=checked,MODE_CLASS=checked,PRODUCT_CLASS=checked,STRUCTURE_ACTIVITY=checked,TA_LEVEL_1=checked,TA_LEVEL_2=checked,TA_LEVEL_3=checked,THERAPEUTIC_CLASS=checked,TISSUE_TOXICITY=checked,DRUGBANK_ATC=checked,DRUGBANK_ATC_CODE=checked,DRUGBANK_CARRIERS=checked,DRUGBANK_ENZYMES=checked,DRUGBANK_TARGETS=checked,DRUGBANK_TRANSPORTERS=checked,CTD_CHEM2DISEASE=checked,CTD_CHEM2GENE_25=checked,CTD_CHEMICALS_DISEASES=checked,CTD_CHEMICALS_GENES=checked,CTD_CHEMICALS_GOENRICH_CELLCOMP=checked,CTD_CHEMICALS_GOENRICH_MOLFUNCT=checked,CTD_CHEMICALS_PATHWAYS=checked,CTD_GOSLIM_BIOPROCESS=checked,CTD_PATHWAY=checked,HTS_ACTIVE=checked,LEADSCOPE_TOXICITY=checked,MULTICASE_TOX_PREDICTION=checked,TOXCAST_ACTIVE=checked,TOXINS_TARGETS=checked,TOXPRINT_STRUCTURE=checked,TOXREFDB=checked,", nodeCutoff=10, setNames){
    # Connect to db
    poolQueue <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Update database with "queue" entry
    query <- sqlInterpolate(ANSI(), paste0("INSERT INTO queue(mode, uuid, annoselectstr, cutoff) VALUES('", mode, "', '", enrichmentUUID, "', '", annoSelectStr, "', ", nodeCutoff, ") ;"), id="createQueueEntry")
    outp <- dbExecute(poolQueue, query)
    # Update database with corresponding status entries
    setNamesSplit <- unlist(str_split(setNames, "\n"))
    lapply(setNamesSplit, function(x){
        query <- sqlInterpolate(ANSI(), paste0("INSERT INTO status(step, uuid, setname) VALUES(0, '", enrichmentUUID, "', '", x, "') ;"), id="createStatusEntry")
        outp <- dbExecute(poolQueue, query) 
    })
    # Close pool
    poolClose(poolQueue)
}

#* Create entry for request in transaction table
#* @param originalMode
#* @param mode
#* @param uuid
#* @param annoSelectStr
#* @param cutoff
#* @param input
#* @param originalNames
#* @param reenrich
#* @param color
#* @param timestampPosted
#* @get /createTransaction
createTransaction <- function(res, req, originalMode="", mode="", uuid="-1", annoSelectStr="MESH=checked,PHARMACTIONLIST=checked,ACTIVITY_CLASS=checked,ADVERSE_EFFECT=checked,INDICATION=checked,KNOWN_TOXICITY=checked,MECH_LEVEL_1=checked,MECH_LEVEL_2=checked,MECH_LEVEL_3=checked,MECHANISM=checked,MODE_CLASS=checked,PRODUCT_CLASS=checked,STRUCTURE_ACTIVITY=checked,TA_LEVEL_1=checked,TA_LEVEL_2=checked,TA_LEVEL_3=checked,THERAPEUTIC_CLASS=checked,TISSUE_TOXICITY=checked,DRUGBANK_ATC=checked,DRUGBANK_ATC_CODE=checked,DRUGBANK_CARRIERS=checked,DRUGBANK_ENZYMES=checked,DRUGBANK_TARGETS=checked,DRUGBANK_TRANSPORTERS=checked,CTD_CHEM2DISEASE=checked,CTD_CHEM2GENE_25=checked,CTD_CHEMICALS_DISEASES=checked,CTD_CHEMICALS_GENES=checked,CTD_CHEMICALS_GOENRICH_CELLCOMP=checked,CTD_CHEMICALS_GOENRICH_MOLFUNCT=checked,CTD_CHEMICALS_PATHWAYS=checked,CTD_GOSLIM_BIOPROCESS=checked,CTD_PATHWAY=checked,HTS_ACTIVE=checked,LEADSCOPE_TOXICITY=checked,MULTICASE_TOX_PREDICTION=checked,TOXCAST_ACTIVE=checked,TOXINS_TARGETS=checked,TOXPRINT_STRUCTURE=checked,TOXREFDB=checked,", cutoff=10, input, casrnBox, originalNames="none", reenrich="", color, timestampPosted, reenrichFlag=FALSE){
    # format reenrich flag
    if(reenrichFlag == TRUE){
        reenrichFlag <- 1
    } else {
        reenrichFlag <- 0
    }
    # Connect to db
    poolTransaction <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Update database with transaction entry
    query <- sqlInterpolate(ANSI(), paste0("INSERT INTO transaction(original_mode, mode, uuid, annotation_selection_string, cutoff, input, casrn_box, original_names, reenrich, reenrich_flag, colors, timestamp_posted, ip) VALUES('", originalMode, "', '", mode, "', '", uuid, "', '", annoSelectStr, "', '", cutoff, "', '", input, "', '", casrnBox, "', '", originalNames, "', '", reenrich, "', '", reenrichFlag, "', '", color, "', '", timestampPosted, "', '", req$REMOTE_ADDR, "');"), id="createTransactionEntry")
    outp <- dbExecute(poolTransaction, query)
    # Close pool
    poolClose(poolTransaction)
    return(TRUE)
}

#* Load details for the selected transaction
#* @param uuid
#* @get /loadTransaction
loadTransaction <- function(uuid="none"){
    # Connect to db
    poolTransaction <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Update database with transaction entry
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM transaction WHERE uuid='", uuid, "';"), id="loadTransaction")
    outp <- dbGetQuery(poolTransaction, query)
    # Close pool
    poolClose(poolTransaction)
    return(outp)
}

#* Get queue position for given enrichment request
#* @param transactionId
#* @get /getQueuePos
getQueuePos <- function(transactionId="-1", mode="none"){
    # Connect to db
    poolUpdate <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Get status entries for request
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM status WHERE uuid='", transactionId, "';"), id="fetchStatusStep")
    outp <- dbGetQuery(poolUpdate, query)
    statusFiles <- outp[, "step"]
    names(statusFiles) <- outp[, "setname"]
    
    # Check if request has completed w/ errors overall (failed completely)
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE error IS NOT NULL AND uuid='", transactionId, "';"), id="fetchQueuePosition")
    outp <- dbGetQuery(poolUpdate, query)
    if(nrow(outp) > 0) {
        return("<div class=\"text-danger\">Failed.</div>")
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
    poolClose(poolUpdate)
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
                if(statusFileUpdate < 2){
                    if(statusFileUpdate == -1){#error
                        return("Ran into error.")
                    }
                    return(paste0("Waiting in queue position: ", queuePos, "."))
                } else if(statusFileUpdate == 2){
                    return("(Step 1/4): Processing input file(s).")
                } else if(statusFileUpdate == 3){
                    return("(Step 2/4): Creating chart and matrix files.")
                } else if(statusFileUpdate == 4){
                    return("(Step 3/4): Clustering (Step 1/4) - calculating kappa score.")
                } else if(statusFileUpdate == 5){
                    return("(Step 3/4): Clustering (Step 2/4) - creating qualified initial seeding groups.")
                } else if(statusFileUpdate == 6){
                    return("(Step 3/4): Clustering (Step 3/4) - merging qualified seeds.")
                } else if(statusFileUpdate == 7){
                    return("(Step 3/4): Clustering (Step 4/4) - calculating enrichment score.")
                } else if(statusFileUpdate == 8){
                    return("(Step 4/4): Creating .gct files.")
                } 
                return("Loading...")
            })
            names(statusList) <- names(statusFiles)
        } else { # if fetch annotations mode
            statusList <- lapply(statusFiles, function(statusFileUpdate){
                # waiting in queue
                if(statusFileUpdate < 2){
                    if(statusFileUpdate == -1){#error
                        return("Ran into error.")
                    }
                    return(paste0("Waiting in queue position: ", queuePos, "."))
                } else if(statusFileUpdate == 2){
                    return("(Step 1/4): Processing input file(s).")
                } else if(statusFileUpdate == 3){
                    return("(Step 2/4): Fetching annotations.")
                } else if(statusFileUpdate == 4){
                    return("(Step 3/4): Creating annotations list file.")
                } else if(statusFileUpdate == 5){
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

#* Get transaction data for given enrichment request
#* @param transactionId
#* @get /getPrevSessionData
getPrevSessionData <- function(transactionId="-1"){
    # Connect to db
    poolSessionData <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )

    # Get transaction data of request
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM transaction WHERE uuid='", transactionId, "';"), id="fetchTransactionData")
    outp <- dbGetQuery(poolSessionData, query)
    # Close pool
    poolClose(poolSessionData)
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

#* Check if enrichment process has terminated for given request
#* @param transactionId UUID of the request
#* @get /finishedRequest
finishedRequest <- function(res, req, transactionId){
    # Connect to db
    poolQueue <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Get status entries for request
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", transactionId, "';"), id="fetchStatusStep")
    outp <- dbGetQuery(poolQueue, query)
    # Close pool
    poolClose(poolQueue)
    finished <- outp[1, "finished"]
    if(is.na(finished)){
        return(-1)
    }
    if(finished == 1){
        return(1)
    }
    return(0)
}

#* Check if error file exists for given request
#* @param transactionId UUID of the request
#* @get /hasError
hasError <- function(res, req, transactionId){
    # Connect to db
    poolQueue <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Get status entries for request
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", transactionId, "' AND error IS NOT NULL;"), id="fetchError")
    outp <- dbGetQuery(poolQueue, query)
    # Close pool
    poolClose(poolQueue)
    if(nrow(outp) > 0){
        return(outp[1, "error"])
    }
    return(FALSE)
}

#* Cancel enrichment process for given UUID
#* @param transactionId UUID of the request
#* @get /cancelEnrichment
cancelEnrichment <- function(res, req, transactionId){
    # Connect to db
    poolCancel <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Update database to show that request was canceled
    query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET cancel=1, finished=1 WHERE uuid='", transactionId, "';"), id="cancelEnrichment")
    outp <- dbExecute(poolCancel, query)
    query <- sqlInterpolate(ANSI(), paste0("UPDATE transaction SET cancel=1 WHERE uuid='", transactionId, "';"), id="cancelEnrichment")
    outp <- dbExecute(poolCancel, query)
    # Close pool
    poolClose(poolCancel)
    return(TRUE)
}

#* Check if a given request has been cancelled for given UUID
#* @param transactionId UUID of the request
#* @get /isCancel
isCancel <- function(res, req, transactionId){
    # Connect to db
    poolCancel <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Update database to show that request was canceled
    query <- sqlInterpolate(ANSI(), paste0("SELECT cancel FROM transaction WHERE uuid='", transactionId, "';"), id="isCancel")
    outp <- dbGetQuery(poolCancel, query)
    # Close pool
    poolClose(poolCancel)
    cancelStatus <- 1
    if(nrow(outp) == 1){
        cancelStatus <- outp[1, "cancel"]
    }
    return(cancelStatus)
}

## SECTION 2: REQUIRED BY CLIENT APPLICATION INITIALIZATION

#* Ping API
#* @get /ping
ping <- function(res, req){
    return(TRUE)
}

#* Get delete time for old transaction
#* @get /deleteTime
deleteTime <- function(res, req){
    return(DELETE_TIME)
}

#* Calculate cookie expiry date for previous transaction and fetch additional request information
#* @get /getAdditionalRequestInfo
getAdditionalRequestInfo <- function(res, req, transactionId=""){
    # Connect to db
    poolExp <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    expQuery <- sqlInterpolate(ANSI(), paste0("SELECT original_mode, mode, cutoff, casrn_box, timestamp_posted, timestamp_started, timestamp_finished FROM transaction WHERE uuid='", transactionId, "';"), id="getAnnotationClasses")
    expOutp <- dbGetQuery(poolExp, expQuery)
    # Close pool
    poolClose(poolExp)
    # Calculate cookie expiry date and add to DF
    expDate <- as.POSIXct(expOutp[, "timestamp_posted"], format="%Y-%m-%d %H:%M:%S", tz="UTC") + (60 * 60 * CLEANUP_TIME)
    fullRequestInfo <- expOutp
    fullRequestInfo["expiry_date"] <- expDate
    return(fullRequestInfo)
}

#* Get cleanup time for old transaction. This number is the number of hours that a transaction will live for on the server. This is used when saving cookie expiry dates on the client as well as the cutoff time to delete in the queue cleanup script.
#* @get /cleanupTime
cleanupTime <- function(res, req){
    return(CLEANUP_TIME)
}

#* Get the maximum number of simultaneous input sets that may be submitted in a request.
#* @get /getInputMax
getInputMax <- function(res, req){
    return(INPUT_MAX)
}

#* Check Tox21 Enricher version
#* @get /getAppVersion
getAppVersion <- function(res, req){
    return(APP_VERSION)
}

#* Get annotation classes/types (internal use only)
#* @get /initAnnotations
initAnnotations <- function(res, req){
    # Connect to db
    poolAnnotations <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21config$database,
        host=tox21config$host,
        user=tox21config$uid,
        password=tox21config$pwd,
        port=tox21config$port,
        idleTimeout=3600000
    )
    annoClassQuery <- sqlInterpolate(ANSI(), "SELECT annoclassname, annotype, annodesc, numberoftermids FROM annotation_class;", id="getAnnotationClasses")
    annoClassOutp <- dbGetQuery(poolAnnotations, annoClassQuery)
    rownames(annoClassOutp) <- seq_len(nrow(annoClassOutp))
    # Close pool
    poolClose(poolAnnotations)
    return(annoClassOutp)
}

#* Get total number of requests (internal use only)
#* @get /total
total <- function(res, req){
    # Connect to db
    poolTotal <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    totalQuery <- sqlInterpolate(ANSI(), "SELECT uuid, timestamp_started, timestamp_finished FROM transaction WHERE cancel=0;", id="getTotalEnrichment")
    totalOutp <- dbGetQuery(poolTotal, totalQuery)
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
    # Close pool
    poolClose(poolTotal)
    return(monthTotal)
}

## SECTION 3: PREPARING CHEMICAL DATA FOR ENRICHMENT

#* Get data for provided SMILES string (substructure) (internal use only)
#* @param input Input string, either SMILES or CASRN (if re-enriching)
#* @param reenrich Boolean value to let the API know if this is a re-enrichment or not
#* @get /substructure
substructure <- function(res, req, input, reenrich=FALSE){
    # Connect to db
    poolSubstructure <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21config$database,
        host=tox21config$host,
        user=tox21config$uid,
        password=tox21config$pwd,
        port=tox21config$port,
        idleTimeout=3600000
    )
    # Sanitize input, convert InChI strings to SMILES
    if (grepl("InChI=", input, fixed=TRUE)) {
        input <- convertInchi(inchi=input)
    }
    substructureQuery <- ""
    substructureOutp <- NULL
    if(reenrich == FALSE){
        substructureQuery <- sqlInterpolate(ANSI(), paste0("SELECT * FROM mols_2 WHERE m @> CAST('", input, "' AS mol);"), id="substructureResults")
    } else {
        substructureQuery <- sqlInterpolate(ANSI(), paste0("SELECT * FROM mols_2 WHERE casrn='", input, "';"), id="substructureResults")
    }
    trySubstructure <- tryCatch({
        substructureOutp <- dbGetQuery(poolSubstructure, substructureQuery)
        substructureOutp$m <- unlist(lapply(substructureOutp$m, function(x) paste0(x))) # coerce mol column to string. If left as "pq_mol" data type, R wont' know how to send this in a json back to the client.
        TRUE
    }, error=function(cond){
        return(FALSE)
    })
    if(!trySubstructure){ # if error occurs when converting to mol
        return(list())
    }
    # Close pool
    poolClose(poolSubstructure)
    return(substructureOutp)
}

#* Get data for provided SMILES string (similarity) (internal use only)
#* @param input SMILES Input string
#* @param threshold Tanimoto similarity threshold
#* @get /similarity
similarity <- function(res, req, input="", threshold=0.5){
    # Connect to db
    poolSimilarity <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21config$database,
        host=tox21config$host,
        user=tox21config$uid,
        password=tox21config$pwd,
        port=tox21config$port,
        idleTimeout=3600000
    )
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
        TRUE
    }, error=function(cond){
        return(FALSE)
    })
    if(!trySimilarity){ # if error occurs when converting to mol
        return(list())
    }
    # Close pool
    poolClose(poolSimilarity)
    return(similarityOutp)
}

#* Get additional data for provided CASRN (internal use only)
#* @param input CASRN Input string
#* @get /casrnData
casrnData <- function(res, req, input){
    # Connect to db
    poolCasrn <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21config$database,
        host=tox21config$host,
        user=tox21config$uid,
        password=tox21config$pwd,
        port=tox21config$port,
        idleTimeout=3600000
    )
    casrnQuery <- sqlInterpolate(ANSI(), paste0("SELECT iupac_name, smiles, dtxsid, dtxrid, mol_formula, mol_weight, inchis, inchikey, cid, testsubstance_chemname FROM chemical_detail WHERE CASRN LIKE '", input, "';"), id="casrnResults")
    casrnOutp <- dbGetQuery(poolCasrn, casrnQuery)
    # Close pool
    poolClose(poolCasrn)
    return(casrnOutp)
}

#* Detect if submitted chemical contains any reactive groups (internal use only)
#* @param input
#* @get /reactiveGroups
reactiveGroups <- function(res, req, input){
    return(calcReactiveGroups(input))
}

#* Convert provided InChI string to SMILES (internal use only)
#* @param inchi
#* @get /inchiToSmiles
inchiToSmiles <- function(res, req, inchi){
    # Connect to db
    poolInchi <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21config$database,
        host=tox21config$host,
        user=tox21config$uid,
        password=tox21config$pwd,
        port=tox21config$port,
        idleTimeout=3600000
    )
    inchiQuery <- sqlInterpolate(ANSI(), paste0("SELECT smiles FROM chemical_detail WHERE inchis='", inchi, "';"), id="convertInchi")
    inchiOutp <- dbGetQuery(poolInchi, inchiQuery)
    # Close pool
    poolClose(poolInchi)
    return(inchiOutp[[1]])
}

# Same as above, but done from within the internal Tox21 Enricher substructure/similarity searches. we don't want to do this asynchronously because it is just a function and not an API endpoint.
convertInchi <- function(res, req, inchi){
    # Connect to db
    poolInchi <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21config$database,
        host=tox21config$host,
        user=tox21config$uid,
        password=tox21config$pwd,
        port=tox21config$port,
        idleTimeout=3600000
    )
    inchiQuery <- sqlInterpolate(ANSI(), paste0("SELECT smiles FROM chemical_detail WHERE inchis='", inchi, "';"), id="convertInchi")
    inchiOutp <- dbGetQuery(poolInchi, inchiQuery)
    # Close pool
    poolClose(poolInchi)
    return(inchiOutp[[1]])
}

#* Use RDKit to generate chemical structure images from a list of molecules (internal use only)
#* @param input
#* @get /generateStructures
generateStructures <- function(res, req, input){
    # Connect to db
    poolSvg <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21config$database,
        host=tox21config$host,
        user=tox21config$uid,
        password=tox21config$pwd,
        port=tox21config$port,
        idleTimeout=3600000
    )
    tmpSplit <- unlist(str_split(input, "\n"))
    structures <- lapply(tmpSplit, function(x){
        tmpSplit2 <- unlist(str_split(x, "__"))
        m <- tmpSplit2[2]
        svgQuery <- sqlInterpolate(ANSI(), paste0("SELECT mol_to_svg('", m, "');"), id="generateStructures")
        svgOutp <- dbGetQuery(poolSvg, svgQuery)
        return(svgOutp[1, "mol_to_svg"])
    })
    names(structures) <- lapply(tmpSplit, function(x){
        tmpSplit2 <- unlist(str_split(x, "__"))
        return(tmpSplit2[1])
    })
    # Close pool
    poolClose(poolSvg)
    return(structures)
}

#* Get a list of CASRNs with reactive structure warnings form database.
#* @param input
#* @get /removeWithWarnings
removeWithWarnings <- function(res, req, input){
    # Connect to db
    poolWarn <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21config$database,
        host=tox21config$host,
        user=tox21config$uid,
        password=tox21config$pwd,
        port=tox21config$port,
        idleTimeout=3600000
    )
    inputSets <- unlist(str_split(input, "\\|"))
    inputSets <- unique(unlist(lapply(inputSets, function(x) unlist(str_split(x, "__"))[1])))
    warnQuery <- sqlInterpolate(ANSI(), paste0("SELECT casrn, cyanide, isocyanate, aldehyde, epoxide FROM mols_2 WHERE ", paste0("casrn='", inputSets, "'", collapse=" OR "), ";"), id="getWarnings")
    warnOutp <- dbGetQuery(poolWarn, warnQuery)
    # Close pool
    poolClose(poolWarn)
    return(warnOutp)
}

## SECTION 4: FILE CREATION AND SERVING

#* Serve text file to client
#* @serializer contentType list(type="application/text")
#* @param transactionId
#* @param filename
#* @param subDir
#* @get /serveFileText
serveFileText <- function(res, req, transactionId, filename, subDir){
    outDir <- paste0(APP_DIR, subDir, "/", transactionId, "/")
    fileToServe <- paste0(outDir, filename)
    readBin(fileToServe, "raw", n=file.info(fileToServe)$size)
}

#* Serve manual to client
#* @serializer contentType list(type="application/pdf")
#* @get /serveManual
serveManual <- function(res, req){
    filename <- paste0(APP_DIR, "docs/Tox21Enricher_Manual_v", APP_VERSION, ".pdf")
    fileToServe <- paste0(APP_DIR, "docs/Tox21Enricher_Manual_v", APP_VERSION, ".pdf")
    readBin(fileToServe, "raw", n=file.info(fileToServe)$size)
}

#* Check if randomly-generated UUID already exists. This should be extremely rare, but not impossible
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

#* Create input files on filesystem
#* @param transactionId UUID for given request
#* @param enrichmentSets User's input sets
#* @param setNames Input set names
#* @get /createInput
createInput <- function(res, req, transactionId, enrichmentSets, setNames, mode, nodeCutoff, annoSelectStr){
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
    poolInput <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21config$database,
        host=tox21config$host,
        user=tox21config$uid,
        password=tox21config$pwd,
        port=tox21config$port,
        idleTimeout=3600000
    )
    # Create input set txt for enrichment
    lapply(names(enrichmentSets), function(i) {
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
    })
    # Close pool
    poolClose(poolInput)
    return(TRUE)
}

#* Returns list of sets that are valid/existing for a given transaction
#* @param transactionId UUID of the request
#* @get /checkSets
checkSets <- function(res, req, transactionId){
    inputDir <- paste0(APP_DIR, IN_DIR)
    inputFilesList <- Sys.glob(paste0(inputDir, transactionId, "/*.txt"))
    inputFilesList <- unlist(lapply(inputFilesList, function(x){
        x_lv1 <- gsub(paste0(inputDir, transactionId, "/"), "", x)
        x_lv2 <- gsub(".txt", "", x_lv1)
    }))
    return(inputFilesList)
}

#* Check if result files exist in Input/Output directories for given request
#* @param transactionId UUID of the request
#* @get /exists
exists <- function(res, req, transactionId="none"){
    # Check if DB records exist
    # Connect to db
    poolExists <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Update database with transaction entry
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM transaction WHERE uuid='", transactionId, "';"), id="loadTransaction")
    outp <- dbGetQuery(poolExists, query)
    # Close pool
    poolClose(poolExists)
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

#* Get results for given directory
#* @param transactionId UUID of the request
#* @setName if supplied, is the set name to fetch from
#* @get /getResults
getResults <- function(res, req, transactionId, setName="###"){
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

#* Read gct files for a given request
#* @param transactionId UUID of the request
#* @param cutoff Given node cutoff value for the request
#* @param mode Chart or Cluster
#* @get /readGct
readGct <- function(res, req, transactionId, cutoff, mode, set="Set1"){
    outDir <- paste0(APP_DIR, OUT_DIR, transactionId, "/")
    gctFile <- NULL
    if(mode == "chart"){
        if(!file.exists(paste0(outDir, "gct/Chart_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.gct"))){
            return(NULL)
        }
        tryReadChart <- tryCatch({
            gctFile <- read.delim(paste0(outDir, "gct/Chart_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.gct"), skip=2, header=TRUE, sep="\t", row.names=1, comment.char="", fill=FALSE, colClasses=c("Terms"="NULL"), check.names=FALSE)
            TRUE
        }, error=function(cond){
            return(NULL)
        })
        if(!tryReadChart){
            return(NULL)
        }
    } else if(mode == "cluster"){
        if(!file.exists(paste0(outDir, "gct/Cluster_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.gct"))){
            return(NULL)
        }
        tryReadCluster <- tryCatch({
            gctFile <- read.delim(paste0(outDir, "gct/Cluster_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.gct"), skip=2, header=TRUE, sep="\t", row.names=1, comment.char="", fill=FALSE, colClasses=c("Terms"="NULL"), check.names=FALSE)
            TRUE
        }, error=function(cond){
            return(NULL)
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
            return(NULL)
        })
        if(!tryReadChart){
            return(NULL)
        }
    }
    return(gctFile)
}

#* Return Chart Simple file for given request and input set
#* @param transactionId UUID of the request
#* @param enrichmentSets names of the enrichment sets
#* @get /bargraph
bargraph <- function(res, req, transactionId, enrichmentSets){
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
                return(list())
            })
            if(!tryReadFile){
                return(list())
            }
            # Reformat P-Values to character vectors so they don't get truncated when being converted to json upon response
            bgChartTmp["PValue"] <- lapply(bgChartTmp["PValue"], function(x) as.character(x))
            return(bgChartTmp)
        }
        return(list())
    })
    return(bgChart)
}

#* Generate interactive visNetwork for given request and input set
#* @param transactionId UUID of the request
#* @param cutoff Node cutoff
#* @param mode Chart or cluster
#* @param input Current input set
#* @get /generateNetwork
generateNetwork <- function(res, req, transactionId="-1", cutoff, mode, input, qval){
    input <- unlist(str_split(input, "#"))
    baseDirName <- paste0(APP_DIR, OUT_DIR, transactionId, "/")
    chartForNetFile <- NULL
    # First, read files from /gct/ directory
    if(mode == "chart") {
        if(!file.exists(paste0(baseDirName, "/gct/Chart_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.ForNet"))){
            return(NULL)
        }
        tryReadChart <- tryCatch({
            chartForNetFile <- read.delim(paste0(baseDirName, "/gct/Chart_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.ForNet"), sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, fill=TRUE)
            TRUE
        }, error=function(cond){
            return(NULL)
        })
        if(!tryReadChart){
            return(NULL)
        }
    } else { # cluster
        if(!file.exists(paste0(baseDirName, "/gct/Cluster_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.ForNet"))){
            return(NULL)
        }
        tryReadChart <- tryCatch({
            chartForNetFile <- read.delim(paste0(baseDirName, "/gct/Cluster_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.ForNet"), sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, fill=TRUE)
            TRUE
        }, error=function(cond){
            return(NULL)
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
    # If no UIDs, return empty list to trigger error handline on client
    if(length(chartNetworkUIDs) < 1) {
        return(list())
    }
    # Create placeholder string for querying database
    termsStringPlaceholder <- paste0(chartNetworkUIDs, collapse=",")
    # Connect to db
    poolNetwork <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21config$database,
        host=tox21config$host,
        user=tox21config$uid,
        password=tox21config$pwd,
        port=tox21config$port,
        idleTimeout=3600000
    )
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
        WHERE p.term1uid IN (", termsStringPlaceholder, ") AND p.term2uid IN (", termsStringPlaceholder, ") AND p.qvalue <", qval, ";"
    ), id="addToDb")
    outpNetwork <- dbGetQuery(poolNetwork, queryNetwork)
    # Close pool
    poolClose(poolNetwork)
    return(outpNetwork)
}

#* Get lists of chemicals that are associated with the annotations displayed in the nodes in the network (internal use only)
#* @param termFrom  
#* @param termTo
#* @param classFrom
#* @param classTo
#* @get /getNodeChemicals
getNodeChemicals <- function(res, req, termFrom, termTo, classFrom, classTo){
    # Connect to db
    poolNode <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21config$database,
        host=tox21config$host,
        user=tox21config$uid,
        password=tox21config$pwd,
        port=tox21config$port,
        idleTimeout=3600000
    )
    # Get internal ID number of From annotation
    nodeQuery <- sqlInterpolate(ANSI(), paste0( "SELECT id FROM annotation_matrix_terms WHERE term='", classFrom, "__", termFrom, "';" ), id="getFromID")
    nodeOutp <- dbGetQuery(poolNode, nodeQuery)
    fromID <- nodeOutp
    # Get internal ID number of To annotation
    nodeQuery <- sqlInterpolate(ANSI(), paste0( "SELECT id FROM annotation_matrix_terms WHERE term='", classTo, "__", termTo, "';" ), id="getToID")
    nodeOutp <- dbGetQuery(poolNode, nodeQuery)
    toID <- nodeOutp
    # Get list of casrns associated with From annotation
    nodeQuery <- sqlInterpolate(ANSI(), paste0( "SELECT casrn FROM annotation_matrix WHERE annotation LIKE '%", fromID, "%';" ), id="getCasrnsFrom")
    nodeOutp <- dbGetQuery(poolNode, nodeQuery)
    casrnsFrom <- nodeOutp
    # Get list of casrns associated with To annotation
    nodeQuery <- sqlInterpolate(ANSI(), paste0( "SELECT casrn FROM annotation_matrix WHERE annotation LIKE '%", toID, "%';" ), id="getCasrnsTo")
    nodeOutp <- dbGetQuery(poolNode, nodeQuery)
    casrnsTo <- nodeOutp
    # Close pool
    poolClose(poolNode)
    return(list(casrnsFrom=casrnsFrom[, "casrn"], casrnsTo=casrnsTo[, "casrn"]))
}

#* Get link from database to view additional info for a selected node in the network (internal use only)
#* @param term
#* @param class
#* @get /getNodeDetails
getNodeDetails <- function(res, req, class){
    # Connect to db
    poolNode <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21config$database,
        host=tox21config$host,
        user=tox21config$uid,
        password=tox21config$pwd,
        port=tox21config$port,
        idleTimeout=3600000
    )
    # Get annotation detail link
    nodeQuery <- sqlInterpolate(ANSI(), paste0( "SELECT baseurl FROM annotation_class WHERE annoclassname='", class, "';" ), id="getNodeDetailsFromClass")
    nodeOutp <- dbGetQuery(poolNode, nodeQuery)
    baseurl <- nodeOutp
    # Close pool
    poolClose(poolNode)
    return(baseurl)
}

#* Get colors from database to display nodes in the network (internal use only)
#* @get /getNodeColors
getNodeColors <- function(res, req){
    # Connect to db
    poolNode <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21config$database,
        host=tox21config$host,
        user=tox21config$uid,
        password=tox21config$pwd,
        port=tox21config$port,
        idleTimeout=3600000
    )
    # Get annotation detail link
    nodeQuery <- sqlInterpolate(ANSI(), paste0( "SELECT annoclassname, networkcolor FROM annotation_class;" ), id="getNodeColors")
    nodeOutp <- dbGetQuery(poolNode, nodeQuery)
    nodeColors <- nodeOutp
    # Close pool
    poolClose(poolNode)
    return(nodeColors)
}

## SECTION 6: API CLIENT ENDPOINTS FOR OPERATING IN NO-GUI MODE

#* Get list of all annotations in the database
#* @get /annotationList
retrieveAnnotations <- function(){
    pool <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21config$database,
        host=tox21config$host,
        user=tox21config$uid,
        password=tox21config$pwd,
        port=tox21config$port,
        idleTimeout=3600000
    )
    query <- sqlInterpolate(ANSI(), paste0("SELECT annoclassname FROM annotation_class;"))
    outp <- dbGetQuery(pool, query)
    # Close pool
    poolClose(pool)
    return(outp[, "annoclassname"])
}

#* Post input set for enrichment analysis
#* @param mode The mode to use when performing enrichment analysis. casrn=CASRN input. substructure=SMILES or InChI input. similarity=SMILES or InChI input. annotation=view annotations.
#* @param input Comma-separated string of chemicals for this enrichment process.
#* @param annotations Comma-separated string of all enabled annotations for this enrichment process.
#* @param cutoff Node cutoff value - integers only between 1-50.
#* @param tanimoto Tanimoto threshold for Similarity search (default 0.5).
#* @post /submit
submit <- function(mode="", input="", annotations="MESH,PHARMACTIONLIST,ACTIVITY_CLASS,ADVERSE_EFFECT,INDICATION,KNOWN_TOXICITY,MECH_LEVEL_1,MECH_LEVEL_2,MECH_LEVEL_3,MECHANISM,MODE_CLASS,PRODUCT_CLASS,STRUCTURE_ACTIVITY,TA_LEVEL_1,TA_LEVEL_2,TA_LEVEL_3,THERAPEUTIC_CLASS,TISSUE_TOXICITY,DRUGBANK_ATC,DRUGBANK_ATC_CODE,DRUGBANK_CARRIERS,DRUGBANK_ENZYMES,DRUGBANK_TARGETS,DRUGBANK_TRANSPORTERS,CTD_CHEM2DISEASE,CTD_CHEM2GENE_25,CTD_CHEMICALS_DISEASES,CTD_CHEMICALS_GENES,CTD_CHEMICALS_GOENRICH_CELLCOMP,CTD_CHEMICALS_GOENRICH_MOLFUNCT,CTD_CHEMICALS_PATHWAYS,CTD_GOSLIM_BIOPROCESS,CTD_PATHWAY,HTS_ACTIVE,LEADSCOPE_TOXICITY,MULTICASE_TOX_PREDICTION,TOXCAST_ACTIVE,TOXINS_TARGETS,TOXPRINT_STRUCTURE,TOXREFDB", cutoff=10, tanimoto=0.5) {
    # TODO: Check if arguments are bad
    # Check if mode is missing
    if(is.null(mode) | mode == ""){
        return("Error: No mode specified. ('casrn', 'substructure', 'similarity', or 'annotation')")
    }
    # Check if input is missing
    if(is.null(input) | input == ""){
        return("Error: No input supplied.")
    }
    # Check if cutoff is not a number
    if(!is.numeric(cutoff)){
        return("Error: Cutoff value is not a number.")
    }
    # Check if cutoff is not an integer
    if(cutoff%%1 != 0){
        return("Error: Cutoff value is not an integer.")
    }
    # Check if cutoff is out of range
    if(cutoff < 1 | cutoff > 50){
        return("Error: Cutoff value must be between 1 and 50 inclusive.")
    }
    # Check if Tanimoto is not a number
    if(!is.numeric(tanimoto)){
        return("Error: Tanimoto threshold is not a number.")
    }
    # Check if Tanimoto is out of range
    if(tanimoto < 0.01 | tanimoto > 1.00){
        return("Error: Tanimoto threshold must be between 0.01 and 1.00 inclusive.")
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
    } else {
        # Open pool for PostgreSQL
        poolTanimoto <- dbPool(
            drv=RPostgres::Postgres(),
            dbname=tox21config$database,
            host=tox21config$host,
            user=tox21config$uid,
            password=tox21config$pwd,
            port=tox21config$port,
            idleTimeout=3600000
        )
        queryTanimoto <- sqlInterpolate(ANSI(), paste0("set rdkit.tanimoto_threshold=", tanimoto, ";"))
        outpTanimoto <- dbExecute(poolTanimoto, queryTanimoto)
        # Close pool
        poolClose(poolTanimoto)
    }
    # Get list of all annotation classes in database
    annotationList <- retrieveAnnotations()
    # Set annotations to default if missing
    if(annotations == ""){
        annotations <- paste0(annotationList, collapse=",")
    }
    # Check if all annotations are valid
    errorAnnotationClasses <- lapply(unlist(str_split(annotations, ",")), function(x){
        if(!(x %in% annotationList)){
            return(paste0("Error: Invalid annotation class: ", x))
        }
        return(NULL)
    })
    errorAnnotationClasses <- errorAnnotationClasses[!vapply(errorAnnotationClasses, is.null, FUN.VALUE=logical(1))]
    if(length(errorAnnotationClasses) > 0){
        return(paste0(errorAnnotationClasses, collapse=", "))
    }
    
    # Put annotations into correct form (annotation selection string with =checked)
    annotations <- gsub(",", "=checked,", annotations)
    # Open main pool
    pool <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21config$database,
        host=tox21config$host,
        user=tox21config$uid,
        password=tox21config$pwd,
        port=tox21config$port,
        idleTimeout=3600000
    )
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
        if(!grepl("^#[A-Za-z0-9]+|[0-9]+-[0-9]+-[0-9]+", casrnValidatedInput[i], ignore.case=TRUE)) {
            return(i)
        }
        return(NULL)
    })
    errorCasrns <- errorCasrns[!vapply(errorCasrns, is.null, FUN.VALUE=logical(1))]
    
    # If there are errors
    if(length(errorCasrns) > 0){
        # Close DB connection
        poolClose(pool)
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
        poolClose(pool)
        return(paste0("Error: It appears you are using set names but have not provided a name for the first input set. Please check your input and try again."))
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
    setNames <- lapply(setNames, function(x){
        return(gsub("#", "", x))
    })
    setNames <- paste0(setNames, collapse="\n")
    # Create input file in queue
    createInput(transactionId=transactionId, enrichmentSets=casrnValidatedInput, setNames=setNames, mode=mode, nodeCutoff=cutoff, annoSelectStr=annotations)
    queue(mode=mode, enrichmentUUID=transactionId, annoSelectStr=annotations, nodeCutoff=cutoff, setNames=setNames)
    # Close DB connection
    poolClose(pool)
    return(transactionId)
}

#* Download enrichment results for a given uuid
#* @serializer contentType list(type="application/zip")
#* @param id The UUID of the enrichment process to download.
#* @get /download
function(id="-1", res) {
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

#* Download individual enrichment result files for a given uuid
#* @serializer contentType list(type="application/zip")
#* @param id The UUID of the enrichment process to download.
#* @param filename The name of the file to download.
#* @get /downloadResultFile
function(id="-1", filename="", res) {
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

#* Check if enrichment results exist for a given uuid
#* @param id The UUID of the enrichment process to check.
#* @get /completed
function(id="-1", res) {
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


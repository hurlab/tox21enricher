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
library(future)
library(ggplot2)
library(httr)
library(parallel)
library(plyr)
library(pool)
library(promises)
library(reticulate)
library(rjson)
library(RPostgreSQL)
library(stringr)
library(tidyverse)
library(uuid)

## Code to run on startup, not part of API endpoints

# Load params from config file
tox21config <- config::get("tox21enricher")
tox21queue <- config::get("tox21enricher-queue")
APP_VERSION <- tox21config$appversion
APP_DIR <- tox21config$appdir
PYTHON_DIR <- tox21config$python
CORES <- tox21config$cores

# Source Python
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
        drv=dbDriver("PostgreSQL", max.con=100),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Update database with "queue" entry
    query <- sqlInterpolate(ANSI(), paste0("INSERT INTO queue(mode, uuid, annoselectstr, cutoff) VALUES('", mode, "', '", enrichmentUUID, "', '", annoSelectStr, "', ", nodeCutoff, ") ;"), id="createQueueEntry")
    outp <- dbGetQuery(poolQueue, query)
    # Update database with corresponding status entries
    setNamesSplit <- unlist(str_split(setNames, "\n"))
    lapply(setNamesSplit, function(x){
        query <- sqlInterpolate(ANSI(), paste0("INSERT INTO status(step, uuid, setname) VALUES(0, '", enrichmentUUID, "', '", x, "') ;"), id="createStatusEntry")
        outp <- dbGetQuery(poolQueue, query) 
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
#* @param apikey
#* @get /createTransaction
createTransaction <- function(originalMode="", mode="", uuid="-1", annoSelectStr="MESH=checked,PHARMACTIONLIST=checked,ACTIVITY_CLASS=checked,ADVERSE_EFFECT=checked,INDICATION=checked,KNOWN_TOXICITY=checked,MECH_LEVEL_1=checked,MECH_LEVEL_2=checked,MECH_LEVEL_3=checked,MECHANISM=checked,MODE_CLASS=checked,PRODUCT_CLASS=checked,STRUCTURE_ACTIVITY=checked,TA_LEVEL_1=checked,TA_LEVEL_2=checked,TA_LEVEL_3=checked,THERAPEUTIC_CLASS=checked,TISSUE_TOXICITY=checked,DRUGBANK_ATC=checked,DRUGBANK_ATC_CODE=checked,DRUGBANK_CARRIERS=checked,DRUGBANK_ENZYMES=checked,DRUGBANK_TARGETS=checked,DRUGBANK_TRANSPORTERS=checked,CTD_CHEM2DISEASE=checked,CTD_CHEM2GENE_25=checked,CTD_CHEMICALS_DISEASES=checked,CTD_CHEMICALS_GENES=checked,CTD_CHEMICALS_GOENRICH_CELLCOMP=checked,CTD_CHEMICALS_GOENRICH_MOLFUNCT=checked,CTD_CHEMICALS_PATHWAYS=checked,CTD_GOSLIM_BIOPROCESS=checked,CTD_PATHWAY=checked,HTS_ACTIVE=checked,LEADSCOPE_TOXICITY=checked,MULTICASE_TOX_PREDICTION=checked,TOXCAST_ACTIVE=checked,TOXINS_TARGETS=checked,TOXPRINT_STRUCTURE=checked,TOXREFDB=checked,", cutoff=10, input, originalNames, reenrich="", color, timestampPosted, apikey){
    # Connect to db
    poolTransaction <- dbPool(
        drv=dbDriver("PostgreSQL", max.con=100),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Update database with transaction entry
    query <- sqlInterpolate(ANSI(), paste0("INSERT INTO transaction(original_mode, mode, uuid, annotation_selection_string, cutoff, input, original_names, reenrich, colors, timestamp_posted, delete, key) VALUES('", originalMode, "', '", mode, "', '", uuid, "', '", annoSelectStr, "', '", cutoff, "', '", input, "', '", originalNames, "', '", reenrich, "', '", color, "', '", timestampPosted, "', 0, '", apikey, "');"), id="createTransactionEntry")
    outp <- dbGetQuery(poolTransaction, query)
    # Close pool
    poolClose(poolTransaction)
    return(TRUE)
}

#* Get all transactions for a given key
#* @param apikey
#* @get /getTransactions
getTransactions <- function(apikey){
    # Connect to db
    poolTransaction <- dbPool(
        drv=dbDriver("PostgreSQL", max.con=100),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Update database with transaction entry
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM transaction WHERE key='", apikey, "' AND delete=0"), id="getTransactions")
    outp <- dbGetQuery(poolTransaction, query)
    # Close pool
    poolClose(poolTransaction)
    return(outp)
}

#* Load details for the selected transaction
#* @param apikey
#* @param uuid
#* @get /loadTransaction
loadTransaction <- function(apikey, uuid){
    # Connect to db
    poolTransaction <- dbPool(
        drv=dbDriver("PostgreSQL", max.con=100),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Update database with transaction entry
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM transaction WHERE key='", apikey, "' AND uuid='", uuid, "'"), id="loadTransaction")
    outp <- dbGetQuery(poolTransaction, query)
    # Close pool
    poolClose(poolTransaction)
    return(outp)
}

#* Mark selected transactions for deletion
#* @param selected
#* @get /deleteTransactionSelected
deleteTransactionSelected <- function(selected){
    # Connect to db
    poolTransaction <- dbPool(
        drv=dbDriver("PostgreSQL", max.con=100),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    selectedSets <- unlist(str_split(selected, ","))
    # Mark each set for deletion
    lapply(selectedSets, function(x){
        # Update database with transaction entry
        query <- sqlInterpolate(ANSI(), paste0("UPDATE transaction SET delete=1 WHERE uuid='", x, "'"), id="markDelete")
        outp <- dbGetQuery(poolTransaction, query)
    })
    # Close pool
    poolClose(poolTransaction)
    return(TRUE)
}

#* Get queue position for given enrichment request
#* @param transactionId
#* @get /getQueuePos
getQueuePos <- function(transactionId="-1", mode="none"){
    # Connect to db
    poolUpdate <- dbPool(
        drv=dbDriver("PostgreSQL", max.con=100),
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
    # Get queue position of request
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE finished=0;"), id="fetchQueuePosition")
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
                    return("(Step 3/4): Clustering (Step 3/4) - merging qualiied seeds.")
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
        }
        if(length(statusList) > 0){
            statusListToReturn <- lapply(seq_len(length(statusList)), function(i){
                return(paste0(names(statusList)[i], ": \t", statusList[i]))
            })
            statusListToReturn <- paste0(statusListToReturn, collapse="\n")
            return(statusListToReturn) 
        } else {
            return("Complete!")
        }
    }
    return("Complete!")
}

#* Check if enrichment process has terminated for given request
#* @param transactionId UUID of the request
#* @get /finishedRequest
finishedRequest <- function(res, req, transactionId){
    # Connect to db
    poolQueue <- dbPool(
        drv=dbDriver("PostgreSQL", max.con=100),
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
    if(finished == 1){
        return(TRUE)
    }
    return(FALSE)
}

#* Check if error file exists for given request
#* @param transactionId UUID of the request
#* @get /hasError
hasError <- function(res, req, transactionId){
    # Connect to db
    poolQueue <- dbPool(
        drv=dbDriver("PostgreSQL", max.con=100),
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
        drv=dbDriver("PostgreSQL", max.con=100),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Update database to show that request was canceled
    query <- sqlInterpolate(ANSI(), paste0("UPDATE enrichment_list SET timestamp_finish='cancelled' WHERE id='", transactionId, "';"), id="cancelEnrichment")
    outp <- dbGetQuery(poolCancel, query)
    query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET cancel=1 WHERE uuid='", transactionId, "';"), id="cancelEnrichment")
    outp <- dbGetQuery(poolCancel, query)
    # Close pool
    poolClose(poolCancel)
    return(TRUE)
}

#* Get timestamps for given enrichment
#* @param transactionId
#* @get /getTimestamp
getTimestamp <- function(res, req, transactionId="-1"){
    # Connect to db
    poolUpdate <- dbPool(
        drv=dbDriver("PostgreSQL", max.con=100),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # update database with ending timestamp for enrichment
    query <- sqlInterpolate(ANSI(), paste0("SELECT timestamp_start, timestamp_finish FROM enrichment_list WHERE id='", transactionId, "';"), id="getFromDb")
    outp <- dbGetQuery(poolUpdate, query)
    # Close pool
    poolClose(poolUpdate)
    return(outp)
}

## SECTION 2: REQUIRED BY CLIENT APPLICATION INITIALIZATION

#* Ping API
#* @get /ping
ping <- function(res, req){
    return(TRUE)
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
        drv=dbDriver("PostgreSQL", max.con=100),
        dbname=tox21config$database,
        host=tox21config$host,
        user=tox21config$uid,
        password=tox21config$pwd,
        port=tox21config$port,
        idleTimeout=3600000
    )
    annoClassQuery <- sqlInterpolate(ANSI(), "SELECT annoclassname, annotype, annodesc FROM annotation_class;", id="getAnnotationClasses")
    annoClassOutp <- dbGetQuery(poolAnnotations, annoClassQuery)
    # Phase out old CTD annotations (TODO: remove these altogether)
    annoClassOutpRemove <- c("CTD_CHEM2DISEASE", "CTD_CHEM2GENE_25", "CTD_PATHWAY")
    annoClassOutp2 <- annoClassOutp[!(annoClassOutp$annoclassname %in% annoClassOutpRemove), ]
    rownames(annoClassOutp2) <- seq_len(nrow(annoClassOutp2))
    # Close pool
    poolClose(poolAnnotations)
    return(annoClassOutp2)
}

#* Get total number of requests (internal use only)
#* @get /total
total <- function(res, req){
    # Connect to db
    poolTotal <- dbPool(
        drv=dbDriver("PostgreSQL", max.con=100),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    totalQuery <- sqlInterpolate(ANSI(), "SELECT id, timestamp_start, timestamp_finish FROM enrichment_list;", id="getTotalEnrichment")
    totalOutp <- dbGetQuery(poolTotal, totalQuery)

    # Extract current month
    currentDate <- unlist(str_split(Sys.time(), " "))[1]
    currentMonth <- unlist(str_split(currentDate, "-"))[2]
    # Get all finished requests for the month
    finishedRequests <- totalOutp[, "timestamp_finish"]
    finishedRequests<- unlist(lapply(finishedRequests, function(x){
        if(!is.na(x) & x != "canceled"){
            xDate <- unlist(str_split(x, " "))[1]
            xMonth <- unlist(str_split(xDate, "-"))[2]
            if(!is.na(xMonth) & xMonth == currentMonth){
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
        drv=dbDriver("PostgreSQL", max.con=100),
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
    if(reenrich == FALSE){
        substructureQuery <- sqlInterpolate(ANSI(), paste0("SELECT * FROM mols_2 WHERE m @> CAST('", input, "' AS mol);"), id="substructureResults")
    } else {
        substructureQuery <- sqlInterpolate(ANSI(), paste0("SELECT * FROM mols_2 WHERE casrn='", input, "';"), id="substructureResults")
    }
    substructureOutp <- dbGetQuery(poolSubstructure, substructureQuery)
    # Close pool
    poolClose(poolSubstructure)
    return(substructureOutp)
}

#* Get data for provided SMILES string (similarity) (internal use only)
#* @param input SMILES Input string
#* @param threshold Tanimoto similarity threshold
#* @get /similarity
similarity <- function(res, req, input="", threshold){
    # Connect to db
    poolSimilarity <- dbPool(
        drv=dbDriver("PostgreSQL", max.con=100),
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
    outpTanimoto <- dbGetQuery(poolSimilarity, queryTanimoto)
    similarityQuery <- sqlInterpolate(ANSI(), paste0("SELECT * FROM get_mfp2_neighbors('", input, "');"), id="similarityResults")
    similarityOutp <- dbGetQuery(poolSimilarity, similarityQuery)
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
        drv=dbDriver("PostgreSQL", max.con=100),
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
        drv=dbDriver("PostgreSQL", max.con=100),
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
        drv=dbDriver("PostgreSQL", max.con=100),
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
        drv=dbDriver("PostgreSQL", max.con=100),
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
    outDir <- paste0(APP_DIR, "/Output/")
    transactions <- Sys.glob(paste0(outDir, "*"))
    return(transactions)
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
    inDir <- paste0(APP_DIR, "Input/", transactionId, "/")
    dir.create(inDir)
    # Create output directory
    outDir <- paste0(APP_DIR, "Output/", transactionId, "/")
    dir.create(outDir)
    # Connect to db
    poolInput <- dbPool(
        drv=dbDriver("PostgreSQL", max.con=100),
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
        errorCasrnsFormatted <- unlist(lapply(errorCasrns, function(x){ # Strip "err__" from beginning of CASRNs
            return(unlist(str_split(x, "err__"))[2])
        }))
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
    inputDir <- paste0(APP_DIR, "Input/")
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
exists <- function(res, req, transactionId){
    inDir <- paste0(APP_DIR, "Input/")
    outDir <- paste0(APP_DIR, "Output/")
    checkIfInFile <- Sys.glob(paste0(inDir, transactionId, "/*"))
    checkIfOutFile <- Sys.glob(paste0(inDir, transactionId, "/*"))
    if(length(checkIfInFile) > 0 & length(checkIfOutFile) > 0){
        return(TRUE)
    } else {
        return(FALSE)
    }
}

## SECTION 5: DEALING WITH ENRICHMENT RESULTS

#* Get results for given directory
#* @param transactionId UUID of the request
#* @setName if supplied, is the set name to fetch from
#* @get /getResults
getResults <- function(res, req, transactionId, setName="###"){
    outDir <- paste0(APP_DIR, "Output/", transactionId, "/")
    setFiles <- NULL
    if(setName == "###") {
        setFiles <- Sys.glob(paste0(outDir, "/*"))
    } else {
        setFiles <- Sys.glob(paste0(outDir, setName, "*"), dirmark=FALSE) 
        setFiles <- unlist(lapply(setFiles, function(fileName){
            tmpSplit <- unlist(str_split(fileName, outDir))
            return(paste0("Output/", transactionId, "/", tmpSplit[2]))
        }))
    }
    # Add zip file
    setFiles <- append(setFiles, paste0("Output/", transactionId, "/tox21enricher_", transactionId, ".zip"))
    return(setFiles)
}

#* Read gct files for a given request
#* @param transactionId UUID of the request
#* @param cutoff Given node cutoff value for the request
#* @param mode Chart or Cluster
#* @get /readGct
readGct <- function(res, req, transactionId, cutoff, mode, set="Set1"){
    outDir <- paste0(APP_DIR, "Output/", transactionId, "/")
    gctFile <- NULL
    if(mode == "chart"){
        gctFile <- read.table(paste0(outDir, "gct/Chart_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.gct"), skip=2, header=TRUE, sep="\t", row.names=1, comment.char="", fill=FALSE, colClasses=c("Terms"="NULL", "integer") )  
    } else if(mode == "cluster"){
        gctFile <- read.table(paste0(outDir, "gct/Cluster_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.gct"), skip=2, header=TRUE, sep="\t", row.names=1, comment.char="", fill=FALSE, colClasses=c("Terms"="NULL", "integer") )
    } else { # per set
        gctFile <- read.table(paste0(outDir, "gct_per_set/", set, "__Chart.gct"), skip=2, header=TRUE, sep="\t", row.names=1, comment.char="", fill=FALSE, colClasses=c("Name"="NULL", "integer") )
    }
    return(gctFile)
}

#* Return Chart Simple file for given request and input set
#* @param transactionId UUID of the request
#* @param inputSet Current input set
#* @get /bargraph
bargraph <- function(res, req, transactionId){
    baseDirName <- paste0(APP_DIR, "Output/", transactionId, "/")
    # Get all Chart Simple files
    allChartFiles <- Sys.glob(paste0(baseDirName, "*__ChartSimple.txt"))
    bgChart <- lapply(allChartFiles, function(chartFile){
        bgChartTmp <- read.table(chartFile, header=TRUE, sep="\t", comment.char="", fill=FALSE)
        # Reformat P-Values to character vectors so they don't get truncated when being converted to json upon response
        bgChartTmp["PValue"] <- lapply(bgChartTmp["PValue"], function(x){
            return(as.character(x))
        })
        return(bgChartTmp)
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
    baseDirName <- paste0(APP_DIR, "Output/", transactionId, "/")
    chartForNetFile <- NULL
    # First, read files from /gct/ directory
    if(mode == "chart") {
        chartForNetFile <- read.delim(paste0(baseDirName, "/gct/Chart_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.ForNet"), sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, fill=TRUE)
    } else { # cluster
        chartForNetFile <- read.delim(paste0(baseDirName, "/gct/Cluster_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.ForNet"), sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, fill=TRUE)
    }
    chartNetworkUIDs <- unlist(mclapply(seq_len(nrow(chartForNetFile)), mc.cores=CORES, function(x){
        include <- lapply(input, function(y) {
            i <- gsub("\\s+", ".", y)
            if(!is.na(chartForNetFile[x, i])) {
                return(TRUE)
            }
            return(NULL)
        })
        include <- include[!vapply(include, is.null, FUN.VALUE=logical(1))]
        if(length(include) > 0) {
            # If significant to multiple input sets
            return(paste0(chartForNetFile[x, "UID"], collapse=","))
        }
        return(NULL)
    }))
    chartNetworkUIDs <- chartNetworkUIDs[!vapply(chartNetworkUIDs, is.null, FUN.VALUE=logical(1))]
    # Create placeholder string for querying database
    termsStringPlaceholder <- paste0(chartNetworkUIDs, collapse=",")
    # Connect to db
    poolNetwork <- dbPool(
        drv=dbDriver("PostgreSQL", max.con=100),
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
        drv=dbDriver("PostgreSQL", max.con=100),
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
        drv=dbDriver("PostgreSQL", max.con=100),
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
        drv=dbDriver("PostgreSQL", max.con=100),
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
        drv=dbDriver("PostgreSQL", max.con=100),
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
    inBaseDir <- paste0(APP_DIR, "/Input/")
    outBaseDir <- paste0(APP_DIR, "/Output/")
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
            drv=dbDriver("PostgreSQL", max.con=100),
            dbname=tox21config$database,
            host=tox21config$host,
            user=tox21config$uid,
            password=tox21config$pwd,
            port=tox21config$port,
            idleTimeout=3600000
        )
        queryTanimoto <- sqlInterpolate(ANSI(), paste0("set rdkit.tanimoto_threshold=", tanimoto, ";"))
        outpTanimoto <- dbGetQuery(poolTanimoto, queryTanimoto)
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
        drv=dbDriver("PostgreSQL", max.con=100),
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
        fName <- paste0(APP_DIR, "Output/", id, "/tox21enricher_", id, ".zip")
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
        fName <- paste0(APP_DIR, "Output/", id, "/tox21enricher_", id, ".zip")
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
        fName <- paste0(APP_DIR, "Output/", id, "/tox21enricher_", id, ".zip")
        # Returns 1 if completed, 0 if not.
        if(file.exists(fName)){
            return(1)  
        } else {
            return(0)
        }
    })
}

## SECTION 7: HANDLING API KEYS FOR USERS
#* Generate a unique key for authorizing API use.
#* @param email The email address to associate with the given API key.
#* @get /key
function(email="public", res) {
    key <- NULL
    # Connect to db
    pool <- dbPool(
        drv=dbDriver("PostgreSQL", max.con=100),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Fetch existing pair if exists
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM key WHERE email='", email, "';"), id="getEmails")
    outp <- dbGetQuery(pool, query)
    # Get list of all keys
    allKeysQuery <- sqlInterpolate(ANSI(), paste0("SELECT key FROM key;"), id="getKeys")
    allKeysOutp <- dbGetQuery(pool, allKeysQuery)
    returnMessage <- ""
    # Generate new key if email does not exist in database
    if(nrow(outp) < 1) {
        newKey <- UUIDgenerate()
        while(newKey %in% allKeysOutp$key) {
            newKey <- UUIDgenerate()
        }
        returnMessage <- paste0("Your new API key is: ", newKey)
        # Add new key/email to database
        query <- sqlInterpolate(ANSI(), paste0("INSERT INTO key(key, email) VALUES('", newKey, "', '", email, "');"), id="addKey")
        outp <- dbGetQuery(pool, query)
    } else { # else return existing key
        returnMessage <- paste0("An existing API key was found: ", outp$key[1])
    }
    # Close pool
    poolClose(pool)
    return(returnMessage)
}


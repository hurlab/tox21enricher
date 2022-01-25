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
CORES <- tox21config$cores
APP_DIR <- tox21config$appdir
IN_DIR <- tox21config$indir
OUT_DIR <- tox21config$outdir

# Cleanup anything old in the queue on startup
# Init database pool
poolClean <- dbPool(
    drv=RPostgres::Postgres(),
    dbname=tox21queue$database,
    host=tox21queue$host,
    user=tox21queue$uid,
    password=tox21queue$pwd,
    port=tox21queue$port,
    idleTimeout=3600000
)

# Unlock any currently locked, unfinished requests WITHOUT ERRORS AND THAT HAVEN'T BEEN CANCELLED so they can be reprocessed
query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET lock=0 WHERE finished=0 AND error IS NULL AND cancel=0;"), id="unlockTransactions")
outp <- dbExecute(poolClean, query)
# Get list of transactions that are unlocked and reset status messages
query <- sqlInterpolate(ANSI(), paste0("SELECT uuid FROM queue WHERE lock=0 AND finished=0 AND error IS NULL AND cancel=0;"), id="resetStatus")
outp <- dbGetQuery(poolClean, query)
unlockedTransactions <- outp$uuid
lapply(unlockedTransactions, function(x){
    query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=0 WHERE uuid='", x, "';"), id="resetStatus")
    outp <- dbExecute(poolClean, query)
    return(paste0("Reprocessing transaction: ", x))
})
# Close pool
poolClose(poolClean)

# Define info for connecting to PostgreSQL Tox21 Enricher database on server startup
pool <- dbPool(
    drv=RPostgres::Postgres(),
    dbname=tox21config$database,
    host=tox21config$host,
    user=tox21config$uid,
    password=tox21config$pwd,
    port=tox21config$port,
    idleTimeout=3600000
)

# Grab annotation list from Tox21 Enricher database on server startup
queryAnnotations <- sqlInterpolate(ANSI(), "SELECT chemical_detail.casrn, annotation_class.annoclassname, annotation_detail.annoterm FROM term2casrn_mapping INNER JOIN chemical_detail ON term2casrn_mapping.casrnuid_id=chemical_detail.casrnuid INNER JOIN annotation_detail ON term2casrn_mapping.annotermid=annotation_detail.annotermid INNER JOIN annotation_class ON term2casrn_mapping.annoclassid=annotation_class.annoclassid;")
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
poolClose(pool)

# Load base Annotations
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
CASRN2funCatTerm_lv1 <- lapply(split(outpAnnotations, outpAnnotations$casrn), function(x) split(x, x$annoclassname))
CASRN2funCatTerm <- lapply(CASRN2funCatTerm_lv1, function(x){
    return(lapply(x, function(y){
        inner_CASRN2funCatTerm <- lapply(y$annoterm, function(z) 1)
        names(inner_CASRN2funCatTerm) <- y$annoterm
        return(inner_CASRN2funCatTerm)
    }))
})
# funCatTerm2CASRN
funCatTerm2CASRN_lv1 <- lapply(split(outpAnnotations, outpAnnotations$annoclassname), function(x) split(x, x$annoterm))
funCatTerm2CASRN <- lapply(funCatTerm2CASRN_lv1, function(x){
    return(lapply(x, function(y) {
        inner_funCatTerm2CASRN <- lapply(y$casrn, function(z) 1) 
        names(inner_funCatTerm2CASRN) <- (y$casrn)
        return(inner_funCatTerm2CASRN)
    }))
})
# funCat2CASRN
funCat2CASRN <- lapply(split(outpAnnotations, outpAnnotations$annoclassname), function(x) split(x, x$casrn))
# term2funCat
term2funCat <- lapply(split(outpAnnotations, outpAnnotations$annoterm), function(x) split(x, x$annoclassname))

print("! Finished loading DrugMatrix annotations.")
print("! Ready to accept connections.")

#######################################################################################

# Perform enrichment analysis (for internal use by Tox21 Enricher only)
# enrichmentUUID UUID for Input/Output directory on local machine, generated by Tox21 Enricher application.
# annoSelectStr String, comma-delimited, containing all enabled annotations for this enrichment process. Passed from Tox21 Enricher application.
# nodeCutoff numerical value between 1-100 for the max number to use in clustering.
performEnrichment <- function(enrichmentUUID="-1", annoSelectStr="MESH=checked,PHARMACTIONLIST=checked,ACTIVITY_CLASS=checked,ADVERSE_EFFECT=checked,INDICATION=checked,KNOWN_TOXICITY=checked,MECH_LEVEL_1=checked,MECH_LEVEL_2=checked,MECH_LEVEL_3=checked,MECHANISM=checked,MODE_CLASS=checked,PRODUCT_CLASS=checked,STRUCTURE_ACTIVITY=checked,TA_LEVEL_1=checked,TA_LEVEL_2=checked,TA_LEVEL_3=checked,THERAPEUTIC_CLASS=checked,TISSUE_TOXICITY=checked,DRUGBANK_ATC=checked,DRUGBANK_ATC_CODE=checked,DRUGBANK_CARRIERS=checked,DRUGBANK_ENZYMES=checked,DRUGBANK_TARGETS=checked,DRUGBANK_TRANSPORTERS=checked,CTD_CHEM2DISEASE=checked,CTD_CHEM2GENE_25=checked,CTD_CHEMICALS_DISEASES=checked,CTD_CHEMICALS_GENES=checked,CTD_CHEMICALS_GOENRICH_CELLCOMP=checked,CTD_CHEMICALS_GOENRICH_MOLFUNCT=checked,CTD_CHEMICALS_PATHWAYS=checked,CTD_GOSLIM_BIOPROCESS=checked,CTD_PATHWAY=checked,HTS_ACTIVE=checked,LEADSCOPE_TOXICITY=checked,MULTICASE_TOX_PREDICTION=checked,TOXCAST_ACTIVE=checked,TOXINS_TARGETS=checked,TOXPRINT_STRUCTURE=checked,TOXREFDB=checked,", nodeCutoff=10) {
    # Connect to db
    poolInput <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    
    # Get begin time for request
    beginTime <- Sys.time()
    
    # Set begin time in transaction table
    query <- sqlInterpolate(ANSI(), paste0("UPDATE transaction SET timestamp_started='", beginTime, "' WHERE uuid='", enrichmentUUID, "';"), id="updateTransactionStart")
    outp <- dbExecute(poolInput, query)   
    
    # Close pool
    poolClose(poolInput)

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
    pvalueThresholdToDisplay <- 0.2 # p-value < 0.1 to be printed

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
        tmp_funCatTerm2CASRNCount <- lapply(names(funCatTerm2CASRN[[funCat]]), function(term) length(names(funCatTerm2CASRN[[funCat]][[term]])))
        names(tmp_funCatTerm2CASRNCount) <- names(funCatTerm2CASRN[[funCat]])
        return(tmp_funCatTerm2CASRNCount)
    })
    names(funCatTerm2CASRNCount) <- names(funCat2Selected)

    # Load input DSSTox ID or CASRN ID sets
    inputFiles <- list.files(path=paste0(APP_DIR, IN_DIR, enrichmentUUID), pattern="*.txt", full.names=TRUE)

    # Throw error if no input sets
    if(length(inputFiles) < 1){
        # Initialize db connection pool
        poolStatus <- dbPool(
            drv=RPostgres::Postgres(),
            dbname=tox21queue$database,
            host=tox21queue$host,
            user=tox21queue$uid,
            password=tox21queue$pwd,
            port=tox21queue$port,
            idleTimeout=3600000
        )
        # Set step flag for each set name
        query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=-1 WHERE uuid='", enrichmentUUID, "';"), id="fetchStatus")
        outp <- dbExecute(poolStatus, query)
        # Check if no errors
        query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
        requestCancel <- dbGetQuery(poolStatus, query)
        # Close pool
        poolClose(poolStatus)
        if(nrow(requestCancel) > 0){
            print(paste0("Cancelling request: ", enrichmentUUID))
            return(FALSE)
        }
        return("No valid input sets.")
    }

    # Check which input sets are good
    ldf <- lapply(inputFiles, function(i){
        openInput <- tryCatch({
            read.delim(file=i, header=FALSE, sep="\t", comment.char="", fill=TRUE)
        }, error=function(e){
            return(NULL)
        })
    })
    ldf <- lapply(seq_len(length(ldf)), function(x) ldf[[x]])

    # Assign names to ldf
    inputFilesNames <- unlist(lapply(inputFiles, function(x){
        x_lv1 <- gsub(paste0(APP_DIR, IN_DIR, enrichmentUUID, "/"), "", x)
        return(x_lv2 <- gsub(".txt", "", x_lv1))
    }))
    names(ldf) <- inputFilesNames
    ldf <- ldf[!vapply(ldf, is.null, FUN.VALUE=logical(1))]
    
    # If there are no good input sets, crash gracefully.
    if(length(ldf) < 1){
        # Initialize db connection pool
        poolStatus <- dbPool(
            drv=RPostgres::Postgres(),
            dbname=tox21queue$database,
            host=tox21queue$host,
            user=tox21queue$uid,
            password=tox21queue$pwd,
            port=tox21queue$port,
            idleTimeout=3600000
        )
        # Set step flag for each set name
        query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=-1 WHERE uuid='", enrichmentUUID, "';"), id="fetchStatus")
        outp <- dbExecute(poolStatus, query)
        # Check if no errors
        query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
        requestCancel <- dbGetQuery(poolStatus, query)
        # Close pool
        poolClose(poolStatus)
        if(nrow(requestCancel) > 0){
            print(paste0("Cancelling request: ", enrichmentUUID))
            return(FALSE)
        }
        return("No lines available in any input file. Cannot perform enrichment analysis.")
    }

    inputIDListHash <- lapply(ldf, function(i){
        res <- apply(i, 1, function(j){
            tmp <- list(1)
            names(tmp) <- j[1] 
            return(tmp)
        })
        return(res)
    })
    print(paste0("Processing ", length(inputIDListHash), " set(s)."))

    # Perform EASE calculation
    outfileBaseNames <- lapply(seq_len(length(ldf)), function(setNameCtr){
        setNameItem <- str_remove(inputFiles[[setNameCtr]], paste0(APP_DIR, IN_DIR, enrichmentUUID, "/")) # Remove path
        setNameItem <- str_remove(setNameItem, ".txt") # Remove filename extension
        return(setNameItem)
    })
    names(inputIDListHash) <- outfileBaseNames

    # Connect to DB to get status info
    poolStatus <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )

    # Get set names from database
    query <- sqlInterpolate(ANSI(), paste0("SELECT setname FROM status WHERE uuid='", enrichmentUUID, "';"), id="fetchStatus")
    outp <- dbGetQuery(poolStatus, query)
    statusFiles <- outp[, "setname"]
    # Set step flag for each set name
    lapply(statusFiles, function(x){
        query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=2 WHERE uuid='", enrichmentUUID, "' AND setname='", x, "' AND step<>-1;"), id="fetchStatus")
        outp <- dbExecute(poolStatus, query)
    })
    # Check if no errors
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
    requestCancel <- dbGetQuery(poolStatus, query)
    # Close pool
    poolClose(poolStatus)
    if(nrow(requestCancel) > 0){
        print(paste0("Cancelling request: ", enrichmentUUID))
        return(FALSE)
    }

    # multi-core
    #TODO
    enrichmentStatusComplete <- mclapply(outfileBaseNames, mc.cores=CORES, mc.silent=FALSE, function(outfileBase){
    #enrichmentStatusComplete <- lapply(outfileBaseNames, function(outfileBase){
        # Get list of CASRN names
        CASRNS <- lapply(inputIDListHash[[outfileBase]], function(j) names(j))
        # Check mapped CASRNS
        mappedCASRNs <- unlist(lapply(CASRNS, function(CASRN){
            if(!is.null(CASRN2DSSTox[[CASRN]])){
                return(1)
            }
            return(NULL)
        }))
        names(mappedCASRNs) <- CASRNS
        mappedCASRNs <- mappedCASRNs[!vapply(mappedCASRNs, is.null, FUN.VALUE=logical(1))]

        # Perform enrichment analysis
        print(paste0("Performing enrichment on ", outfileBase, "..."))
        enrichmentStatus <- perform_CASRN_enrichment_analysis(CASRNS, paste0(APP_DIR, OUT_DIR, enrichmentUUID, "/"), outfileBase, mappedCASRNs, funCat2Selected, CASRN2funCatTerm, funCatTerm2CASRN, funCat2CASRNCount, funCat2termCount, funCatTerm2CASRNCount, pvalueThresholdToDisplay, similarityThreshold, initialGroupMembership, multipleLinkageThreshold, EASEThreshold, nodeCutoff, enrichmentUUID)
    })
    
    # Create individual GCT file

    # Update status file
    # Connect to DB to get status info
    poolStatus <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Get set names from database
    query <- sqlInterpolate(ANSI(), paste0("SELECT setname FROM status WHERE uuid='", enrichmentUUID, "';"), id="fetchStatus")
    outp <- dbGetQuery(poolStatus, query)
    statusFiles <- outp[, "setname"]
    # Set step flag for each set name
    lapply(statusFiles, function(x){
        query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=7 WHERE uuid='", enrichmentUUID, "' AND setname='", x, "' AND step<>-1;"), id="fetchStatus")
        outp <- dbExecute(poolStatus, query)
    })
    # Check if no errors
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
    requestCancel <- dbGetQuery(poolStatus, query)
    # Close pool
    poolClose(poolStatus)
    if(nrow(requestCancel) > 0){
        print(paste0("Cancelling request: ", enrichmentUUID))
        return(FALSE)
    }

    # Get the corresponding directory names
    baseinputDirName <- inDir
    baseDirName <- outDir
    baseShortDirName <- ""
    baseOutputDir <- paste0(baseDirName, "/gct_per_set/")
    baseOutputDirGct <- paste0(baseDirName, "/gct/")
    dir.create(baseOutputDir)

    # Load CASRN names
    CASRN2Name <- lapply(seq_len(nrow(outpChemDetail)), function(x) outpChemDetail[x, "testsubstance_chemname"])
    names(CASRN2Name) <- lapply(seq_len(nrow(outpChemDetail)), function(x) outpChemDetail[x, "casrn"])

    # Generate individual gct files
    process_variable_DAVID_CHART_directories_individual_file (baseinputDirName, baseDirName, baseOutputDir, '', '', "P", 0.05, "P", CASRN2Name)
    # Generate clustering images (heatmaps)
    create_clustering_images (baseOutputDir, "-color=BR")
    # Create DAVID Chart/Cluster files
    create_david_chart_cluster (baseDirName, nodeCutoff, "ALL", "P", 0.05, "P")
    # Generate heatmaps for multiple sets
    create_clustering_images (baseOutputDirGct, "-color=BR")

    # zip result files
    if(dir.exists(baseDirName)){
        zipDir <- dir(baseDirName, recursive=TRUE, include.dirs=TRUE)
        filesToZip <- unlist(lapply(zipDir, function(x) paste0(baseDirName, "/", x)))
        system2("cd", paste0(baseDirName, "/ ; zip -r9X ./tox21enricher_", enrichmentUUID, ".zip ./*"))
    } else { # Return with error if did not complete. Do not update in database
        return(-1) 
    }

    # Update status file(s)
    # Connect to DB to get status info
    poolStatus <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    
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
    poolClose(poolStatus)
    if(nrow(requestCancel) > 0){
        print(paste0("Cancelling request: ", enrichmentUUID))
        return(FALSE)
    }

    # success
    return(200)
}

## SUBROUTINES

# Generate individual GCT files
process_variable_DAVID_CHART_directories_individual_file <- function(inputDirName, dirName, outputDir, extTag, additionalFileName, sigColumn, sigCutOff, valueColumn, CASRN2Name) {
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
        infileDF <- read.delim(infile, sep="\t", header=TRUE, comment.char="", fill=TRUE, stringsAsFactors=FALSE)
        if(nrow(infileDF) < 1){
            return(NULL)
        }
        return(infile)
    })
    infiles <- infiles[!vapply(infiles, is.null, FUN.VALUE=logical(1))]
    sigColumnName <- toupper(sigColumn)
    sigColumnIndex <- get_column_index(sigColumnName) # 5=p-value, 12=BH p-value, 13=FDR
    valueColumnName <- toupper(valueColumn)
    valueColumnIndex <- get_column_index(valueColumnName) # 5=p-value, 12=BH p-value, 13=FDR

    # Step1. Get the list of significant terms
    mclapply(infiles, mc.cores=CORES, mc.silent=FALSE, function(infile){
        tmp1 <- unlist(str_split(infile, "/"))
        tmp1[length(tmp1[[1]])] <- gsub(".txt", "", tmp1[[1]][length(tmp1[[1]])])
        tmp2 <- tmp1
        DATA <- read.delim(file=infile, header=TRUE, sep="\t", comment.char="", fill=TRUE) 
        if(nrow(DATA) < 1){
            # If no rows, make blank .gct file
            setFileName <- gsub(".txt", "", tmp2[length(tmp2)])
            OUTFILE <- file(paste0(outputDir, setFileName, ".gct"))
            outputContent <- paste0("#1.2\n", 0, "\t", 0, "\nCASRN\tName")
            writeLines(outputContent, OUTFILE) 
            close(OUTFILE)
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
                tmpSplit[12] <- as.character(DATA[line, "Benjamini"])
                tmpSplit[13] <- as.character(DATA[line, "FDR"])
                if ( grepl("^\\D", tmpSplit[[sigColumnIndex]]) | as.double(tmpSplit[[sigColumnIndex]]) >= sigCutOff | as.double(tmpSplit[[10]]) < 1) {
                    # skip
                } else {
                    return(tmpSplit[[sigColumnIndex]])
                }
            })

            # Remove null elements
            term2pvalue <- lapply(term2pvalue, function(innerList) innerList[vapply(innerList, length, FUN.VALUE=numeric(1)) > 0])
            term2pvalue <- term2pvalue[!vapply(term2pvalue, is.null, FUN.VALUE=logical(1))]

            nameListTerm2pvalue <- lapply(seq_len(nrow(DATA)), function(line){
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
                tmpSplit[12] <- as.character(DATA[line, "Benjamini"])
                tmpSplit[13] <- as.character(DATA[line, "FDR"])
                if ( grepl("^\\D", tmpSplit[[sigColumnIndex]]) | as.double(tmpSplit[[sigColumnIndex]]) >= sigCutOff | as.double(tmpSplit[[10]]) < 1) {
                    # skip
                } else {
                    tmpTermKey <- paste0(tmpSplit[[2]], " | ", tmpSplit[[1]])
                    return(tmpTermKey)
                }
            })
            
            # Remove null elements
            nameListTerm2pvalue <- lapply(nameListTerm2pvalue, function(innerList) innerList[vapply(innerList, length, FUN.VALUE=numeric(1)) > 0])
            nameListTerm2pvalue <- nameListTerm2pvalue[!vapply(nameListTerm2pvalue, is.null, FUN.VALUE=logical(1))]
    
            # Set names of term2pvalue
            names(term2pvalue) <- nameListTerm2pvalue

            CASRN2TermMatrix_lv1 <- lapply(seq_len(nrow(DATA)), function(line){
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
                tmpSplit[12] <- as.character(DATA[line, "Benjamini"])
                tmpSplit[13] <- as.character(DATA[line, "FDR"])
                if ( grepl("^\\D", tmpSplit[[sigColumnIndex]]) | as.double(tmpSplit[[sigColumnIndex]]) >= sigCutOff | as.double(tmpSplit[[10]]) < 1) {
                    # skip
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
        }
    })
}

get_column_index <- function(columnType) {
    if (grepl("P", columnType, ignore.case=TRUE)) { 
        return(5)
    } else if(grepl("BH", columnType, ignore.case=TRUE)) { 
        return(12)
    } else if(grepl("BF", columnType, ignore.case=TRUE)) {
        return(13)
    } else { 
        return("!ERROR! Wrong Signficance type. Use P, BH, or BF\n\n")
    }
}

# Create clustering images
create_clustering_images <- function(outDir="", imageFlags="-color=BR"){
    # Define required variables - Clustering
    dirName <- outDir
    dirNameSplit <- unlist(str_split(dirName, "/"))
    dirTypeTag <- ""
    dirNameSplit <- dirNameSplit[nchar(dirNameSplit) > 0]
    if(grepl("CLUSTER", dirNameSplit[1], fixed=TRUE)){
        dirTypeTag <- "CLUSTER__"
    } else if(grepl("CHART", dirNameSplit[1], fixed=TRUE)) { 
        dirTypeTag <- "CHART__"
    } else if (grepl("PRESELECTED", dirNameSplit[1], fixed=TRUE)){
        dirTypeTag <- "PRESELECTED__"
    }

    # Define required variables - Clustering Images
    java_flags <- "-Djava.awt.headless=true -Xmx1024m"
    row_size <- "16"
    column_size <- "16"
    show_grid <- "yes"
    grid_color <- "0:0:0"
    show_row_description <- "yes"
    show_row_names <- "yes"
    row_to_highlight <- ""
    row_highlight_color <- ""
    use_color_gradient <- "no"
    outputBaseDir <- outDir
    libDir <- paste0(APP_DIR, "HClusterLibrary/")
    column_distance_measure <- "2" #pearson correlation
    row_distance_measure <- "2" #pearson correlation
    clustering_method <- "m" #pairwise complete-linkage
    color_scheme <- "global" # or "row normalized"
    color_palette <- paste0(libDir, "colorSchemeBlackRed.txt")
    output_format <- "png"
    # .jpeg, .png, .tiff, .bmp, .eps
    if (!is.null(imageFlags) & grepl("(jpeg|png|tiff|bmp|eps)", tolower(imageFlags), fixed=TRUE)){ 
        output_format <- tolower(imageFlags)
    }

    # Check OS and define program - this is probably always going to be 64-bit Linux at the moment.
    cluster_program <- "clusterLinux64"

    # Load directory list
    baseNameSplit <- unlist(str_split(dirName, "/"))
    baseNameSplit <- baseNameSplit[nchar(baseNameSplit) > 0]
    baseShortDirName  <- ""
    if (baseNameSplit[length(baseNameSplit)] == ""){
        baseShortDirName <- paste0(baseNameSplit[length(baseNameSplit)-1], '/')
    } else {
        baseShortDirName <- paste0(baseNameSplit[length(baseNameSplit)], '/')
    }
    DIR <- list.files(dirName)
    tmpDirs <- DIR

    # Remove current directory and previous directory
    baseSubDirs <- lapply(tmpDirs, function(x){
        if(x != "." & x != "..") {
            return(x)
        }
        return(NULL)
    })
    baseSubDirs <- baseSubDirs[!vapply(baseSubDirs, is.null, FUN.VALUE=logical(1))]

    # Perform HClustering
    perform_hclustering_per_directory (dirName, '', outputBaseDir, dirTypeTag, libDir, cluster_program, row_distance_measure, column_distance_measure, clustering_method, java_flags, output_format, column_size, row_size, show_grid, grid_color, show_row_description, show_row_names, row_to_highlight, row_highlight_color, color_scheme, color_palette, use_color_gradient)
    if (!is.null(baseSubDirs[1])){
        performHclusteringForEach <- mclapply(baseSubDirs, mc.cores=CORES, mc.silent=FALSE, function(x){
            perform_hclustering_per_directory (paste0(dirName, '/', x, baseShortDirName, '/'), outputBaseDir, dirTypeTag, libDir, cluster_program, row_distance_measure, column_distance_measure, clustering_method, java_flags, output_format, column_size, row_size, show_grid, grid_color, show_row_description, show_row_names, row_to_highlight, row_highlight_color, color_scheme, color_palette, use_color_gradient)
        })
    }
    print("! COMPLETE ...")
    return("success")
}

# Generate heatmap images using HierarchicalClusteringImages (HCI)
perform_hclustering_per_directory <- function(givenDirName, additionalDirName, outputBaseDir, dirTypeTag, libDir, cluster_program, row_distance_measure, column_distance_measure, clustering_method, java_flags, output_format, column_size, row_size, show_grid, grid_color, show_row_description, show_row_names, row_to_highlight, row_highlight_color, color_scheme, color_palette, use_color_gradient) {
    # Load GCT files for hierarchical clustering, in the given directory
    gctFiles <- Sys.glob(paste0(givenDirName, "/", "*.gct"), dirmark=FALSE)
    tmp1 <- unlist(str_split (givenDirName, "/"))
    tmp1 <- tmp1[nchar(tmp1) > 0]
    baseDirName <- tmp1[length(tmp1)]
    outputDir <- paste0(outputBaseDir, '/')
    create_sub_directory(outputDir) # create subdirectories
    generateImagesProcess <- mclapply (gctFiles, function(infile){
        tmp1 <- unlist(str_split(infile, "/"))
        tmp1 <- tmp1[nchar(tmp1) > 0]
        tmp2 <- unlist(str_split(tmp1[length(tmp1)], ".gct"))
        tmp2 <- tmp2[nchar(tmp2) > 0]
        # Check gct file content and skip if there is less than 2 entries
        if(!check_gct_contains_more_than_two_lines(infile)){
            # Do nothing
        }
        else {
            output_base_name <- paste0(outputDir, dirTypeTag, tmp2[1])
            shorter_base_name <- paste0(outputDir, dirTypeTag, tmp2[1])
            cluster_input_file <- paste0(shorter_base_name, ".txt")
            if (convert_gct_to_cluster_input_file(infile, cluster_input_file, outputDir)){
                system2(paste0(libDir, cluster_program), paste0(" -f ", cluster_input_file, " -g ", row_distance_measure, " -e ", column_distance_measure, " -m ", clustering_method))
                cdtFile <- paste0(output_base_name, ".cdt")
                gtrFile <- paste0(output_base_name, ".gtr")
                atrFile <- paste0(output_base_name, ".atr")
                gtrCmd <- ""
                atrCmd <- ""
                if (row_distance_measure != 0){ 
                    gtrCmd <- paste0(" -x\"", gtrFile, "\"")
                }
                if (column_distance_measure != 0){ 
                    atrCmd <- paste0(" -y\"", atrFile, "\"")
                }
                # Create heatmap image
                system2("java", paste0(java_flags, " -DlibDir=", libDir, " -jar ", libDir, "hclimage-o.jar \"", cdtFile, "\" \"", output_base_name, "\" ", output_format, " -c", column_size, " -r", row_size, " -g", show_grid, " -l", grid_color, " -a", show_row_description, " -s", show_row_names, " -n", color_scheme, " -m", color_palette, " -u", use_color_gradient))
            } else {
                print(paste0("Conversion failed for ", infile, "\n"))
            }
        }
    })
}

convert_gct_to_cluster_input_file <- function(gctFile, clusterInputFile, outputDir){
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
    return(1)
}

create_sub_directory <- function(outputDir){
    tmp1 <- unlist(str_split(outputDir, "/"))
    tmp1 <- tmp1[nchar(tmp1) > 0]
    dirName <- paste0("/", tmp1, "/", collapse="")
    dir.create(dirName)
}

check_gct_contains_more_than_two_lines <- function(infile){
    INFILE <- read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, skip=2, fill=TRUE)
    lineCount <- nrow(INFILE)
    if(lineCount >= 2){
        return(1)
    } else {
        return(0)
    }
}

###### Create DAVID Chart and Cluster files ######
create_david_chart_cluster <- function(baseDirName="", topTermLimit=10, mode="ALL", sigColumnName="P", sigCutoff=0.05, valueColumnName="P"){
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
    if (dir.exists(baseOutputDir)) {
        # do nothing if directory already exists (i.e., we are regenerating the network)
    } else {
        # else create the directory if this is the first time we are dealing with this data set
        print(paste0("No directory found for ", baseOutputDir, ". Creating..."))
        dir.create(baseOutputDir)
    }
    # Load term annotation data
    className2classID <- lapply(seq_len(nrow(outpClasses)), function(line) outpClasses[line, "annoclassid"])
    names(className2classID) <- lapply(seq_len(nrow(outpClasses)), function(line) outpClasses[line, "annoclassname"])
    classID2className <- lapply(seq_len(nrow(outpClasses)), function(line) outpClasses[line, "annoclassname"])
    names(classID2className) <- lapply(seq_len(nrow(outpClasses)), function(line) outpClasses[line, "annoclassid"])
    classID2annotationTerm2termUniqueID_lv1 <- lapply(split(outpAnnoDetail, outpAnnoDetail$annoclassid), function(x) split(x, x$annoterm))
    classID2annotationTerm2termUniqueID <- lapply(classID2annotationTerm2termUniqueID_lv1, function(x) {
        return(lapply(x, function(y) {
            inner_classID2annotationTerm2termUniqueID <- lapply(y$annotermid, function(z) y$annotermid)
            names(inner_classID2annotationTerm2termUniqueID) <- y$annotermid
            return(inner_classID2annotationTerm2termUniqueID)
        }))
    })
    # Enumerate all possible directories
    process_variable_DAVID_CHART_directories (baseDirName, baseOutputDir, "", "", topTermLimit, mode, sigColumnName, sigCutoff, valueColumnName, className2classID, classID2annotationTerm2termUniqueID)
    process_variable_DAVID_CLUSTER_directories (baseDirName, baseOutputDir, "", "", topTermLimit, mode, sigColumnName, sigCutoff, valueColumnName, className2classID, classID2annotationTerm2termUniqueID)
}

process_variable_DAVID_CLUSTER_directories <- function(dirName, outputDir, extTag, additionalFileName, topTermLimit, mode, sigColumnName, sigCutoff, valueColumnName, className2classID, classID2annotationTerm2termUniqueID){
    # Check the directory for any file
    infiles <- Sys.glob(paste0(dirName, "/*_Cluster.txt"))
    if (is.null(infiles[1])){
        return(FALSE)
    }
    # Define number of top clusters
    dir.create(outputDir)
    dirInputName <- dirName
    dirInputExpression <- paste0(dirInputName, "ExpressionData/")
    sigColumnIndex <- get_column_index(sigColumnName) # 5=p-value, 12=BH p-value, 13=FDR
    valueColumnIndex <- get_column_index(valueColumnName)
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
    getSigTerms <- lapply(infiles, function(infile) {
        tmp1 <- unlist(str_split(infile, "/"))
        tmpNameSplit <- unlist(str_split(tmp1[length(tmp1)], "__Cluster.txt"))
        shortFileBaseName <- tmpNameSplit[1]
        originalSourceFile <- paste0(dirInputName, "/", shortFileBaseName, ".txt")
        lines <- read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=FALSE, fill=TRUE, col.names=c("Category", "Term", "Count", "%", "PValue", "CASRNs", "List", "Total", "Pop Hits", "Pop Total", "Fold Enrichment", "Bonferroni", "Benjamini", "FDR"))
        if(nrow(lines) < 1){
            return(FALSE)
        }
        tmpIDList <- lapply(seq_len(nrow(lines)), function(i){
            tmpSplit <- lapply(lines[i, ], function(lineItem) {
                return(lineItem)
            })
            if(grepl("^Annotation Cluster", tmpSplit[[1]])) {
                firstClusterIndex <- i + 2
                tmpSplitInner <- lapply(lines[firstClusterIndex, ], function(lineItem) lineItem)
                if (tmpSplitInner[[sigColumnIndex]] == "" | is.na(tmpSplitInner[[sigColumnIndex]]) | grepl("^\\D", tmpSplitInner[[sigColumnIndex]]) | as.double(tmpSplitInner[[sigColumnIndex]]) >= sigCutoff | as.double(tmpSplitInner[[10]]) < 1) {
                    return(NULL)
                } else {
                    tmpID <- paste0(tmpSplitInner[[1]], " | ", tmpSplitInner[[2]])
                    return(tmpID)
                }
            }
        })
        
        #Get not null elements
        tmpIDList <- tmpIDList[!vapply(tmpIDList, is.null, FUN.VALUE=logical(1))]
        tmpIDList <- unlist(tmpIDList, recursive=FALSE)
        
        # Cut off entries over the limit
        if(length(tmpIDList) > as.double(topTermLimit)){
            tmpIDList <- tmpIDList[seq_len((as.double(topTermLimit)))]
        }
    
        tmp_ID2Term <- lapply(seq_len(nrow(lines)), function(i){
            tmpSplit <- lapply(lines[i, ], function(lineItem) lineItem)
            if(grepl("^Annotation Cluster", tmpSplit[[1]])) {
                firstClusterIndex <- i + 2
                tmpSplitInner <- lapply(lines[firstClusterIndex, ], function(lineItem) lineItem)
                if (tmpSplitInner[[sigColumnIndex]] == "" | is.na(tmpSplitInner[[sigColumnIndex]]) | grepl("^\\D", tmpSplitInner[[sigColumnIndex]]) | as.double(tmpSplitInner[[sigColumnIndex]]) >= sigCutoff | as.double(tmpSplitInner[[10]]) < 1) {
                    return(NULL)
                } else {
                    tmpTerm <- paste0(tmpSplitInner[[1]], " | ", tmpSplitInner[[2]])
                    return(tmpTerm)
                }
            }
            return(NULL)
        })
        #Get not null elements
        tmp_ID2Term <- tmp_ID2Term[!vapply(tmp_ID2Term, is.null, FUN.VALUE=logical(1))]
        tmp_ID2Term <- unlist(tmp_ID2Term, recursive=FALSE)
        
        # Cut off entries over the limit
        if(length(tmp_ID2Term) > as.double(topTermLimit)){
            tmp_ID2Term <- tmp_ID2Term[seq_len((as.double(topTermLimit)))]
        }
        
        tmp_ID2Class <- lapply(seq_len(nrow(lines)), function(i){
            tmpSplit <- lapply(lines[i, ], function(lineItem) lineItem)
            if(grepl("^Annotation Cluster", tmpSplit[[1]])) {
                firstClusterIndex <- i + 2
                tmpSplitInner <- lapply(lines[firstClusterIndex, ], function(lineItem) lineItem)
                if (tmpSplitInner[[sigColumnIndex]] == "" | is.na(tmpSplitInner[[sigColumnIndex]]) | grepl("^\\D", tmpSplitInner[[sigColumnIndex]]) | as.double(tmpSplitInner[[sigColumnIndex]]) >= sigCutoff | as.double(tmpSplitInner[[10]]) < 1) {
                    return(NULL)
                } else {
                    return(tmpSplitInner[[1]])
                }
            }
            return(NULL)
        })
        #Get not null elements
        tmp_ID2Class <- tmp_ID2Class[!vapply(tmp_ID2Class, is.null, FUN.VALUE=logical(1))]
        tmp_ID2Class <- unlist(tmp_ID2Class, recursive=FALSE)
        
        # Cut off entries over the limit
        if(length(tmp_ID2Class) > as.double(topTermLimit)){
            tmp_ID2Class <- tmp_ID2Class[seq_len((as.double(topTermLimit)))]
        }
        names(tmp_ID2Term) <- tmpIDList
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
        DATA <- tryCatch(read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, fill=TRUE, skip=1), error=function(cond) {
            print("No lines in input file.")
            return(NULL)
        })
        if (nrow(DATA) > 0){
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
        pvalueMatrix_inner <- unlist(lapply(seq_len(length(x[["PValue"]])), function(y) {
            if (x[y, "PValue"] == "" | is.null(x[y, "PValue"]) | is.null(x[y, "Fold.Enrichment"]) | grepl("^\\D", x[y, "PValue"]) | (mode == "ALL" & x[y, "PValue"] >= sigCutoff) | x[y, "Fold.Enrichment"] < 1) {
                return(NULL)
            }
            return(-1*log10(as.double(x[y, "PValue"])))
        }))
        if(is.null(pvalueMatrix_inner)) {
            return(NULL)
        }
        names(pvalueMatrix_inner) <- unlist(lapply(seq_len(length(x[["PValue"]])), function(y) {
            if (x[y, "PValue"] == "" | is.null(x[y, "PValue"]) | is.null(x[y, "Fold.Enrichment"]) | grepl("^\\D", x[y, "PValue"]) | (mode == "ALL" & x[y, "PValue"] >= sigCutoff) | x[y, "Fold.Enrichment"] < 1) {
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
    file.create(paste0(outputDir, summaryFileName))
    SUMMARY <- file(paste0(outputDir, summaryFileName))
    fileHeaderNames <- sort_by_file_number(unlist(fileHeaderNames))
    writeToSummaryHeader <- paste0("GROUP\tID\tTerms\t", paste0(fileHeaderNames, collapse="\t"), "\n")

    #Get not null elements
    IDs <- IDs[!vapply(IDs, is.na, FUN.VALUE=logical(1))]
    IDs <- unique(IDs)
  
    writeToSummary <- lapply(IDs, function(ID) { 
        writeToSummaryLabel <- paste0(ID2Class[ID], "\t", ID, "\t")
        IDSplit <- unlist(str_split(ID, "\\|"))
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
    write(paste0(writeToSummaryHeader, writeToSummary), SUMMARY, append=TRUE)
    close(SUMMARY)

    # Create a network summary file for Cluster
    ForNetworkFile <- paste0(summaryFileNameBase, "__ValueMatrix.ForNet")
    file.create(paste0(outputDir, ForNetworkFile))
    NETWORK <- file(paste0(outputDir, ForNetworkFile))
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
    write(paste0(writeToNetworkHeader, writeToNetwork), NETWORK, append=TRUE)
    close(NETWORK)
  
    # Create a gct file from ValueMatrix
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
}

process_variable_DAVID_CHART_directories <- function(dirName, outputDir, extTag, additionalFileName, topTermLimit, mode, sigColumnName, sigCutoff, valueColumnName, className2classID, classID2annotationTerm2termUniqueID){
    # Check the directory for any file
    infilesAll <- Sys.glob(paste0(dirName, "/*_Chart.txt"))
    if (is.null(infilesAll[1])){
        return(FALSE)
    }
  
    # Define number of top clusters
    dir.create(outputDir)
    dirInputName <- dirName
    dirInputExpression <- paste0(dirInputName, "ExpressionData/")
    sigColumnIndex <- get_column_index(sigColumnName) # 5=p-value, 12=BH p-value, 13=FDR
    valueColumnIndex <- get_column_index(valueColumnName)
    summaryFileNameBase <- additionalFileName
    summaryFileNameExt <- extTag
    dirNameSplit <- unlist(str_split(dirName, "/"))
    summaryFileNameBase <- paste0(summaryFileNameBase, "Chart_Top", topTermLimit, "_", mode, "__", sigColumnName, "_", sigCutoff, "_", valueColumnName)
    summaryFileNameExt <- paste0(summaryFileNameExt,  "Chart_Top", topTermLimit, "_", mode, "__", sigColumnName, "_", sigCutoff, "_", valueColumnName)
  
    # Load DAVID Chart files
    # Step0. Get only the files that actually have contents
    infiles <- lapply(infilesAll, function(infile) {
        infileDF <- read.delim(infile, sep="\t", header=TRUE, comment.char="", fill=TRUE, stringsAsFactors=FALSE)
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
    getSigTerms <- lapply(infiles, function(infile) {
        tmp1 <- unlist(str_split(infile, "/"))
        tmpNameSplit <- unlist(str_split(tmp1[length(tmp1)], "__Chart.txt"))
        shortFileBaseName <- tmpNameSplit[1]
        originalSourceFile <- paste0(dirInputName, "/", shortFileBaseName, ".txt")
        DATA <- read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, fill=TRUE)
        lines <- DATA
        if(nrow(lines) < 1){
            return(FALSE)
        }
        tmpIDList <- lapply(seq_len(nrow(lines)), function(i){
            tmpSplit <- lapply(lines[i, ], function(lineItem) lineItem)
            if (tmpSplit[[sigColumnIndex]] == "" | is.na(tmpSplit[[sigColumnIndex]]) | grepl("^\\D", tmpSplit[[sigColumnIndex]]) | tmpSplit[[sigColumnIndex]] >= sigCutoff | tmpSplit[[10]] < 1) {
                return(NULL)
            } else {
                return(paste0(tmpSplit[[2]]))
            }
            return(NULL)
        })
        
        #Get not null elements
        tmpIDList <- tmpIDList[!vapply(tmpIDList, is.null, FUN.VALUE=logical(1))]
        tmpIDList <- unlist(tmpIDList, recursive=FALSE)
        
        # Cut off entries over the limit
        if(length(tmpIDList) > as.double(topTermLimit)){
            tmpIDList <- tmpIDList[seq_len(as.double(topTermLimit))]
        }
        
        tmp_ID2Term <- lapply(seq_len(nrow(lines)), function(i){
            tmpSplit <- lapply(lines[i, ], function(lineItem) lineItem)
            if (tmpSplit[[sigColumnIndex]] == "" | is.na(tmpSplit[[sigColumnIndex]]) | grepl("^\\D", tmpSplit[[sigColumnIndex]]) | tmpSplit[[sigColumnIndex]] >= sigCutoff | tmpSplit[[10]] < 1) {
                return(NULL)
            } else {
                tmpTerm <- paste0(tmpSplit[[1]], "|", tmpSplit[[2]])
                return(tmpTerm)
            }
        })
        #Get not null elements
        tmp_ID2Term <- tmp_ID2Term[!vapply(tmp_ID2Term, is.null, FUN.VALUE=logical(1))]
        tmp_ID2Term <- unlist(tmp_ID2Term, recursive=FALSE)
        # Cut off entries over the limit
        if(length(tmp_ID2Term) > as.double(topTermLimit)){
            tmp_ID2Term <- tmp_ID2Term[seq_len(as.double(topTermLimit))]
        }
        
        tmp_ID2Class <- lapply(seq_len(nrow(lines)), function(i){
            tmpSplit <- lapply(lines[i, ], function(lineItem) lineItem)
            if (tmpSplit[[sigColumnIndex]] == "" | is.na(tmpSplit[[sigColumnIndex]]) | grepl("^\\D", tmpSplit[[sigColumnIndex]]) | tmpSplit[[sigColumnIndex]] >= sigCutoff | tmpSplit[[10]] < 1) {
                return(NULL)
            } else {
                return(tmpSplit[[1]])
            }
        })
        #Get not null elements
        tmp_ID2Class <- tmp_ID2Class[!vapply(tmp_ID2Class, is.null, FUN.VALUE=logical(1))]
        tmp_ID2Class <- unlist(tmp_ID2Class, recursive=FALSE)
        # Cut off entries over the limit
        if(length(tmp_ID2Class) > as.double(topTermLimit)){
            tmp_ID2Class <- tmp_ID2Class[seq_len(as.double(topTermLimit))]
        }
        names(tmp_ID2Term) <- tmpIDList
        names(tmp_ID2Class) <- tmpIDList
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
        DATA <- tryCatch(read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, fill=TRUE), error=function(cond) {
            print("No lines in input file.")
            return(NULL)
        })
        if (nrow(DATA) > 0){
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
        pvalueMatrix_inner <- unlist(lapply(seq_len(length(x[["PValue"]])), function(y) {
            if (x[y, "PValue"] == "" | is.null(x[y, "PValue"]) | is.null(x[y, "Fold.Enrichment"]) | grepl("^\\D", x[y, "PValue"]) | (mode == "ALL" & x[y, "PValue"] >= sigCutoff) | x[y, "Fold.Enrichment"] < 1) {
                return(NULL)
            }
            return(-1*log10(as.double(x[y, "PValue"])))
        }))
        if(is.null(pvalueMatrix_inner)) {
            return(NULL)
        }
        names(pvalueMatrix_inner) <- unlist(lapply(seq_len(length(x[["PValue"]])), function(y) {
            if (x[y, "PValue"] == "" | is.null(x[y, "PValue"]) | is.null(x[y, "Fold.Enrichment"]) | grepl("^\\D", x[y, "PValue"]) | (mode == "ALL" & x[y, "PValue"] >= sigCutoff) | x[y, "Fold.Enrichment"] < 1) {
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
    file.create(paste0(outputDir, summaryFileName))
    SUMMARY <- file(paste0(outputDir, summaryFileName))
    fileHeaderNames <- unlist(fileHeaderNames)
    fileHeaderNames <- sort_by_file_number(fileHeaderNames)
    writeToSummaryHeader <- paste0("GROUP\tID\tTerms\t", paste0(fileHeaderNames, collapse="\t"), "\n")
    IDs <- unique(IDs)
    writeToSummary <- lapply(IDs, function(ID) {
        writeToSummaryLabel <- paste0(ID2Class[ID], "\t", ID)
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
        return(paste0(writeToSummaryLabel, "\t\t", writeToSummary2))
    })
    writeToSummary <- paste0(writeToSummary, collapse="\n")
    write(paste0(writeToSummaryHeader, writeToSummary), SUMMARY, append=TRUE)
    close(SUMMARY)

    # Create a network summary file for Chart
    ForNetworkFile <- paste0(summaryFileNameBase, "__ValueMatrix.ForNet")
    file.create(paste0(outputDir, ForNetworkFile))
    NETWORK <- file(paste0(outputDir, ForNetworkFile))
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
    write(paste0(writeToNetworkHeader, writeToNetwork), NETWORK, append=TRUE)
    close(NETWORK)
  
    # Create a gct file from ValueMatrix
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

###### Perform enrichment analysis ######
perform_CASRN_enrichment_analysis <- function(CASRNRef, outputBaseDir, outfileBase, mappedCASRNsFromProcess, funCat2Selected, CASRN2funCatTerm, funCatTerm2CASRN, funCat2CASRNCount, funCat2termCount, funCatTerm2CASRNCount, pvalueThresholdToDisplay, similarityThreshold=0.50, initialGroupMembership, multipleLinkageThreshold, EASEThreshold, nodeCutoff=10, enrichmentUUID){
    setName <- outfileBase
  
    # Define output file names
    outfileChart <- paste0(outputBaseDir, outfileBase, "__Chart.txt")
    outfileSimple <- paste0(outputBaseDir, outfileBase, "__ChartSimple.txt")
    outfileMatrix <- paste0(outputBaseDir, outfileBase, "__Matrix.txt")
    
    # Open connections
    OUTFILE <- file(outfileChart)
    SIMPLE <- file(outfileSimple)
    MATRIX <- file(outfileMatrix)
    
    # Create directories if they don't exist
    dir.create(paste0(outputBaseDir))
    
    # Initialize headers
    chartHeader <- "Category\tTerm\tCount\t%\tPValue\tCASRNs\tList Total\tPop Hits\tPop Total\tFold Enrichment\tBonferroni\tBenjamini\tFDR\n"
    simpleHeader <- "Category\tTerm\tCount\t%\tPValue\tFold Enrichment\tBenjamini\n"
    
    # Calculate EASE score
    inputCASRNs <- CASRNRef
    inputCASRNsCount <- length(inputCASRNs)
    mappedCASRNs <- mappedCASRNsFromProcess # Among the CASRNS, use only those included in the full Tox21 list

    # Populate sigTerm2CASRNMatrix
    funCat2SelectedProcessed_sigTerm2CASRNMatrix <- mclapply(names(funCat2Selected), mc.cores=CORES, mc.silent=FALSE, function(funCat){
        if (!is.null(funCat2Selected[[funCat]]) & funCat2Selected[[funCat]] != ""){
            funCatTerms <- names(funCatTerm2CASRN[[funCat]])
            tmp_sigTerm2CASRNMatrix <- lapply(funCatTerms, function(term){
                sigTermResults <- lapply(names(mappedCASRNs), function(CASRN) {
                    if (!is.null(funCatTerm2CASRN[[funCat]][[term]][[CASRN]])){
                        return(1)
                    }
                    return(NULL)
                })
                names(sigTermResults) <- lapply(names(mappedCASRNs), function(CASRN) {
                    if (!is.null(funCatTerm2CASRN[[funCat]][[term]][[CASRN]])){
                        return(CASRN)  
                    }
                    return(NULL)
                })
                sigTermResults <- sigTermResults[vapply(sigTermResults, length, FUN.VALUE=numeric(1)) > 0]
                return(sigTermResults)
            })
        }
        names(tmp_sigTerm2CASRNMatrix) <- lapply(funCatTerms, function(term) paste0(funCat, "|", term))
        tmp_sigTerm2CASRNMatrix <- tmp_sigTerm2CASRNMatrix[vapply(tmp_sigTerm2CASRNMatrix, length, FUN.VALUE=numeric(1)) > 0]
        if(length(tmp_sigTerm2CASRNMatrix) < 1){
            return(NULL)
        }
        return(tmp_sigTerm2CASRNMatrix)
    })
    funCat2SelectedProcessed_sigTerm2CASRNMatrix <- funCat2SelectedProcessed_sigTerm2CASRNMatrix[!vapply(funCat2SelectedProcessed_sigTerm2CASRNMatrix, is.null, FUN.VALUE=logical(1))]
    sigTerm2CASRNMatrix <- unlist(funCat2SelectedProcessed_sigTerm2CASRNMatrix, recursive=FALSE)

    # Populate datArray
    funCat2SelectedProcessed_datArray <- mclapply(names(funCat2Selected), mc.cores=CORES, mc.silent=FALSE, function(funCat){
        # Calculate the CASRN counts for the given categories
        if (!is.null(funCat2Selected[[funCat]]) & funCat2Selected[[funCat]] != ""){
            # Variables
            localTerms  <- lapply(names(mappedCASRNs), function(CASRN){
                if (!is.null(CASRN2funCatTerm[[CASRN]][[funCat]])){
                    return(names(CASRN2funCatTerm[[CASRN]][[funCat]]))
                }
                return(NULL)
            })
            localTerms <- localTerms[!vapply(localTerms, is.null, FUN.VALUE=logical(1))]
            totalCount <- length(localTerms)
            targetTotalTermCount <- length(localTerms)
            targetTotalCASRNInFunCatCount <- totalCount
            funCatTerms <- names(funCatTerm2CASRN[[funCat]]) 
            tmp_datArray <- lapply(funCatTerms, function(term){
                if (!is.null(funCatTerm2CASRN[[funCat]][term]) & funCatTerm2CASRN[[funCat]][term] != ""){ 
                    # This is a valid term, check if the CASRN count is more than 1
                    targetCASRNs <- lapply(names(mappedCASRNs), function(CASRN) {
                        if (!is.null(funCatTerm2CASRN[[funCat]][[term]][[CASRN]])){
                            return(CASRN)
                        }
                        return(NULL)
                    })
                    targetCASRNs <- targetCASRNs[!vapply(targetCASRNs, is.null, FUN.VALUE=logical(1))]
                    CASRNCount   <- length(targetCASRNs)
                    targetCASRNsRef   <- targetCASRNs
                    targetCASRNCount  <- CASRNCount
                    
                    # Calculate the EASE score
                    if (targetCASRNCount > 1){
                        np1 <- targetTotalCASRNInFunCatCount - 1
                        n11 <- targetCASRNCount - 1
                        npp <- funCat2CASRNCount[[funCat]]  
                        n1p <- funCatTerm2CASRNCount[[funCat]][[term]]
                        
                        # skip any under-represented terms
                        foldenrichment <- (targetCASRNCount/length(targetTotalCASRNInFunCatCount))/(n1p/npp)
                        datArrayLine  <- list(n11, (n1p-n11), (np1-n11), (npp-n1p-np1+n11))
                        return(datArrayLine)
                    }
                    return(NULL)
                }
                return(NULL)
            })
            tmp_datArray <- tmp_datArray[!vapply(tmp_datArray, is.null, FUN.VALUE=logical(1))]
            return(tmp_datArray)
        }
        return(NULL)
    })
    funCat2SelectedProcessed_datArray <- funCat2SelectedProcessed_datArray[!vapply(funCat2SelectedProcessed_datArray, is.null, FUN.VALUE=logical(1))]
    datArray <- unlist(funCat2SelectedProcessed_datArray, recursive=FALSE)

    # Populate annoArray
    funCat2SelectedProcessed_annoArray <- mclapply(names(funCat2Selected), mc.cores=CORES, mc.silent=FALSE, function(funCat){
        # Calculate the CASRN counts for the given categories
        if (!is.null(funCat2Selected[[funCat]]) & funCat2Selected[[funCat]] != ""){
            # Variables
            localTerms  <- lapply(names(mappedCASRNs), function(CASRN){
                if (!is.null(CASRN2funCatTerm[[CASRN]][[funCat]])){
                    return(names(CASRN2funCatTerm[[CASRN]][[funCat]]))
                }
            })
            localTerms <- localTerms[!vapply(localTerms, is.null, FUN.VALUE=logical(1))]
            totalCount <- length(localTerms)
            targetTotalTermCount <- length(localTerms)
            targetTotalCASRNInFunCatCount <- totalCount
            funCatTerms <- names(funCatTerm2CASRN[[funCat]]) 
            tmp_annoArray <- lapply(funCatTerms, function(term){
                if (!is.null(funCatTerm2CASRN[[funCat]][term]) & funCatTerm2CASRN[[funCat]][term] != ""){ 
                    # This is a valid term, check if the CASRN count is more than 1
                    targetCASRNs <- lapply(names(mappedCASRNs), function(CASRN) {
                        if (!is.null(funCatTerm2CASRN[[funCat]][[term]][[CASRN]])){
                            return(CASRN)
                        }
                        return(NULL)
                    })
                    targetCASRNs <- targetCASRNs[!vapply(targetCASRNs, is.null, FUN.VALUE=logical(1))]
                    CASRNCount <- length(targetCASRNs)
                    targetCASRNsRef <- targetCASRNs
                    targetCASRNCount <- CASRNCount
                    
                    # Calculate the EASE score
                    if (targetCASRNCount > 1){
                        np1 <- targetTotalCASRNInFunCatCount - 1
                        n11 <- targetCASRNCount - 1
                        npp <- funCat2CASRNCount[[funCat]]  
                        n1p <- funCatTerm2CASRNCount[[funCat]][[term]]
                        
                        # Skip any under-represented terms
                        foldenrichment <- (targetCASRNCount/length(targetTotalCASRNInFunCatCount))/(n1p/npp)
                        # Truncate pvalue digits after decimal to 15
                        annoArrayLine <- c(funCat, term, targetCASRNCount, round((targetCASRNCount/inputCASRNsCount*100), digits=15), 1, paste(unlist(unname(vapply(targetCASRNsRef, paste, FUN.VALUE=character(1), collapse=", "))), collapse=', '), targetTotalCASRNInFunCatCount, n1p, npp, (targetCASRNCount/targetTotalCASRNInFunCatCount)/(n1p/npp))
                        return(annoArrayLine)
                    }
                    return(NULL)
                }
                return(NULL)
            })
            tmp_annoArray <- tmp_annoArray[!vapply(tmp_annoArray, is.null, FUN.VALUE=logical(1))]
            return(tmp_annoArray)
        }
        return(NULL)
    })
    funCat2SelectedProcessed_annoArray <- unlist(funCat2SelectedProcessed_annoArray, recursive=FALSE)
    funCat2SelectedProcessed_annoArray <- funCat2SelectedProcessed_annoArray[!vapply(funCat2SelectedProcessed_annoArray, is.null, FUN.VALUE=logical(1))]
    annoArray <- funCat2SelectedProcessed_annoArray

    # Save the data file -> create "RINPUT" but as a data frame here
    RINPUT_df <- do.call(rbind, datArray)
    rownames(RINPUT_df) <- seq_len(nrow(RINPUT_df))
    colnames(RINPUT_df) <- c("X1", "X2", "X3", "X4")
    
    # Handle if bad RINPUT file
    if(nrow(RINPUT_df) < 2){
        sortedHeaderTerms <- sigTerm2CASRNMatrix[order(unlist(sigTerm2CASRNMatrix), decreasing=FALSE)]
        # Clean out NA values
        sortedHeaderTerms <- sortedHeaderTerms[lengths(sortedHeaderTerms) != 0]
        matrixHeader <- paste0(names(sortedHeaderTerms), collapse='\t')
        matrixPrintToFile <- lapply(names(mappedCASRNs), function(tmpCasrn){ 
            tmpMatrixHeaderProcess <- lapply(names(sortedHeaderTerms), function(tmpMatrixHeader){
                if (!is.null(sigTerm2CASRNMatrix[[tmpMatrixHeader]][[tmpCasrn]]) & tmpMatrixHeader != "NA"){
                    return("1")
                }
                else{
                    return("0")
                }
            })
            return(paste0(tmpCasrn, "\t", paste0(tmpMatrixHeaderProcess, collapse="\t")))
        })
        matrixOutput <- paste0("CASRN\t", matrixHeader, "\n\n", paste0(matrixPrintToFile, collapse="\n"))
        
        # Write blank files to chart, simple, and matrix
        writeLines(chartHeader, OUTFILE)
        writeLines(simpleHeader, SIMPLE)
        writeLines(matrixOutput, MATRIX)
        close(OUTFILE)
        close(SIMPLE)
        close(MATRIX)
        # Open and create a blank cluster file
        outfileCluster <- paste0(outputBaseDir, outfileBase, "__Cluster.txt")
        file.create(outfileCluster)
        return(FALSE)
    }
    # Calculate results of the fisher test - P-value, Bonferroni, BY, and FDR
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
        sortedHeaderTerms <- sigTerm2CASRNMatrix[order(unlist(sigTerm2CASRNMatrix), decreasing=FALSE)]
        # Clean out NA values
        sortedHeaderTerms <- sortedHeaderTerms[lengths(sortedHeaderTerms) != 0]
        matrixHeader <- paste0(names(sortedHeaderTerms), collapse='\t')
        matrixPrintToFile <- lapply(names(mappedCASRNs), function(tmpCasrn){ 
            tmpMatrixHeaderProcess <- lapply(names(sortedHeaderTerms), function(tmpMatrixHeader){
                if (!is.null(sigTerm2CASRNMatrix[[tmpMatrixHeader]][[tmpCasrn]]) & tmpMatrixHeader != "NA"){
                    return("1")
                }
                else{
                    return("0")
                }
            })
            return(paste0(tmpCasrn, "\t", paste0(tmpMatrixHeaderProcess, collapse="\t")))
        })
        matrixOutput <- paste0("CASRN\t", matrixHeader, "\n\n", paste0(matrixPrintToFile, collapse="\n"))
        # Write blank files to chart, simple, and matrix
        writeLines(chartHeader, OUTFILE)
        writeLines(simpleHeader, SIMPLE)
        writeLines(matrixOutput, MATRIX)
        close(OUTFILE)
        close(SIMPLE)
        close(MATRIX)
        # Open and create a blank cluster file
        outfileCluster <- paste0(outputBaseDir, outfileBase, "__Cluster.txt")
        file.create(outfileCluster)
        # Initialize db connection pool
        poolStatus <- dbPool(
            drv=RPostgres::Postgres(),
            dbname=tox21queue$database,
            host=tox21queue$host,
            user=tox21queue$uid,
            password=tox21queue$pwd,
            port=tox21queue$port,
            idleTimeout=3600000
        )
        # Set step flag for each set name
        query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=-1 WHERE uuid='", enrichmentUUID, "' AND setname='", setName, "';"), id="fetchStatus")
        outp <- dbExecute(poolStatus, query)
        # Check if no errors
        query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
        requestCancel <- dbGetQuery(poolStatus, query)
        # Close pool
        poolClose(poolStatus)
        if(nrow(requestCancel) > 0){
            print(paste0("Cancelling request: ", enrichmentUUID))
            return(FALSE)
        }
        return(FALSE)
    }
    
    # Remove empty elements in annoArray
    annoArray <- annoArray[lengths(annoArray) > 0L]
    annoArray <- lapply(seq_len(length(annoArray)), function(annoArrayIndex){
        return(list(
            annoArray[[annoArrayIndex]][[1]], # annotation category
            annoArray[[annoArrayIndex]][[2]], # annotation term name
            as.numeric(annoArray[[annoArrayIndex]][[3]]), # count
            as.numeric(annoArray[[annoArrayIndex]][[4]]), # %
            ROutputData[[annoArrayIndex]][["p.value"]], # update the p-value
            annoArray[[annoArrayIndex]][[6]], # casrn list
            as.numeric(annoArray[[annoArrayIndex]][[7]]), # list total
            as.numeric(annoArray[[annoArrayIndex]][[8]]), # pop hits
            as.numeric(annoArray[[annoArrayIndex]][[9]]), # pop total
            as.numeric(annoArray[[annoArrayIndex]][[10]]), # fold enrichment
            ROutputData[[annoArrayIndex]][["bonferroni"]], # Bonferroni
            ROutputData[[annoArrayIndex]][["by"]], # Benjamini
            ROutputData[[annoArrayIndex]][["fdr"]] # add FDR to the array 
        ))
    })
    term2Contents <- lapply(seq_len(length(annoArray)), function(annoArrayIndex) paste0(annoArray[[annoArrayIndex]]))
    names(term2Contents) <- lapply(seq_len(length(annoArray)), function(annoArrayIndex) paste0(annoArray[[annoArrayIndex]][1], "|", annoArray[[annoArrayIndex]][2]))
    term2Pvalue <- lapply(seq_len(length(annoArray)), function(annoArrayIndex) ROutputData[[annoArrayIndex]][["p.value"]])
    names(term2Pvalue) <- lapply(seq_len(length(annoArray)), function(annoArrayIndex) paste0(annoArray[[annoArrayIndex]][1], "|", annoArray[[annoArrayIndex]][2]))

    # Sort by the p-values across multiple funCat
    sortedFunCatTerms <- term2Pvalue[order(unlist(term2Pvalue), decreasing=FALSE)]
    sortedFunCatTermsCount <- length(sortedFunCatTerms)

    # Write to chart File
    writeToChart <- unlist(lapply(names(sortedFunCatTerms), function(funCatTerm){ 
        if (term2Pvalue[[funCatTerm]] <= pvalueThresholdToDisplay){
            return(paste0(term2Contents[[funCatTerm]], collapse='\t'))
        } else {
            return(NULL)
        }
    }))
    writeToChart <- writeToChart[!vapply(writeToChart, is.null, FUN.VALUE=logical(1))]
    writeLines(paste0(chartHeader, paste0(writeToChart, collapse="\n")), OUTFILE)
    close(OUTFILE)
    
    # Write to simple chart file
    sortFunCatProcess <- lapply(names(sortedFunCatTerms), function(funCatTerm){ 
        if (term2Pvalue[[funCatTerm]] <= pvalueThresholdToDisplay){ 
            tmpSplit <- term2Contents[[funCatTerm]]
            if (tmpSplit[[10]] > 1){
                return(list(tmpSplit[[1]], tmpSplit[[2]], tmpSplit[[3]], tmpSplit[[4]], tmpSplit[[5]], tmpSplit[[10]], tmpSplit[[12]]))
            }
            return(NULL)
        }
        return(NULL)
    })
    sortFunCatProcess <- do.call(rbind.data.frame, sortFunCatProcess)
    rownames(sortFunCatProcess) <- seq_len(nrow(sortFunCatProcess))
    colnames(sortFunCatProcess) <- c("Category", "Term", "Count", "Percent", "PValue", "Fold Enrichment", "Benjamini")
    sortFunCatProcess <- sortFunCatProcess[order(sortFunCatProcess$PValue), ] # sort by pvalue
    sortFunCatProcess <- do.call(rbind, by(sortFunCatProcess, sortFunCatProcess["Category"], head, n=nodeCutoff)) #remove any entries if more than the cutoff value
    sortFunCatProcess <- sortFunCatProcess %>% arrange(PValue) # Sort simple chart contents by pvalue
    sortFunCatProcess <- apply(sortFunCatProcess, 1, function(x) paste0(x, collapse="\t"))
    writeLines(paste0(simpleHeader, paste0(sortFunCatProcess, collapse="\n")), SIMPLE)
    close(SIMPLE)

    # Write to matrix file
    sortedHeaderTerms <- sigTerm2CASRNMatrix[order(unlist(sigTerm2CASRNMatrix), decreasing=FALSE)]
    sortedHeaderTerms <- sortedHeaderTerms[lengths(sortedHeaderTerms)!=0]
    matrixHeader <- paste0(names(sortedHeaderTerms), collapse='\t')
    matrixPrintToFile <- lapply(names(mappedCASRNs), function(tmpCasrn){ 
        tmpMatrixHeaderProcess <- lapply(names(sortedHeaderTerms), function(tmpMatrixHeader){
            if (!is.null(sigTerm2CASRNMatrix[[tmpMatrixHeader]][[tmpCasrn]]) & tmpMatrixHeader != "NA"){
                return("1")
            }
            else{
                return("0")
            }
        })
        return(paste0(tmpCasrn, "\t", paste0(tmpMatrixHeaderProcess, collapse="\t")))
    })
    matrixOutput <- paste0("CASRN\t", matrixHeader, "\n\n", paste0(matrixPrintToFile, collapse="\n"))
    writeLines(matrixOutput, MATRIX)
    close(MATRIX)

    # Perform functional term clustering
    # Calculate enrichment score
    df <- read.delim(outfileChart, sep="\t", comment.char="", quote="", stringsAsFactors=FALSE)
    res <- kappa_cluster(x=df, outputBaseDir=outputBaseDir, outfileBase=outfileBase, sortedFunCatTerms=sortedFunCatTerms, sigTerm2CASRNMatrix=sigTerm2CASRNMatrix, sortedFunCatTermsCount=sortedFunCatTermsCount, inputCASRNsCount=inputCASRNsCount, similarityThreshold=similarityThreshold, initialGroupMembership=initialGroupMembership, multipleLinkageThreshold=multipleLinkageThreshold, EASEThreshold=EASEThreshold, term2Pvalue=term2Pvalue, term2Contents=term2Contents, enrichmentUUID=enrichmentUUID)
}

kappa_cluster <- function(x, deg=NULL, useTerm=FALSE, cutoff=0.5, overlap=0.5, minSize=5, escore=3, outputBaseDir, outfileBase, sortedFunCatTerms, sigTerm2CASRNMatrix, sortedFunCatTermsCount, inputCASRNsCount=0, similarityThreshold=0.50, initialGroupMembership, multipleLinkageThreshold=0.5, EASEThreshold=1.0, term2Pvalue, term2Contents, enrichmentUUID) {
    # Perform functional term clustering
    # Update status file
    # Connect to DB to get status info
    poolStatus <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Set step flag for each set name
    query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=3 WHERE uuid='", enrichmentUUID, "' AND setname='", outfileBase, "' AND step<>-1;"), id="fetchStatus")
    outp <- dbExecute(poolStatus, query)
    # Check if no errors
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
    requestCancel <- dbGetQuery(poolStatus, query)
    # Close pool
    poolClose(poolStatus)
    if(nrow(requestCancel) > 0){
        print(paste0("Cancelling request: ", enrichmentUUID))
        return(FALSE)
    }
  
    # Step#1: Calculate kappa score
    posTermCASRNCount <- lapply(names(sortedFunCatTerms), function(funCatTerm) length(sigTerm2CASRNMatrix[[funCatTerm[[1]]]]))
    # Set names
    names(posTermCASRNCount) <- names(sortedFunCatTerms)
    # Calculate kappa score for each term pair
    sortedFunCatTerms <- names(sortedFunCatTerms)
    termpair2kappaOverThreshold <- mclapply ((seq_len((sortedFunCatTermsCount-1))), mc.cores=CORES, mc.silent=FALSE, function(i) {
        termpair2kappaOverThresholdInner <- lapply (seq((i+1), sortedFunCatTermsCount), function(j) {
            #calculate_kappa_statistics 
            posTerm1Total <- posTermCASRNCount[[sortedFunCatTerms[i]]]
            posTerm2Total <- posTermCASRNCount[[sortedFunCatTerms[j]]]
            negTerm1Total <- inputCASRNsCount - posTerm1Total # note that the total is inputCASRNsCount not the mapped total
            negTerm2Total <- inputCASRNsCount - posTerm2Total # note that the total is inputCASRNsCount not the mapped total
            # Get number of chemicals that are shared or not for term1 and term2
            sharedTerms <- intersect(names(sigTerm2CASRNMatrix[[sortedFunCatTerms[i]]]), names(sigTerm2CASRNMatrix[[sortedFunCatTerms[j]]]))
            term1term2 <- length(sharedTerms)
            term1only <- length(names(sigTerm2CASRNMatrix[[sortedFunCatTerms[i]]])) - length(sharedTerms)
            term2only <- length(names(sigTerm2CASRNMatrix[[sortedFunCatTerms[j]]])) - length(sharedTerms)
            term1term2Non <- inputCASRNsCount - term1term2 - term1only - term2only
            # Calculate the kappa score 
            # http://david.abcc.ncifcrf.gov/content.jsp?file=linear_search.html
            Oab <- (term1term2 + term1term2Non)/inputCASRNsCount
            Aab <- ((posTerm1Total * posTerm2Total) + (negTerm1Total * negTerm2Total))/(inputCASRNsCount * inputCASRNsCount)
            if (Aab != 1) {
                Kappa <- as.double(sprintf("%.2f", (Oab - Aab)/(1 - Aab)))
                if (Kappa > similarityThreshold) {
                    iTerm <- paste0(sortedFunCatTerms[i])
                    jTerm <- paste0(sortedFunCatTerms[j])
                    ijFrame <- data.frame(iTerm=paste0(jTerm), jTerm=paste0(iTerm), stringsAsFactors=FALSE)
                    colnames(ijFrame) <- list(iTerm, jTerm)
                    return(ijFrame)
                }
                return(NULL)
            }
            return(NULL)
        })
        termpair2kappaOverThresholdInner <- termpair2kappaOverThresholdInner[!vapply(termpair2kappaOverThresholdInner, is.null, FUN.VALUE=logical(1))]
        if(length(termpair2kappaOverThresholdInner) > 0){
            termpair2kappaOverThresholdInner <- unlist(termpair2kappaOverThresholdInner, recursive=FALSE)
            return(termpair2kappaOverThresholdInner)
        }
        return(NULL)
    })
    termpair2kappaOverThreshold <- unlist( termpair2kappaOverThreshold[!vapply(termpair2kappaOverThreshold, is.null, FUN.VALUE=logical(1))], recursive=FALSE )
    termpair2kappaOverThreshold <- tapply(unlist(termpair2kappaOverThreshold, use.names=FALSE), rep(names(termpair2kappaOverThreshold), lengths(termpair2kappaOverThreshold)), FUN=c)

    # Step#2: Create qualified initial seeding groups
    # Each term could form a initial seeding group (initial seeds) as long as it has close relationships (kappa > 0.35 or any designated number) with more than > 2 or any designated number of other members. 
    # Update status file
    # Connect to DB to get status info
    poolStatus <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Set step flag for each set name
    query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=4 WHERE uuid='", enrichmentUUID, "' AND setname='", outfileBase, "' AND step<>-1;"), id="fetchStatus")
    outp <- dbExecute(poolStatus, query)
    # Check if no errors
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
    requestCancel <- dbGetQuery(poolStatus, query)
    # Close pool
    poolClose(poolStatus)
    if(nrow(requestCancel) > 0){
        print(paste0("Cancelling request: ", enrichmentUUID))
        return(FALSE)
    }
    term2sToPass <- NULL
    if(length(termpair2kappaOverThreshold) > 0){
        termpair2kappaOverThreshold <- termpair2kappaOverThreshold[order(names(termpair2kappaOverThreshold), decreasing=TRUE)]
        # Prepend sortedFunCatTerms to each element in list
        term2sToPass <- lapply(seq_len(sortedFunCatTermsCount), function(i) append(termpair2kappaOverThreshold[[sortedFunCatTerms[i]]], sortedFunCatTerms[i], after=0))
        names(term2sToPass) <- mclapply(seq_len(sortedFunCatTermsCount), mc.cores=CORES, mc.silent=FALSE, function(i) sortedFunCatTerms[i])
    }
    qualifiedSeeds <- mclapply(seq_len(sortedFunCatTermsCount), mc.cores=CORES, mc.silent=FALSE, function(i){
        # Seed condition #1: initial group membership
        if (!is.null(termpair2kappaOverThreshold[[sortedFunCatTerms[i]]]) & length(termpair2kappaOverThreshold[[sortedFunCatTerms[i]]]) >= (initialGroupMembership-1)) {
            # Seed condition #2: majority of the members 
            results_calculate_percentage_of_membership_over_threshold <- calculate_percentage_of_membership_over_threshold (termpair2kappaOverThreshold, term2sToPass[[sortedFunCatTerms[i]]])
            over_percentage <- unlist(results_calculate_percentage_of_membership_over_threshold["overPercentage"])
            term2sRef <- results_calculate_percentage_of_membership_over_threshold["term2s"]
            if (as.numeric(over_percentage) > as.numeric(multipleLinkageThreshold)) {
                return(unlist(unname(term2sRef))) # this seed group is qualified
            }
            return(NULL)
        }
        return(NULL)
    })
    # remove empty nested lists
    qualifiedSeeds <- lapply(qualifiedSeeds, function(innerList) innerList[vapply(innerList, length, FUN.VALUE=numeric(1)) > 0])
    qualifiedSeeds <- qualifiedSeeds[!vapply(qualifiedSeeds, is.null, FUN.VALUE=logical(1))]

    #  Step#3: Iteratively merge qualifying seeds
    # Update status file
    # Connect to DB to get status info
    poolStatus <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Set step flag for each set name
    query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=5 WHERE uuid='", enrichmentUUID, "' AND setname='", outfileBase, "' AND step<>-1;"), id="fetchStatus")
    outp <- dbExecute(poolStatus, query)
    # Check if no errors
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
    requestCancel <- dbGetQuery(poolStatus, query)
    # Close pool
    poolClose(poolStatus)
    if(nrow(requestCancel) > 0){
        print(paste0("Cancelling request: ", enrichmentUUID))
        return(FALSE)
    }
    remainingSeeds <- qualifiedSeeds
    finalGroups <- vector("list", length(remainingSeeds))
    finalGroupsIndex <- 1
    # TODO: make this more efficient (no while loops)
    while(!is.null(unlist(remainingSeeds[1]))){
        currentSeedRef <- remainingSeeds[[1]]
        remainingSeeds <- remainingSeeds[-1]
        while(TRUE) {
            results_get_the_best_seed <- get_the_best_seed (currentSeedRef, remainingSeeds, newSeeds, multipleLinkageThreshold, finalGroupsIndex)
            # update the current reference seed ref with new seeds
            remainingSeeds <- results_get_the_best_seed[["remainingSeedsRef"]]
            newSeeds <- results_get_the_best_seed[["newSeedRef"]]
            seedStatus <- results_get_the_best_seed[["finished"]]
            if(seedStatus == 0) {
                break
            } else {
                currentSeedRef <- newSeeds
            }
        }
        # if there is no more merging possible, add the current seeds to the final groups
        finalGroups[[finalGroupsIndex]] <- currentSeedRef
        finalGroupsIndex <- finalGroupsIndex + 1
    }
    # Remove null elements
    finalGroups <- lapply(finalGroups, function(innerList){
        if(length(innerList) > 0) {
            return(innerList)
        }
        return(NULL)
    })
    finalGroups <- finalGroups[!vapply(finalGroups, is.null, FUN.VALUE=logical(1))]

    # Step#4: Calculate enrichment score and print out the results
    # Update status file
    # Connect to DB to get status info
    poolStatus <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Set step flag for each set name
    query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=6 WHERE uuid='", enrichmentUUID, "' AND setname='", outfileBase, "' AND step<>-1;"), id="fetchStatus")
    outp <- dbExecute(poolStatus, query)
    # Check if no errors
    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
    requestCancel <- dbGetQuery(poolStatus, query)
    # Close pool
    poolClose(poolStatus)
    if(nrow(requestCancel) > 0){
        print(paste0("Cancelling request: ", enrichmentUUID))
        return(FALSE)
    }
    outfileCluster <- paste0(outputBaseDir, outfileBase, "__Cluster.txt")
    CLUSTER <- file(outfileCluster)
    clusterHeader <- "Category\tTerm\tCount\t%\tPValue\tCASRNs\tList Total\tPop Hits\tPop Total\tFold Enrichment\tBonferroni\tBenjamini\tFDR\n"
    EASEScore <- list()
    if(length(finalGroups) > 0){
        EASEScore <- lapply(seq_len(length(finalGroups)), function(i) calculate_Enrichment_Score(finalGroups[i], term2Pvalue))
        EASEScore <- unlist(EASEScore, recursive=FALSE)
    }
  
    # Sort
    sortedIndex <- finalGroups
    names(sortedIndex) <- EASEScore
    sortedIndexFloat <- unlist(lapply(names(sortedIndex), function(x) as.numeric(x)))
    sortedIndex <- sortedIndex[order(sortedIndexFloat, decreasing=TRUE)]
    writeToClusterFinal <- NULL
    if(length(sortedIndex) > 0){
        writeToClusterFinal <- lapply(seq_len(length(sortedIndex)), function(myIndex) {
            if(length(finalGroups[[myIndex]] > 0)){
                # sort terms again by p-value
                finalGroups2Pvalue <- lapply(sortedIndex[[myIndex]], function(term) term2Pvalue[[term]])
                finalGroups2Pvalue <- unlist(finalGroups2Pvalue, recursive=FALSE)
                names(finalGroups2Pvalue) <- sortedIndex[[myIndex]]
                sortedFunCatTerms <- finalGroups2Pvalue[order(unlist(finalGroups2Pvalue), decreasing=FALSE)]
                writeTermsToCluster <- lapply(names(sortedFunCatTerms), function(myTerm) return(paste0(term2Contents[[myTerm]], collapse="\t")))
                writeToCluster <- paste0(writeTermsToCluster, collapse="\n")
                return(writeToCluster)
            }
        })
    }
    writeToClusterFinal <- lapply(seq_len(length(writeToClusterFinal)), function(x) paste0("Annotation Cluster ", x, "\tEnrichment Score: ", names(sortedIndex)[x], "\n", clusterHeader, writeToClusterFinal[[x]], "\n"))
    writeToClusterFinal <- paste0(writeToClusterFinal, collapse="\n")
    write(writeToClusterFinal, CLUSTER)
    close(CLUSTER)
    return(1)
}

calculate_Enrichment_Score <- function(tmp_groupRef, term2PvalueRef) {
    groupRef <- tmp_groupRef[[1]]
    EASESum <- Reduce("+", unlist(lapply(groupRef, function(termTmp) {
        term <- termTmp[[1]]
        if(is.null(term2PvalueRef[[term]][[1]])) {
            return(16)
        } else {
            if (term2PvalueRef[[term]][[1]] == 0) {
                return(16)
            } else {
                return(-log(term2PvalueRef[[term]][[1]])/log(10))
            }
        }
    })))
    enrichmentScore <- EASESum / length(groupRef)
    return(enrichmentScore)
}

calculate_percentage_of_membership_over_threshold <- function(termpair2kappaOverThresholdRef, term2s) {
    # calculate 
    passedPair <- unlist(lapply(seq_len((length(term2s)-1)), function(i) {
        passedPairInner <- lapply(seq((i+1), (length(term2s))), function(j) {
            if(term2s[j] %in% termpair2kappaOverThresholdRef[[term2s[i]]]) {
                return(1)
            } 
            return(NULL)
        })
        passedPairInner <- unlist(passedPairInner[!vapply(passedPairInner, is.null, FUN.VALUE=logical(1))])
        return(passedPairInner)
    }))
    passedPair <- passedPair[!vapply(passedPair, is.null, FUN.VALUE=logical(1))]
    passedPair <- length(passedPair)
    # Use n choose k to calculate total number of unique pairs
    totalPairs <- choose(length(term2s), 2)
    return(list(overPercentage=(passedPair/totalPairs), term2s=term2s))
}

get_the_best_seed <- function(currentSeedRef, remainingSeedsRef, newSeedRef=list(), multipleLinkageThreshold, currentSeedIndex) {
    bestOverlapping <- 0
    bestSeedIndex <- 0
    currentSeedTerms <- currentSeedRef
    currentSeedTermCount <- length(currentSeedTerms)
    if(length(remainingSeedsRef) > 1){
        bestIndex <- lapply(seq_len((length(remainingSeedsRef) - 1)), function(i) {
            # calculate the overlapping
            secondSeedTerms <- remainingSeedsRef[[i]]
            commonCount <- length(intersect(secondSeedTerms, currentSeedTerms))
            totalCount <- length(secondSeedTerms)
            overlapping <- 2 * commonCount / (currentSeedTermCount + totalCount)
            if (overlapping > multipleLinkageThreshold) {
                return(list(overlapping, i))
            }
            return(NULL)
        })
        bestIndex <- bestIndex[!vapply(bestIndex, is.null, FUN.VALUE=logical(1))]
        if(!is.null(bestIndex) & length(bestIndex) >= 1) {
            bestIndex <- do.call(rbind.data.frame, bestIndex)
            colnames(bestIndex) <- c("overlapping", "seedindex")
            bestIndex <- bestIndex %>% slice_max(overlapping) %>% slice(1) # Get the largest overlapping value
            bestOverlapping <- bestIndex[[1]]
            bestSeedIndex <- bestIndex[[2]]
        }
    }
    if (bestOverlapping == 0) {
        # no more merging is possible
        return(list(remainingSeedsRef=remainingSeedsRef, newSeedRef=newSeedRef, bestSeedIndex=bestSeedIndex, finished=0))
    } else {
        # best mergable seed found
        newSeedRef <- union(currentSeedTerms, remainingSeedsRef[[bestSeedIndex]])
        # splice
        remainingSeedsRef <- remainingSeedsRef[-bestSeedIndex]
        return(list(remainingSeedsRef=remainingSeedsRef, newSeedRef=newSeedRef, bestSeedIndex=bestSeedIndex, finished=1))
    }
}

# Fetch all annotations in the Tox21 Enricher database for given CASRNs.
getAnnotations <- function(enrichmentUUID="-1", annoSelectStr="MESH=checked,PHARMACTIONLIST=checked,ACTIVITY_CLASS=checked,ADVERSE_EFFECT=checked,INDICATION=checked,KNOWN_TOXICITY=checked,MECH_LEVEL_1=checked,MECH_LEVEL_2=checked,MECH_LEVEL_3=checked,MECHANISM=checked,MODE_CLASS=checked,PRODUCT_CLASS=checked,STRUCTURE_ACTIVITY=checked,TA_LEVEL_1=checked,TA_LEVEL_2=checked,TA_LEVEL_3=checked,THERAPEUTIC_CLASS=checked,TISSUE_TOXICITY=checked,DRUGBANK_ATC=checked,DRUGBANK_ATC_CODE=checked,DRUGBANK_CARRIERS=checked,DRUGBANK_ENZYMES=checked,DRUGBANK_TARGETS=checked,DRUGBANK_TRANSPORTERS=checked,CTD_CHEM2DISEASE=checked,CTD_CHEM2GENE_25=checked,CTD_CHEMICALS_DISEASES=checked,CTD_CHEMICALS_GENES=checked,CTD_CHEMICALS_GOENRICH_CELLCOMP=checked,CTD_CHEMICALS_GOENRICH_MOLFUNCT=checked,CTD_CHEMICALS_PATHWAYS=checked,CTD_GOSLIM_BIOPROCESS=checked,CTD_PATHWAY=checked,HTS_ACTIVE=checked,LEADSCOPE_TOXICITY=checked,MULTICASE_TOX_PREDICTION=checked,TOXCAST_ACTIVE=checked,TOXINS_TARGETS=checked,TOXPRINT_STRUCTURE=checked,TOXREFDB=checked,", nodeCutoff=10) {
    # Connect to db
    poolInput <- dbPool(
        drv=RPostgres::Postgres(),
        dbname=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        password=tox21queue$pwd,
        port=tox21queue$port,
        idleTimeout=3600000
    )
    # Get begin time for request
    beginTime <- Sys.time()
    # Set begin time in transaction table
    query <- sqlInterpolate(ANSI(), paste0("UPDATE transaction SET timestamp_started='", beginTime, "' WHERE uuid='", enrichmentUUID, "';"), id="updateTransactionStart")
    outp <- dbExecute(poolInput, query)
    # Close pool
    poolClose(poolInput)
    
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
        poolStatus <- dbPool(
            drv=RPostgres::Postgres(),
            dbname=tox21queue$database,
            host=tox21queue$host,
            user=tox21queue$uid,
            password=tox21queue$pwd,
            port=tox21queue$port,
            idleTimeout=3600000
        )
        # Set step flag for each set name
        query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=2 WHERE uuid='", enrichmentUUID, "' AND setname='", setName, "' AND step<>-1;"), id="fetchStatus")
        outp <- dbExecute(poolStatus, query)
        # Check if no errors
        query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
        requestCancel <- dbGetQuery(poolStatus, query)
        # Close pool
        poolClose(poolStatus)
        if(nrow(requestCancel) > 0){
            print(paste0("Cancelling request: ", enrichmentUUID))
            return(FALSE)
        }
        
        # Get the corresponding annotations for each input CASRN
        annotations <- lapply(inputCASRNs, function(CASRN){
            # Connect to db
            poolMatrix <- dbPool(
                drv=RPostgres::Postgres(),
                dbname=tox21config$database,
                host=tox21config$host,
                user=tox21config$uid,
                password=tox21config$pwd,
                port=tox21config$port,
                idleTimeout=3600000
            )
            # Grab matrix
            queryMatrix <- sqlInterpolate(ANSI(), paste0("SELECT annotation FROM annotation_matrix WHERE casrn='", CASRN, "';"))
            outpMatrix <- tryCatch({
                dbGetQuery(poolMatrix, queryMatrix)
            }, error=function(e){
                print(e)
                return(NULL)
            })
            poolClose(poolMatrix)
            fetchedCASRNs <- outpMatrix[, 1]
            
            # Split up list of annotation IDs
            fetchedCASRNsList <- unlist(str_split(fetchedCASRNs, "\\|"))
            fetchedCASRNsList <- fetchedCASRNsList[lapply(fetchedCASRNsList, length) > 0]
            fetchedCASRNsList <- fetchedCASRNsList[-length(fetchedCASRNsList)]
            return(fetchedCASRNsList)
        })
        # Set list names to CASRNs
        names(annotations) <- inputCASRNs
        
        # Read status file and update step
        # Connect to DB to get status info
        poolStatus <- dbPool(
            drv=RPostgres::Postgres(),
            dbname=tox21queue$database,
            host=tox21queue$host,
            user=tox21queue$uid,
            password=tox21queue$pwd,
            port=tox21queue$port,
            idleTimeout=3600000
        )
        # Set step flag for each set name
        query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=3 WHERE uuid='", enrichmentUUID, "' AND setname='", setName, "' AND step<>-1;"), id="fetchStatus")
        outp <- dbExecute(poolStatus, query)
        # Check if no errors
        query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
        requestCancel <- dbGetQuery(poolStatus, query)
        # Close pool
        poolClose(poolStatus)
        if(nrow(requestCancel) > 0){
            print(paste0("Cancelling request: ", enrichmentUUID))
            return(FALSE)
        }
        
        # Create output files for each CASRN in the Set
        individualMatrix <- mclapply(inputCASRNs, mc.cores=CORES, mc.silent=FALSE, function(CASRN){
            print(paste0("Creating output file at: ", paste0(outDir, "/", setName, "__", CASRN, ".txt")))
            file.create(paste0(outDir, "/", setName, "__", CASRN, ".txt"))
            OUTPUT <- file(paste0(outDir, "/", setName, "__", CASRN, ".txt"))
            outAnnotationList <- paste0(annotations[[CASRN]], collapse="\n")
            
            # Fetch actual term names from IDs
            fetchedTerms <- lapply(annotations[[CASRN]], function(annotation){
                # Connect to db
                poolMatrix <- dbPool(
                    drv=RPostgres::Postgres(),
                    dbname=tox21config$database,
                    host=tox21config$host,
                    user=tox21config$uid,
                    password=tox21config$pwd,
                    port=tox21config$port,
                    idleTimeout=3600000
                )
                queryTerm <- sqlInterpolate(ANSI(), paste0("SELECT term FROM annotation_matrix_terms WHERE id=", annotation, ";"))
                outpTerm <- dbGetQuery(poolMatrix, queryTerm)
                poolClose(poolMatrix)
                fetchedTerm <- outpTerm[[1, 1]]
                
                # Check if the fetched term is in the selected annotations
                fetchedTermSplit <- unlist(str_split(fetchedTerm, "__"), recursive=FALSE)[1]
                if(fetchedTermSplit %in% annoSelect){
                    # replace "__" with "\t" for better utility and return
                    return(gsub("__", "\t", fetchedTerm))
                }
                # else, return null if we deselected the annotation set for this given annotation
                return(NULL)
            })
            fetchedTerms <- fetchedTerms[!vapply(fetchedTerms, is.null, FUN.VALUE=logical(1))]
            fetchedTerms <- unlist(fetchedTerms, recursive=FALSE)
          
            # Write annotations to output file
            if(!is.null(fetchedTerms)){
                writeLines(fetchedTerms, OUTPUT)
            } else { # blank file if no matches
                writeLines("No matching annotations.", OUTPUT)
            }
            close(OUTPUT)
            return(fetchedTerms)
        })
        # Set list names to CASRNs
        # Merge class and annotation names
        individualMatrix <- lapply(individualMatrix, function(x) fixedInnerList <- unlist(lapply(x, function(y) gsub("\t", "\\|", y))))
        names(individualMatrix) <- inputCASRNs
        combinedMatrix <- unlist(unname(individualMatrix), recursive=FALSE)

        # Read status file and update step
        # Connect to DB to get status info
        poolStatus <- dbPool(
            drv=RPostgres::Postgres(),
            dbname=tox21queue$database,
            host=tox21queue$host,
            user=tox21queue$uid,
            password=tox21queue$pwd,
            port=tox21queue$port,
            idleTimeout=3600000
        )
        # Set step flag for each set name
        query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=4 WHERE uuid='", enrichmentUUID, "' AND setname='", setName, "' AND step<>-1;"), id="fetchStatus")
        outp <- dbExecute(poolStatus, query)
        # Check if no errors
        query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
        requestCancel <- dbGetQuery(poolStatus, query)
        # Close pool
        poolClose(poolStatus)
        if(nrow(requestCancel) > 0){
            print(paste0("Cancelling request: ", enrichmentUUID))
            return(FALSE)
        }
        
        # Create matrix file for all CASRNs in set
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
    })
    annotationMatrix <- annotationMatrix[!vapply(annotationMatrix, is.null, FUN.VALUE=logical(1))]
    if(length(annotationMatrix) < 1){
        # Return error message
        return("No lines available in any input file. Cannot fetch annotations.")
    }
    
    # zip result files
    if(dir.exists(outDir)){
        zipDir <- dir(outDir, recursive=TRUE, include.dirs=TRUE)
        filesToZip <- unlist(lapply(zipDir, function(x) paste0(outDir, "/", x)))
        system2("cd", paste0(outDir, "/ ; zip -r9X ./tox21enricher_", enrichmentUUID, ".zip ./*")) 
    } else { # Return with error if did not complete. Do not update in database
        return(-1) 
    }
    
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
    # Get ending time
    finishTime <- Sys.time()
    
    # Set finish time in transaction table
    query <- sqlInterpolate(ANSI(), paste0("UPDATE transaction SET timestamp_finished='", finishTime, "' WHERE uuid='", enrichmentUUID, "';"), id="transactionUpdateFinished")
    outp <- dbExecute(poolUpdate, query)

    # Close pool
    poolClose(poolUpdate)
    return(200)
} 

# Keeps looping and looks for unfinished requests
queue <- function(){
    plan('multicore')
    while(TRUE){
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
        # Get only unfinished requests ("finished" flag is set to 0 and not currently being processed by another thread)
        query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE finished=0 AND lock=0 AND error IS NULL;"), id="createQueueEntry")
        outp <- dbGetQuery(poolQueue, query)
        # Close pool
        poolClose(poolQueue)
        
        if(nrow(outp) > 0){
            # Sort requests from database by index value
            inputSets <- outp[order(outp$index), ] 
        
            # Only process first five requests in queue at a time to conserve resources
            upperBound <- nrow(inputSets)
            if(nrow(inputSets) > 5){
                upperBound <- 5
            }
            #TODO
            future({
                mclapply(seq_len(upperBound), mc.cores=CORES, mc.silent=FALSE, function(input){
                #lapply(seq_len(upperBound), function(input){
                    # Get data from queue table entry
                    queueFile <- inputSets[input, ]
                    mode <- queueFile[1, 1]
                    enrichmentUUID <- queueFile[1, 2]
                    annoSelectStr <- queueFile[1, 3]
                    nodeCutoff <- queueFile[1, 4]
                    status_code <- 500
                      
                    # Connect to DB to get status info
                    poolStatus <- dbPool(
                        drv=RPostgres::Postgres(),
                        dbname=tox21queue$database,
                        host=tox21queue$host,
                        user=tox21queue$uid,
                        password=tox21queue$pwd,
                        port=tox21queue$port,
                        idleTimeout=3600000
                    )
                    
                    # Set lock for current request so it won't be reprocessed
                    query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET lock=1 WHERE uuid='", enrichmentUUID, "';"), id="setLock")
                    outp <- dbExecute(poolStatus, query)
                      
                    # Get status entry for corresponding queue entry
                    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM status WHERE uuid='", enrichmentUUID, "';"), id="fetchStatus")
                    outp <- dbGetQuery(poolStatus, query)
                    statusFiles <- outp
                
                    # Read status entry(ies) and change flag to signify enrichment has started
                    lapply(seq_len(nrow(statusFiles)), function(i){
                        tmpSetName <- statusFiles[i, "setname"]
                        query <- sqlInterpolate(ANSI(), paste0("UPDATE status SET step=1 WHERE uuid='", enrichmentUUID, "' AND setname='", tmpSetName, "' AND step<>-1;"), id="fetchStatus")
                        outp <- dbExecute(poolStatus, query)
                    })
                    
                    # Check if no errors
                    query <- sqlInterpolate(ANSI(), paste0("SELECT * FROM queue WHERE uuid='", enrichmentUUID, "' AND cancel=1;"), id="fetchStatus")
                    requestCancel <- dbGetQuery(poolStatus, query)
                    # Close pool
                    poolClose(poolStatus)
                    if(nrow(requestCancel) > 0){
                        return(FALSE)
                    }
                      
                    # Perform enrichment analysis or fetch relevant annotations
                    if(mode == "annotation") {
                        status_code <- getAnnotations(enrichmentUUID=enrichmentUUID, annoSelectStr=annoSelectStr, nodeCutoff=nodeCutoff)
                    } else { # else query R API server 
                        status_code <- performEnrichment(enrichmentUUID=enrichmentUUID, annoSelectStr=annoSelectStr, nodeCutoff=nodeCutoff)
                    } 
                    
                    # Connect to DB to set appropriate finishing flags
                    poolFinished <- dbPool(
                        drv=RPostgres::Postgres(),
                        dbname=tox21queue$database,
                        host=tox21queue$host,
                        user=tox21queue$uid,
                        password=tox21queue$pwd,
                        port=tox21queue$port,
                        idleTimeout=3600000
                    )
                    
                    # Check if request was cancelled
                    query <- sqlInterpolate(ANSI(), paste0("SELECT cancel FROM queue WHERE uuid='", enrichmentUUID, "';"), id="checkCancel")
                    outp <- dbGetQuery(poolFinished, query)
                      
                    cancelValue <- outp[1, "cancel"]
                    if(cancelValue == 1){
                        status_code <- -1
                    }
                      
                    # Upon success
                    if (status_code == 200){
                        # Set flag for queue
                        query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET finished=1 WHERE uuid='", enrichmentUUID, "';"), id="deleteEntries")
                        outp <- dbExecute(poolFinished, query)
                    } else if (status_code == -1) {
                        print(paste0("Request cancelled for ", enrichmentUUID, ". Deleting..."))
                        # Set flag for queue
                        query <- sqlInterpolate(ANSI(), paste0("UPDATE queue SET finished=1 WHERE uuid='", enrichmentUUID, "';"), id="deleteEntries")
                        outp <- dbExecute(poolFinished, query)
                    } else { # Else, generate error file for reference
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
                    poolClose(poolFinished)
                })
            }, seed=TRUE)
        }
        # Wait
        Sys.sleep(2)
    }
}

# Run queue on startup
queue()


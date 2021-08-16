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
library(rjson)
library(RPostgreSQL)
library(stringr)
library(tidyverse)
library(uuid)
library(xlsx)

################## Code to run on startup, not part of API endpoints ##################

#Current version of the application
tox21config <- config::get("tox21enricher")
APP_VERSION <- tox21config$appversion
APP_DIR <- tox21config$appdir

print(paste0("! Tox21 Enricher Plumber API version ", APP_VERSION, "."))

# Define info for connecting to PostgreSQL Tox21 Enricher database on server startup

pool <- dbPool(
  drv = dbDriver("PostgreSQL",max.con = 100),
  dbname = tox21config$database,
  host = tox21config$host,
  user = tox21config$uid,
  password = tox21config$pwd,
  idleTimeout = 3600000
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

# Base annotations
DSSTox2name           <- list()
DSSTox2CASRN          <- list()
CASRN2DSSTox          <- list()
tmp_DSSTox2name       <- list()
tmp_DSSTox2CASRN      <- list()
tmp_CASRN2DSSTox      <- list()

# DrugMatrix annotations
CASRN2funCatTerm      <- list()
funCatTerm2CASRN      <- list()
funCat2CASRN          <- list()
term2funCat           <- list()

# Load base Annotations
baseAnnotations <- apply(outpChemDetail, 1, function(i){
  if (i["testsubstance_chemname"] == "") {
    DSSTox2name[[i["dtxrid"]]]  <<- i["testsubstance_chemname"]
  }
  else {
    DSSTox2name[[i["dtxrid"]]]  <<- "";
  }
  if (i["casrn"] != "") {
    DSSTox2CASRN[[i["dtxrid"]]] <<- i["casrn"]
    CASRN2DSSTox[[i["casrn"]]] <<- list() # instantiate with list()
    CASRN2DSSTox[[i["casrn"]]][i["dtxrid"]] <<- 1 # populate with 1
  }
})

print("! Finished loading base annotations.")

# Load DrugMatrix Annotations
# CASRN2funCatTerm
CASRN2funCatTerm_lv1 <- lapply(split(outpAnnotations, outpAnnotations$casrn), function(x){
  return(split(x, x$annoclassname)) 
})
CASRN2funCatTerm <- lapply(CASRN2funCatTerm_lv1, function(x){
  return(lapply(x, function(y){
    inner_CASRN2funCatTerm <- lapply(y$annoterm, function(z) 1)
    names(inner_CASRN2funCatTerm) <- y$annoterm
    return(inner_CASRN2funCatTerm)
  }))
})

# funCatTerm2CASRN
funCatTerm2CASRN_lv1 <- lapply(split(outpAnnotations, outpAnnotations$annoclassname), function(x){
  return(split(x, x$annoterm))
})
funCatTerm2CASRN <- lapply(funCatTerm2CASRN_lv1, function(x){
  return(lapply(x, function(y) {
    inner_funCatTerm2CASRN <- lapply(y$casrn, function(z) 1) 
    names(inner_funCatTerm2CASRN) <- (y$casrn)
    return(inner_funCatTerm2CASRN)
  }))
})

# funCat2CASRN
funCat2CASRN <- lapply(split(outpAnnotations, outpAnnotations$annoclassname), function(x) {
  return(split(x, x$casrn))
})

# term2funCat
term2funCat <- lapply(split(outpAnnotations, outpAnnotations$annoterm), function(x) {
  return(split(x, x$annoclassname))
})

print("! Finished loading DrugMatrix annotations.")
print("! Ready to accept connections.")

#######################################################################################

#################################### API endpoints ####################################

### SECTION 1: DEALING WITH THE QUEUE ###

#* Place enrichment request in queue
#* @param mode
#* @param enrichmentUUID
#* @param annoSelectStr
#* @param nodeCutoff
#* @get /queue
queue <- function(mode="", enrichmentUUID="-1", annoSelectStr="MESH=checked,PHARMACTIONLIST=checked,ACTIVITY_CLASS=checked,ADVERSE_EFFECT=checked,INDICATION=checked,KNOWN_TOXICITY=checked,MECH_LEVEL_1=checked,MECH_LEVEL_2=checked,MECH_LEVEL_3=checked,MECHANISM=checked,MODE_CLASS=checked,PRODUCT_CLASS=checked,STRUCTURE_ACTIVITY=checked,TA_LEVEL_1=checked,TA_LEVEL_2=checked,TA_LEVEL_3=checked,THERAPEUTIC_CLASS=checked,TISSUE_TOXICITY=checked,DRUGBANK_ATC=checked,DRUGBANK_ATC_CODE=checked,DRUGBANK_CARRIERS=checked,DRUGBANK_ENZYMES=checked,DRUGBANK_TARGETS=checked,DRUGBANK_TRANSPORTERS=checked,CTD_CHEM2DISEASE=checked,CTD_CHEM2GENE_25=checked,CTD_CHEMICALS_DISEASES=checked,CTD_CHEMICALS_GENES=checked,CTD_CHEMICALS_GOENRICH_CELLCOMP=checked,CTD_CHEMICALS_GOENRICH_MOLFUNCT=checked,CTD_CHEMICALS_PATHWAYS=checked,CTD_GOSLIM_BIOPROCESS=checked,CTD_PATHWAY=checked,HTS_ACTIVE=checked,LEADSCOPE_TOXICITY=checked,MULTICASE_TOX_PREDICTION=checked,TOXCAST_ACTIVE=checked,TOXINS_TARGETS=checked,TOXPRINT_STRUCTURE=checked,TOXREFDB=checked,", nodeCutoff=10){
  # async
  #future_promise({
    queueDir <- paste0(APP_DIR, "Queue/")
    queueDirContents <- Sys.glob(paste0(APP_DIR, "Queue/*__queue__*"))

    # Regenerate 0__queue__default file if missing
    if(("0__queue__default" %in% queueDirContents) == FALSE) {
      file.create(paste0(queueDir, "0__queue__default"))
      queueDirContents <- Sys.glob(paste0(APP_DIR, "Queue/*__queue__*"))
    }
    
    queueDirNumbers <- lapply(queueDirContents, function(queueFile){
      #strip path
      noPathFile <- unlist(str_split(queueFile, paste0(APP_DIR, "Queue/")))[2]
      return(as.integer(unlist(str_split(noPathFile, "__queue__"))[1]))
    })
    
    QUEUE_ORDER <- max(sapply(queueDirNumbers, max)) + 1
    
    # Reset QUEUE_ORDER to 1 if too big
    #if(QUEUE_ORDER >= .Machine$integer.max){
    #  QUEUE_ORDER <<- 1
    #}
    
    # Create queue file in Queue dir
    
    file.create(paste0(queueDir, QUEUE_ORDER, "__queue__", enrichmentUUID))
    queueFile <- file(paste0(queueDir, QUEUE_ORDER, "__queue__", enrichmentUUID))
    writeLines(paste0(mode, "\t", enrichmentUUID, "\t", annoSelectStr, "\t", nodeCutoff), queueFile)
    close(queueFile)
    #QUEUE_ORDER <<- QUEUE_ORDER + 1
  #})
}

#* Get queue position for given enrichment request
#* @param transactionId
#* @get /getQueuePos
getQueuePos <- function(transactionId="-1"){
  queueDir <- paste0(APP_DIR, "Queue/")
  queueDirContents <- Sys.glob(paste0(APP_DIR, "Queue/*__queue__*"))
  
  # Sort
  queueDirContents <- sort(queueDirContents)
  
  queueDirIDs <- lapply(queueDirContents, function(queueFile){
    #strip path
    noPathFile <- unlist(str_split(queueFile, paste0(APP_DIR, "Queue/")))[2]
    if(unlist(str_split(noPathFile, "__queue__"))[2] != "default"){
      return(unlist(str_split(noPathFile, "__queue__"))[2])
    } else {
      return(NULL)
    }
  })
  queueDirIDs <- unlist(queueDirIDs[!sapply(queueDirIDs, is.null)])
  
  index <- 1
  for(i in queueDirIDs) {
    if(i == transactionId) {
      return(index)
    } else {
      index <- index + 1
    }
  }
  return("complete")
}

#* Check if enrichment process has terminated for given request
#* @param transactionId UUID of the request
#* @get /finishedRequest
finishedRequest <- function(res, req, transactionId){
  # async
  #future_promise({
  queueDir <- paste0(APP_DIR, "Queue/")
  checkIfQueueFile <- Sys.glob(paste0(queueDir, "[0-9]*__queue__", transactionId))
  if(length(checkIfQueueFile) > 0){
    return(FALSE)
  } else {
    return(TRUE)
  }
  #})
}

#* Check if error file exists for given request
#* @param transactionId UUID of the request
#* @get /hasError
hasError <- function(res, req, transactionId){
  # async
  #future_promise({
  queueDir <- paste0(APP_DIR, "Queue/")
  if (file.exists(paste0(queueDir, "error__", transactionId))){
    # Get error message from file
    errorFile <- unlist(readLines(paste0(queueDir, "error__", transactionId)))
    # Delete error file
    unlink(paste0(queueDir, "error__", transactionId))
    return(errorFile)
  }
  return(FALSE)
  #})
}

#* Cancel enrichment process for given UUID
#* @param transactionId UUID of the request
#* @get /cancelEnrichment
cancelEnrichment <- function(res, req, transactionId){
  # async
  #future_promise({
  inDir <- paste0(APP_DIR, "Input/")
  outDir <- paste0(APP_DIR, "Output/")
  queueDir <- paste0(APP_DIR, "Queue/")
  requestToCancel <- Sys.glob(paste0(queueDir, "[0-9]*__queue__", transactionId), dirmark=FALSE)
  requestToCancelError <- Sys.glob(paste0(queueDir, "[0-9]*__error__", transactionId), dirmark=FALSE)

  # If queue file exists, delete it
  if(length(requestToCancel) > 0){
    msg <- unlink(requestToCancel[1])
  } else {
    # Check if error file exists if queue file is gone and delete that too
    if(length(requestToCancelError) > 0){
      unlink(requestToCancelError[1])
    }
  }
  
  # Connect to db
  
  poolCancel <- dbPool(
    drv = dbDriver("PostgreSQL", max.con = 100),
    dbname = tox21config$database,
    host = tox21config$host,
    user = tox21config$uid,
    password = tox21config$pwd,
    idleTimeout = 3600000
  )
  
  # Update database to show that request was canceled
  #query <- sqlInterpolate(ANSI(), paste0("DELETE FROM enrichment_list WHERE id='", transactionId, "';"), id="cancelEnrichment")
  query <- sqlInterpolate(ANSI(), paste0("UPDATE enrichment_list SET timestamp_finish='cancelled' WHERE id='", transactionId, "';"), id="cancelEnrichment")
  outp <- dbGetQuery(poolCancel, query)
  
  # Delete Input/Output files on filesystem
  unlink(paste0(inDir, transactionId), recursive=TRUE)
  unlink(paste0(outDir, transactionId), recursive=TRUE)
  
  return(TRUE)
  #})
}

#* Get timestamps for given enrichment
#* @param transactionId
#* @get /getTimestamp
getTimestamp <- function(res, req, transactionId="-1"){
  # Connect to db
  
  poolUpdate <- dbPool(
    drv = dbDriver("PostgreSQL", max.con = 100),
    dbname = tox21config$database,
    host = tox21config$host,
    user = tox21config$uid,
    password = tox21config$pwd,
    idleTimeout = 3600000
  )
  
  # update database with ending timestamp for enrichment
  query <- sqlInterpolate(ANSI(), paste0("SELECT timestamp_start, timestamp_finish FROM enrichment_list WHERE id='", transactionId, "';"), id="getFromDb")
  outp <- dbGetQuery(poolUpdate, query)
  
  return(outp)
}

#* Fetch all annotations in the Tox21 Enricher database for given CASRNs.
#* @param enrichmentUUID UUID for Input/Output directory on local machine, generated by Tox21 Enricher application.
#* @param annoSelectStr String, comma-delimited, containing all enabled annotations for this enrichment process. Passed from Tox21 Enricher application.
#* @get /getAnnotations
getAnnotations <- function(res, req, enrichmentUUID="-1", annoSelectStr="MESH=checked,PHARMACTIONLIST=checked,ACTIVITY_CLASS=checked,ADVERSE_EFFECT=checked,INDICATION=checked,KNOWN_TOXICITY=checked,MECH_LEVEL_1=checked,MECH_LEVEL_2=checked,MECH_LEVEL_3=checked,MECHANISM=checked,MODE_CLASS=checked,PRODUCT_CLASS=checked,STRUCTURE_ACTIVITY=checked,TA_LEVEL_1=checked,TA_LEVEL_2=checked,TA_LEVEL_3=checked,THERAPEUTIC_CLASS=checked,TISSUE_TOXICITY=checked,DRUGBANK_ATC=checked,DRUGBANK_ATC_CODE=checked,DRUGBANK_CARRIERS=checked,DRUGBANK_ENZYMES=checked,DRUGBANK_TARGETS=checked,DRUGBANK_TRANSPORTERS=checked,CTD_CHEM2DISEASE=checked,CTD_CHEM2GENE_25=checked,CTD_CHEMICALS_DISEASES=checked,CTD_CHEMICALS_GENES=checked,CTD_CHEMICALS_GOENRICH_CELLCOMP=checked,CTD_CHEMICALS_GOENRICH_MOLFUNCT=checked,CTD_CHEMICALS_PATHWAYS=checked,CTD_GOSLIM_BIOPROCESS=checked,CTD_PATHWAY=checked,HTS_ACTIVE=checked,LEADSCOPE_TOXICITY=checked,MULTICASE_TOX_PREDICTION=checked,TOXCAST_ACTIVE=checked,TOXINS_TARGETS=checked,TOXPRINT_STRUCTURE=checked,TOXREFDB=checked,") {
  # async
  #future_promise({
    
  
    # Get list of selected annotations
    annoSelect <- unlist(strsplit(annoSelectStr,"=checked,", fixed=TRUE), recursive=FALSE)
    
    # Get input sets
    inDir <- paste0(APP_DIR, "Input/", enrichmentUUID)    # Directory for input files for enrichment set
    outDir <- paste0(APP_DIR, "Output/", enrichmentUUID)  # Directory for output files for enrichment set
    
    inputSets <- Sys.glob(paste0(inDir,"/*"))

    
    # Connect to db
    
    poolMatrix <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21config$database,
      host = tox21config$host,
      user = tox21config$uid,
      password = tox21config$pwd,
      idleTimeout = 3600000
    )
    
    #TODO: make multithreaded
    annotationMatrix <- lapply(inputSets, function(infile){
      # Get set name
      setNameSplit <- str_split(infile, "/")[[1]]
      setNameSplit2 <- str_split(setNameSplit[length(setNameSplit)], "\\.")[[1]]
      setName <- setNameSplit2[1]
      
      # Read input files
      input <- tryCatch({
        read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors = FALSE, header = FALSE)
      }, error=function(e){
        return(NULL)
      })

      if(length(input) < 1) {
        return(NULL)
      }
      
      # Get just the CASRNs
      inputCASRNs <- input[, 1]
    
      # Get the corresponding annotations for each input CASRN
      annotations <- lapply(inputCASRNs, function(CASRN){
        
        # Grab matrix
        queryMatrix <- sqlInterpolate(ANSI(), paste0("SELECT annotation FROM annotation_matrix WHERE casrn = '", CASRN, "';"))
        outpMatrix <- tryCatch({
          dbGetQuery(poolMatrix, queryMatrix)
        }, error=function(e){
          return(NULL)
        })
        
        fetchedCASRNs <- outpMatrix[, 1]
        
        # Split up list of annotation IDs
        fetchedCASRNsList <- str_split(fetchedCASRNs, "\\|")[[1]]
        fetchedCASRNsList <- fetchedCASRNsList[lapply(fetchedCASRNsList, length) > 0]
        fetchedCASRNsList <- fetchedCASRNsList[-length(fetchedCASRNsList)]
        
        return(fetchedCASRNsList)
          
      })
      # Set list names to CASRNs
      names(annotations) <- inputCASRNs
      
      # Create output files for each CASRN in the Set
      individualMatrix <- lapply(inputCASRNs, function(CASRN){
        file.create(paste0(outDir, "/", setName, "__", CASRN, ".txt"))
        OUTPUT <- file(paste0(outDir, "/", setName, "__", CASRN, ".txt"))
        
        outAnnotationList <- paste0(annotations[[CASRN]], collapse="\n")
        
        # Fetch actual term names from IDs
        fetchedTerms <- lapply(annotations[[CASRN]], function(annotation){
          queryTerm <- sqlInterpolate(ANSI(), paste0("SELECT term FROM annotation_matrix_terms WHERE id = ", annotation, ";"))
          outpTerm <- dbGetQuery(poolMatrix, queryTerm)
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
        fetchedTerms <- fetchedTerms[!sapply(fetchedTerms, is.null)]
        fetchedTerms <- unlist(fetchedTerms, recursive=FALSE)

        # Write annotations to output file
        if(is.null(fetchedTerms) == FALSE){
          writeLines(fetchedTerms, OUTPUT)
        } else { # blank file if no matches
          writeLines("No matching annotations.", OUTPUT)
        }
        close(OUTPUT)
        
        return(fetchedTerms)
      })
      # Set list names to CASRNs
      names(individualMatrix) <- inputCASRNs
      
      combinedMatrix <- unlist(unname(individualMatrix), recursive = FALSE)
                               
      # Create matrix file for all CASRNs in set
      file.create(paste0(outDir, "/", setName, "__FullMatrix.txt"))
      MATRIX <- file(paste0(outDir, "/", setName, "__FullMatrix.txt"))
      
      matrixHeader <- paste(combinedMatrix, collapse='\t')
      matrixOutput <- paste0("CASRN\t", matrixHeader, "\n\n")
      
      for (CASRN in inputCASRNs) {
        matrixOutput <- paste0(matrixOutput, CASRN, sep="")
        for (matrixItem in combinedMatrix){
          if (matrixItem %in% individualMatrix[[CASRN]]){
            matrixOutput <- paste0(matrixOutput, "\t1", sep="")
          }
          else{
            matrixOutput <- paste0(matrixOutput, "\t0", sep="")
          } 
        }
        matrixOutput <- paste0(matrixOutput, "\n", sep="")
      }
      
      # Write to matrix file
      write(matrixOutput, MATRIX, append=TRUE)
      return(TRUE)
      
    })
    annotationMatrix <- annotationMatrix[!sapply(annotationMatrix, is.null)]
      
    if(length(annotationMatrix) < 1){
      res$status = 500
      res$body = paste0("No lines available in any input file. Cannot fetch annotations.")
      return(res) #return error message  
    }
    
    # zip result files
    zipDir <- dir(outDir, recursive=TRUE, include.dirs=TRUE)
    filesToZip <- unlist(lapply(zipDir, function(x) paste0(outDir, "/", x)))
    # try with system call instead vvv
    system(paste0("cd ", outDir, "/ ; zip -r9X ./tox21enricher_", enrichmentUUID, ".zip ./*"))
    
    # Close pool connection to db when done accessing
    poolClose(poolMatrix)
  #})
} 

### SECTION 2: REQUIRED BY CLIENT APPLICATION INITIALIZATION ###

#* Ping API
#* @get /ping
ping <- function(res, req){
  return(TRUE)
}

#* Check Tox21 Enricher version
#* @get /getAppVersion
getAppVersion <- function(res, req){
  #future_promise({
  return(APP_VERSION)
  #})
}

#* Get annotation classes/types (internal use only)
#* @get /initAnnotations
initAnnotations <- function(res, req){
  # async
  #future_promise({
    
    # Connect to db
    
    poolAnnotations <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21config$database,
      host = tox21config$host,
      user = tox21config$uid,
      password = tox21config$pwd,
      idleTimeout = 3600000
    )
    annoClassQuery <- sqlInterpolate(ANSI(), "SELECT annoclassname, annotype, annodesc FROM annotation_class;", id = "getAnnotationClasses")
    annoClassOutp <- dbGetQuery(poolAnnotations, annoClassQuery)
    
    # Phase out old CTD annotations (TODO: remove these altogether)
    annoClassOutpRemove <- c("CTD_CHEM2DISEASE", "CTD_CHEM2GENE_25", "CTD_PATHWAY")
    annoClassOutp2 <- annoClassOutp[!(annoClassOutp$annoclassname %in% annoClassOutpRemove), ]
    rownames(annoClassOutp2) <- 1:nrow(annoClassOutp2)
    
    poolClose(poolAnnotations)
    return(annoClassOutp2)
  #})
}

#* Get total number of enrichments (internal use only)
#* @get /total
total <- function(res, req){
  # async
  #future_promise({
    
    # Connect to db
    
    poolTotal <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21config$database,
      host = tox21config$host,
      user = tox21config$uid,
      password = tox21config$pwd,
      idleTimeout = 3600000
    )
    totalQuery <- sqlInterpolate(ANSI(), "SELECT COUNT(*) FROM enrichment_list;", id = "getTotalEnrichment")
    totalOutp <- dbGetQuery(poolTotal, totalQuery)
    return(totalOutp)
  #})
}

### SECTION 3: PREPARING CHEMICAL DATA FOR ENRICHMENT ###

#* Get data for provided SMILES string (substructure) (internal use only)
#* @param input Input string, either SMILES or CASRN (if re-enriching)
#* @param reenrich Boolean value to let the API know if this is a re-enrichment or not
#* @get /substructure
substructure <- function(res, req, input, reenrich=FALSE){
  # async
  #future_promise({
    # Connect to db
    
    poolSubstructure <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21config$database,
      host = tox21config$host,
      user = tox21config$uid,
      password = tox21config$pwd,
      idleTimeout = 3600000
    )
    
    # Sanitize input, convert InChI strings to SMILES
    # TODO: make it so the client doesn't have to request this, just do it all here
    if (grepl("InChI=", input, fixed=TRUE)) {
      input <- convertInchi(inchi=input)
    }
    
    substructureQuery <- ""
    
    if(reenrich == FALSE){
      substructureQuery <- sqlInterpolate(ANSI(), paste0("SELECT * FROM mols_2 WHERE m @> CAST('", input, "' AS mol);"), id = "substructureResults")
    } else {
      substructureQuery <- sqlInterpolate(ANSI(), paste0("SELECT * FROM mols_2 WHERE casrn='", input, "';"), id = "substructureResults")
    }
    
    substructureOutp <- dbGetQuery(poolSubstructure, substructureQuery)
    poolClose(poolSubstructure)
    return(substructureOutp)
  #})
}

#* Get data for provided SMILES string (similarity) (internal use only)
#* @param input SMILES Input string
#* @param threshold Tanimoto similarity threshold
#* @get /similarity
similarity <- function(res, req, input, threshold){
  # async
  #future_promise({
    
    # Connect to db
    
    poolSimilarity <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21config$database,
      host = tox21config$host,
      user = tox21config$uid,
      password = tox21config$pwd,
      idleTimeout = 3600000
    )
    
    # Sanitize input, convert InChI strings to SMILES
    if (grepl("InChI=", input, fixed=TRUE)) {
      input <- convertInchi(inchi=input)
    }
    
    queryTanimoto <- sqlInterpolate(ANSI(), paste0("set rdkit.tanimoto_threshold=", threshold, ";"), id = "tanimotoResults")
    outpTanimoto <- dbGetQuery(poolSimilarity, queryTanimoto)
  
    similarityQuery <- sqlInterpolate(ANSI(), paste0("SELECT * FROM get_mfp2_neighbors('", input, "');"), id = "similarityResults")
    similarityOutp <- dbGetQuery(poolSimilarity, similarityQuery)
    poolClose(poolSimilarity)
    return(similarityOutp)
  #})
}

#* Get additional data for provided CASRN (internal use only)
#* @param input CASRN Input string
#* @get /casrnData
casrnData <- function(res, req, input){
  # async
  #future_promise({
    
    # Connect to db
    
    poolCasrn <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21config$database,
      host = tox21config$host,
      user = tox21config$uid,
      password = tox21config$pwd,
      idleTimeout = 3600000
    )
  
    casrnQuery <- sqlInterpolate(ANSI(), paste0("SELECT iupac_name, smiles, dtxsid, dtxrid, mol_formula, mol_weight, inchis, inchikey FROM chemical_detail WHERE CASRN LIKE '", input, "';"), id = "casrnResults")
    casrnOutp <- dbGetQuery(poolCasrn, casrnQuery)
    poolClose(poolCasrn)
    return(casrnOutp)
  #})
}

#* Convert provided InChI string to SMILES (internal use only)
#* @param inchi
#* @get /inchiToSmiles
inchiToSmiles <- function(res, req, inchi){
  #async
  #future_promise({
    # Connect to db
    
    poolInchi <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21config$database,
      host = tox21config$host,
      user = tox21config$uid,
      password = tox21config$pwd,
      idleTimeout = 3600000
    )
    inchiQuery <- sqlInterpolate(ANSI(), paste0("SELECT smiles FROM chemical_detail WHERE inchis = '", inchi, "';"), id = "convertInchi")
    inchiOutp <- dbGetQuery(poolInchi, inchiQuery)
    poolClose(poolInchi)
    return(inchiOutp[[1]])
  #})
}

# Same as above, but done from within the internal Tox21 Enricher substructure/similarity searches. we don't want to do this asynchronously because it is just a function and not an API endpoint.
convertInchi <- function(res, req, inchi){
  # Connect to db
  
  poolInchi <- dbPool(
    drv = dbDriver("PostgreSQL", max.con = 100),
    dbname = tox21config$database,
    host = tox21config$host,
    user = tox21config$uid,
    password = tox21config$pwd,
    idleTimeout = 3600000
  )
  inchiQuery <- sqlInterpolate(ANSI(), paste0("SELECT smiles FROM chemical_detail WHERE inchis = '", inchi, "';"), id = "convertInchi")
  inchiOutp <- dbGetQuery(poolInchi, inchiQuery)
  poolClose(poolInchi)
  return(inchiOutp[[1]])
}

#* Use RDKit to generate chemical structure images from a list of molecules (internal use only)
#* @param input
#* @get /generateStructures
generateStructures <- function(res, req, input){
  # async
  #future_promise({
    
    # Connect to db
    
    poolSvg <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21config$database,
      host = tox21config$host,
      user = tox21config$uid,
      password = tox21config$pwd,
      idleTimeout = 3600000
    )
  
    tmpSplit <- unlist(str_split(input, "\n"))
    
    structures <- lapply(tmpSplit, function(x){
      tmpSplit2 <- unlist(str_split(x, "__"))
      m <- tmpSplit2[2]
      
      svgQuery <- sqlInterpolate(ANSI(), paste0("SELECT mol_to_svg('", m, "');"), id = "generateStructures")
      svgOutp <- dbGetQuery(poolSvg, svgQuery)
      
      return(svgOutp[1, "mol_to_svg"])
    })
    names(structures) <- lapply(tmpSplit, function(x){
      tmpSplit2 <- unlist(str_split(x, "__"))
      return(tmpSplit2[1])
    })
    
    poolClose(poolSvg)
    return(structures)
  #})
}

### SECTION 4: FILE CREATION AND SERVING ###

#* Serve text file to client
#* @serializer contentType list(type="application/text")
#* @param transactionId
#* @param filename
#* @param subDir
#* @get /serveFileText
serveFileText <- function(res, req, transactionId, filename, subDir){
  #future_promise({
    outDir <- paste0(APP_DIR, subDir, "/", transactionId, "/")
    fileToServe <- paste0(outDir, filename)
    readBin(fileToServe, "raw", n=file.info(fileToServe)$size)
  #})
}

#* Serve manual to client
#* @serializer contentType list(type="application/pdf")
#* @get /serveManual
serveManual <- function(res, req){
  #future_promise({
    filename <- paste0(APP_DIR, "docs/Tox21Enricher_Manual_v", APP_VERSION,".pdf")
    fileToServe <- paste0(APP_DIR, "docs/Tox21Enricher_Manual_v", APP_VERSION, ".pdf")
    readBin(fileToServe, "raw", n=file.info(fileToServe)$size)
  #})
}

#* Create input files on filesystem
#* @param transactionId UUID for given request
#* @param enrichmentSets User's input sets
#* @param setNames Input set names
#* @get /createInput
createInput <- function(res, req, transactionId, enrichmentSets, setNames, mode, nodeCutoff, annoSelectStr){
  # async
  #future_promise({

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
      innerList <- innerList[!sapply(innerList, is.null)]
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
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21config$database,
      host = tox21config$host,
      user = tox21config$uid,
      password = tox21config$pwd,
      idleTimeout = 3600000
    )
    
    # Add request to database
    #query <- sqlInterpolate(ANSI(), paste0("INSERT INTO enrichment_list(id, chemlist, type, node_cutoff, anno_select_str, timestamp_start, ip) VALUES('", transactionId, "','", enrichmentSets, "','", mode, "','", nodeCutoff, "','", annoSelectStr, "','", Sys.time(), "','", req$REMOTE_ADDR, "');"), id="addToDb")
    #outp <- dbGetQuery(poolInput, query)
  
    # Create input set txt for enrichment
    for (i in names(enrichmentSets)) {
      outString <- ""
      for (j in enrichmentSets[[i]]) {
        query <- sqlInterpolate(ANSI(), paste0("SELECT testsubstance_chemname FROM chemical_detail WHERE CASRN LIKE '", j, "';"), id = j)
        outp <- dbGetQuery(poolInput, query)
        if (dim(outp)[1] > 0 & dim(outp)[2] > 0) {
          outString <- paste0(outString, j, "\t", outp, "\n\n")
        }
      }
      # Only write if there are matching chemicals (don't create input files for empty sets)
      if(nchar(outString) > 0){
        inFile <- file(paste0(inDir, i, ".txt"))
        writeLines(outString,inFile)
        close(inFile) 
      }
    }
    poolClose(poolInput)
    return(TRUE)
 #})
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

### SECTION 5: DEALING WITH ENRICHMENT RESULTS ###

#* Get results for given directory
#* @param transactionId UUID of the request
#* @setName if supplied, is the set name to fetch from
#* @get /getResults
getResults <- function(res, req, transactionId, setName="###"){
  # "###" is a placeholder, this cannot be created as a valid set name. TODO: Handle this better
  # async
  #future_promise({
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
  #})
}

#* Read gct files for a given request
#* @param transactionId UUID of the request
#* @param cutoff Given node cutoff value for the request
#* @param mode Chart or Cluster
#* @get /readGct
readGct <- function(res, req, transactionId, cutoff, mode, set="Set1"){
  # async
  #future_promise({
    outDir <- paste0(APP_DIR, "Output/", transactionId, "/")
    gctFile <- NULL
    if(mode == "chart"){
      gctFile <- read.table(paste0(outDir, "gct/Chart_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.gct"), skip=2, header=TRUE, sep="\t", row.names=1, comment.char="", fill=FALSE, colClasses=c("Terms"="NULL","integer") )  
    } else if(mode == "cluster"){
      gctFile <- read.table(paste0(outDir, "gct/Cluster_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.gct"), skip=2, header=TRUE, sep="\t", row.names=1, comment.char="", fill=FALSE, colClasses=c("Terms"="NULL","integer") )
    } else { # per set
      gctFile <- read.table(paste0(outDir, "gct_per_set/", set, "__Chart.gct"), skip=2, header=TRUE, sep="\t", row.names=1, comment.char="", fill=FALSE, colClasses=c("Name"="NULL","integer") )
    }
    return(gctFile)
  #})
}

#* Return Chart Simple file for given request and input set
#* @param transactionId UUID of the request
#* @param inputSet Current input set
#* @get /bargraph
bargraph <- function(res, req, transactionId, inputSet){
  # async
  #future_promise({
  baseDirName <- paste0(APP_DIR, "Output/", transactionId, "/")
  
  # Get all Chart Simple files
  allChartFiles <- Sys.glob(paste0(baseDirName, "*__ChartSimple.txt"))
  
  bgChart <- lapply(allChartFiles, function(chartFile){
    bgChartTmp <- read.table(chartFile, header=TRUE, sep="\t", comment.char="", fill=FALSE)  
  })
  
  # Reformat P-Values to character vectors so they don't get truncated when being converted to json upon response
  for(setName in 1:length(bgChart)){
    for(x in 1:nrow(bgChart[[setName]])){
      #bgChart[[setName]][x, "PValue"] <- as.numeric(sprintf("%.9f", bgChart[[setName]][x, "PValue"]))
      bgChart[[setName]][x, "PValue"] <- as.character(bgChart[[setName]][x, "PValue"])
    }
  }
  return(bgChart)
  #})
}

#* Generate interactive visNetwork for given request and input set
#* @param transactionId UUID of the request
#* @param cutoff Node cutoff
#* @param mode Chart or cluster
#* @param input Current input set
#* @get /generateNetwork
generateNetwork <- function(res, req, transactionId, cutoff, mode, input, qval){
  # async
  #future_promise({
  input <- unlist(str_split(input, "#"))
  
  baseDirName <- paste0(APP_DIR, "Output/", transactionId, "/")
  chartForNetFile <- NULL
  # First, read files from /gct/ directory
  if(mode == "chart") {
    chartForNetFile <- read.delim(paste0(baseDirName, "/gct/Chart_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.ForNet"), sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, fill=TRUE)
  } else { # cluster
    chartForNetFile <- read.delim(paste0(baseDirName, "/gct/Cluster_Top", cutoff, "_ALL__P_0.05_P__ValueMatrix.ForNet"), sep="\t", comment.char="", quote="", stringsAsFactors=FALSE, header=TRUE, fill=TRUE)
  }
  
  # Isolate Terms and UID columns
  chartNetworkTerms <- unlist(lapply(1:nrow(chartForNetFile), function(x){
    include <- FALSE
    for(y in input) {
      i <- gsub("\\s+", ".", y)
      if(!is.na(chartForNetFile[x, i])) {
        include <- TRUE
      }
    }
    if(include == TRUE) {
      return(chartForNetFile[x,"Terms"])  
    } else {
      return(NULL)
    }
  }))
  chartNetworkTerms <- chartNetworkTerms[!sapply(chartNetworkTerms, is.null)]
  
  chartNetworkUIDs <- unlist(lapply(1:nrow(chartForNetFile), function(x){
    include <- FALSE
    for(y in input) {
      i <- gsub("\\s+", ".", y)
      if(is.na(chartForNetFile[x, i]) == FALSE) {
        include <- TRUE
      }
    }
    if(include == TRUE) {
      return(chartForNetFile[x,"UID"])  
    } else {
      return(NULL)
    }
  }))
  chartNetworkUIDs <- chartNetworkUIDs[!sapply(chartNetworkUIDs, is.null)]
  
  # Create placeholder string for querying database
  termsStringPlaceholder <- paste0(chartNetworkUIDs, collapse=",")
  
  # Connect to db
  
  poolNetwork <- dbPool(
    drv = dbDriver("PostgreSQL", max.con = 100),
    dbname = tox21config$database,
    host = tox21config$host,
    user = tox21config$uid,
    password = tox21config$pwd,
    idleTimeout = 3600000
  )
  
  # Query database
  queryNetwork <- sqlInterpolate(ANSI(), paste0(
    "SELECT
              p.*, a.annoterm as name1, b.annoterm as name2, ac.annoclassname as class1, bc.annoclassname as class2, ac.baseurl as url1, bc.baseurl as url2
              FROM annoterm_pairwise p
              LEFT JOIN annotation_detail a 
              ON p.term1uid = a.annotermid
              LEFT JOIN annotation_detail b 
              ON p.term2uid = b.annotermid
              LEFT JOIN annotation_class ac 
              ON a.annoclassid = ac.annoclassid
              LEFT JOIN annotation_class bc 
              ON b.annoclassid = bc.annoclassid
              WHERE p.term1uid IN (", termsStringPlaceholder, ") AND p.term2uid IN (", termsStringPlaceholder, ") AND p.qvalue <", qval, ";"
  ), id="addToDb")
  outpNetwork <- dbGetQuery(poolNetwork, queryNetwork)
  
  # Close pool
  poolClose(poolNetwork)
  
  return(outpNetwork)
  #})
}

#######################################################################################

#* Post input set for enrichment analysis
#* @param enrichmentType The mode to use when performing enrichment analysis. CASRNs = CASRN input. Substructure = SMILES or InChI input. Similarity = SMILES or InChI input.
#* @param inputStr Comma-separated string of chemicals for this enrichment process.
#* @param annoSelectStr Comma-separated string of all enabled annotations for this enrichment process.
#* @param tanimoto Tanimoto threshold for Similarity search (default 0.5).
#* @post /input
function(enrichmentType="CASRNs", inputStr="", annoSelectStr="MESH PHARMACTIONLIST ACTIVITY_CLASS ADVERSE_EFFECT INDICATION KNOWN_TOXICITY MECH_LEVEL_1 MECH_LEVEL_2 MECH_LEVEL_3 MECHANISM MODE_CLASS PRODUCT_CLASS STRUCTURE_ACTIVITY TA_LEVEL_1 TA_LEVEL_2 TA_LEVEL_3 THERAPEUTIC_CLASS TISSUE_TOXICITY DRUGBANK_ATC DRUGBANK_ATC_CODE DRUGBANK_CARRIERS DRUGBANK_ENZYMES DRUGBANK_TARGETS DRUGBANK_TRANSPORTERS CTD_CHEM2DISEASE CTD_CHEM2GENE_25 CTD_CHEMICALS_DISEASES CTD_CHEMICALS_GENES CTD_CHEMICALS_GOENRICH_CELLCOMP CTD_CHEMICALS_GOENRICH_MOLFUNCT CTD_CHEMICALS_PATHWAYS CTD_GOSLIM_BIOPROCESS CTD_PATHWAY HTS_ACTIVE LEADSCOPE_TOXICITY MULTICASE_TOX_PREDICTION TOXCAST_ACTIVE TOXINS_TARGETS TOXPRINT_STRUCTURE TOXREFDB", tanimoto=0.5) {
  
  # async
  #future_promise({
    # Generate UUID for enrichment process (return this to user)
    transactionId <- UUIDgenerate()

    # Open pool for PostgreSQL
    poolTerms <- dbPool(
      drv = dbDriver("PostgreSQL",max.con = 100),
      dbname = tox21config$database,
      host = tox21config$host,
      user = tox21config$uid,
      password = tox21config$pwd,
      idleTimeout = 3600000
    )
    
    # Set tanimoto threshold
    queryTanimoto <- sqlInterpolate(ANSI(), paste0("set rdkit.tanimoto_threshold=", tanimoto, ";"))
    outpTanimoto <- dbGetQuery(poolTerms, queryTanimoto)
  
    inputList <- list()
    currentSet <- ""
    
    inputListTry <- tryCatch({
      # should be of format [:digit:]+-[:digit:]+-[:digit:]+
      inputListTmp <- str_split(inputStr, " ")
      inputListTmp <- unlist(inputListTmp)
      
      # Grab CASRNs if type is Substructure
      if(enrichmentType == "Substructure"){
        setNameIndex <- 1
        substructureMainProcess <- lapply(inputListTmp, function(i){
          goodToAdd <- TRUE #Set this to false and don't add if InChI is bad
          
          # First, check if we have any InChIs, and convert to SMILES
          if (grepl("InChI=",i,fixed=TRUE)) {
            queryInchi <- sqlInterpolate(ANSI(), paste0("SELECT smiles FROM chemical_detail WHERE inchis = '", i, "';"),
                                         id = "smilesResults")
            outpInchi <- dbGetQuery(poolTerms, queryInchi)
            if(length(outpInchi) > 0){
              i <- outpInchi[[1]]
            } else {
              goodToAdd <- FALSE
            }
          }
          if (goodToAdd == TRUE) {
            queryGetCasFromSubstructure <- sqlInterpolate(ANSI(), paste0("SELECT casrn FROM mols_2 WHERE m @> CAST('", i, "' AS mol);"))
            outpGetCasFromSubstructure <- dbGetQuery(poolTerms, queryGetCasFromSubstructure)
            
            if(length(outpGetCasFromSubstructure) > 0){ 
              currentSet <<- paste0("Set",setNameIndex)
              substructureProcess <- lapply(1:nrow(outpGetCasFromSubstructure), function(index){
                casrn <- paste0(outpGetCasFromSubstructure[index, "casrn"])
                return(casrn)
              })
              inputList[[currentSet]] <<- unlist(substructureProcess)
              setNameIndex <<- setNameIndex + 1
            }
          }
        })
      }
      
      # Grab CASRNs if type is Similarity
      else if(enrichmentType == "Similarity"){
        setNameIndex <- 1
        similarityMainProcess <- lapply(inputListTmp, function(i){
          goodToAdd <- TRUE #Set this to false and don't add if InChI is bad
          
          # First, check if we have any InChIs, and convert to SMILES
          if (grepl("InChI=",i,fixed=TRUE)) {
            queryInchi <- sqlInterpolate(ANSI(), paste0("SELECT smiles FROM chemical_detail WHERE inchis = '", i, "';"),
                                         id = "smilesResults")
            outpInchi <- dbGetQuery(poolTerms, queryInchi)
            if(length(outpInchi) > 0){
              i <- outpInchi[[1]]
            } else {
              goodToAdd <- FALSE
            }
          }
          
          if (goodToAdd == TRUE) {
            queryGetCasFromSimilarity <- sqlInterpolate(ANSI(), paste0("SELECT * FROM get_mfp2_neighbors('", i, "');"))
            outpGetCasFromSimilarity <- dbGetQuery(poolTerms, queryGetCasFromSimilarity)
            if (length(outpGetCasFromSimilarity) > 0) {
              currentSet <<- paste0("Set",setNameIndex)
              similarityProcess <- lapply(1:nrow(outpGetCasFromSimilarity), function(index){
                casrn <- paste0(outpGetCasFromSimilarity[index, "casrn"])
                return(casrn)
              })
              inputList[[currentSet]] <<- unlist(similarityProcess)
              setNameIndex <<- setNameIndex + 1
            }
          }
        })
        
      }
  
      # Else do CASRNs
      else {
        currentSet <- "Set1"
        preparedInputList <- lapply(inputListTmp, function(i){
          if(startsWith(i, "#") == TRUE) { # Set Name
            currentSet <<- str_replace(i, "#", "") #remove pound symbol from set name
            inputList[[currentSet]] <<- list()
          } else {
            inputList[[currentSet]] <<- append(inputList[[currentSet]], i)
          }
        })
      
      }
      
    }, error=function(e) {
      message(paste0("Error at ", e))
      # Close DB connection
      poolClose(poolTerms)
      return(NULL)
    }, finally={
      # Validate list - remove anything that's not a valid CASRN
      inputListRemove <- lapply(1:length(inputList), function(i) {
        inputList[[i]] <<- inputList[[i]][grepl("\\d+-\\d+-\\d+", inputList[[i]])]
      })
  
    })
    
    fileGenerationProcess <- lapply(names(inputList), function(i){
      # Create input directory & input file
      inputDir <- paste0(APP_DIR, "Input/", transactionId, "/")
      inputFile	<- paste0(APP_DIR, "Input/", transactionId, "/", i, ".txt")
      dir.create(inputDir)
      INPUT <- file(inputFile)  
      
      writeInput <- ""
      # For each set in input, fetch the terms for its CASRNs
      getTermNames <- lapply(inputList[[i]], function(j){
        queryTermName <- sqlInterpolate(ANSI(), paste0("SELECT TestSubstance_ChemName FROM chemical_detail WHERE CASRN LIKE '", j, "';"))
        outpTermName <- dbGetQuery(poolTerms, queryTermName)
        fetchedTerm <- outpTermName[1, "testsubstance_chemname"]
        # Validate list - remove anything that's not in Tox21
        if (length(fetchedTerm) == 1) {
          writeInput <<- paste0(writeInput, j, "\t", fetchedTerm, "\n\n")  
        }
        
      })
      
      # Write to input file
      write(writeInput, INPUT, append=TRUE)
      # Close file connection
      close(INPUT)
    })
    # Close DB connection
    poolClose(poolTerms)
    
    # Send to enrich endpoint to perform enrichment analysis
    performEnrichment(transactionId, annoSelectStr)
  
    outDir <- paste0(APP_DIR, "Output/", transactionId,"/", sep="")
    for (i in names(inputList)) {
      xlsxChart <- read.table(paste0(outDir, i, "__Chart.txt"), header=TRUE, sep="\t", comment.char="", fill=FALSE)
      xlsxChartSimple <- read.table(paste0(outDir, i, "__ChartSimple.txt"), header=TRUE, sep="\t", comment.char="", fill=FALSE)
      xlsxCluster <- read.table(paste0(outDir, i, "__Cluster.txt"), header=FALSE, sep="\t", comment.char="", 
                                col.names=seq_len(13), fill=TRUE ) #this needs to be treated differently due to having different # of columns
      write.xlsx2(xlsxChart, paste0(outDir, i, "__Chart.xlsx"), sheetName=paste0(i), col.names=TRUE, row.names=TRUE, append=FALSE)
      write.xlsx2(xlsxChartSimple, paste0(outDir, i, "__ChartSimple.xlsx"), sheetName=paste0(i), col.names=TRUE, row.names=TRUE, append=FALSE)
      write.xlsx2(xlsxCluster, paste0(outDir, i, "__Cluster.xlsx"), sheetName=paste0(i), col.names=TRUE, row.names=TRUE, append=FALSE)
    }
    
    print('done')
    return(transactionId)
  #})
}

########################################################################################################################################################################################################################################################

### ! headless (no-gui-client) endpoints ! ###

#* Download enrichment results for a given uuid
#* @serializer contentType list(type="application/zip")
#* @param id The UUID of the enrichment process to download.
#* @get /download
function(id="-1", res) {
  # async
  future_promise({
    fName <- paste0(APP_DIR, "Output/", id, "/tox21enricher_", id, ".zip", sep="")
    readBin(fName, 'raw', n = file.info(fName)$size)
  })
}
#######################################################################################


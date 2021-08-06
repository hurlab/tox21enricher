# performEnrichment.R
###############################################################
# API for Tox21 Enricher                                      #
# developed by Junguk Hur and                                 #               
# Parker Combs (parker.combs@und.edu)                         #
# xx/xx/xxxx - ver. 1.0                                       #
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
tox21db <- config::get("tox21enricher")
pool <- dbPool(
  drv = dbDriver("PostgreSQL",max.con = 100),
  dbname = tox21db$database,
  host = tox21db$host,
  user = tox21db$uid,
  password = tox21db$pwd,
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

#TODO: speed this up vvv
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
  #return(c(tmp_DSSTox2name, tmp_DSSTox2CASRN, tmp_CASRN2DSSTox))
})

print("! Finished loading base annotations.")

#TODO: Map
# TODO: move this on API load
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
    print("! Current queue: ")
    print(queueDirContents)
    print("!!!!!!!!!!!!!!!!!")
    # Regenerate 0__queue__default file if missing
    if(("0__queue__default" %in% queueDirContents) == FALSE) {
      file.create(paste0(queueDir, "0__queue__default"))
      queueDirContents <- Sys.glob(paste0(APP_DIR, "Queue/*__queue__*"))
    }
    
    
    queueDirNumbers <- lapply(queueDirContents, function(queueFile){
      #strip path
      noPathFile <- unlist(str_split(queueFile, paste0(APP_DIR, "Queue/")))[2]
      print("noPathFile")
      print(noPathFile)
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
  
  print("queueDirIDs")
  print(queueDirIDs)
  
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

#* Ping API
#* @get /ping
ping <- function(res, req){
  return(TRUE)
}

#* Get timestamps for given enrichment
#* @param transactionId
#* @get /getTimestamp
getTimestamp <- function(res, req, transactionId="-1"){
  # Connect to db
  tox21db <- config::get("tox21enricher")
  poolUpdate <- dbPool(
    drv = dbDriver("PostgreSQL", max.con = 100),
    dbname = tox21db$database,
    host = tox21db$host,
    user = tox21db$uid,
    password = tox21db$pwd,
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
    print("annoSelect")
    print(annoSelect)
    
    # Get input sets
    inDir <- paste0(APP_DIR, "Input/", enrichmentUUID)    # Directory for input files for enrichment set
    outDir <- paste0(APP_DIR, "Output/", enrichmentUUID)  # Directory for output files for enrichment set
    
    inputSets <- Sys.glob(paste0(inDir,"/*"))
    
    print("inputSets")
    print(inputSets)
    
    # Connect to db
    tox21db <- config::get("tox21enricher")
    poolMatrix <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21db$database,
      host = tox21db$host,
      user = tox21db$uid,
      password = tox21db$pwd,
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
      
      print("input")
      print(input)
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
        print(paste0("creating output file at: ", paste0(outDir, "/", setName, "__", CASRN, ".txt")))
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
        
        print("fetchedTerms")
        print(fetchedTerms)
        
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
      
      print("individualMatrix")
      print(individualMatrix)
      
      combinedMatrix <- unlist(unname(individualMatrix), recursive = FALSE)
                               
      # Create matrix file for all CASRNs in set
      print(paste0("creating output file at: ", paste0(outDir, "/", setName, "__FullMatrix.txt")))
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

#* Get annotation classes/types (internal use only)
#* @get /initAnnotations
initAnnotations <- function(res, req){
  # async
  #future_promise({
    
    # Connect to db
    tox21db <- config::get("tox21enricher")
    poolAnnotations <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21db$database,
      host = tox21db$host,
      user = tox21db$uid,
      password = tox21db$pwd,
      idleTimeout = 3600000
    )
    annoClassQuery <- sqlInterpolate(ANSI(), "SELECT annoclassname, annotype, annodesc FROM annotation_class;", id = "getAnnotationClasses")
    annoClassOutp <- dbGetQuery(poolAnnotations, annoClassQuery)
    
    # Phase out old CTD annotations (TODO: remove these altogether)
    annoClassOutpRemove <- c("CTD_CHEM2DISEASE", "CTD_CHEM2GENE_25", "CTD_PATHWAY")
    annoClassOutp2 <- annoClassOutp[!(annoClassOutp$annoclassname %in% annoClassOutpRemove), ]
    rownames(annoClassOutp2) <- 1:nrow(annoClassOutp2)
    
    print("annoClassOutp2")
    print(annoClassOutp2)
    
    poolClose(poolAnnotations)
    #return(annoClassOutp)
    return(annoClassOutp2)
  #})
}

#* Get total number of enrichments (internal use only)
#* @get /total
total <- function(res, req){
  # async
  #future_promise({
    
    # Connect to db
    tox21db <- config::get("tox21enricher")
    poolTotal <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21db$database,
      host = tox21db$host,
      user = tox21db$uid,
      password = tox21db$pwd,
      idleTimeout = 3600000
    )
    totalQuery <- sqlInterpolate(ANSI(), "SELECT COUNT(*) FROM enrichment_list;", id = "getTotalEnrichment")
    totalOutp <- dbGetQuery(poolTotal, totalQuery)
    return(totalOutp)
  #})
}

#* Get data for provided SMILES string (substructure) (internal use only)
#* @param input Input string, either SMILES or CASRN (if re-enriching)
#* @param reenrich Boolean value to let the API know if this is a re-enrichment or not
#* @get /substructure
substructure <- function(res, req, input, reenrich=FALSE){
  # async
  #future_promise({
    # Connect to db
    tox21db <- config::get("tox21enricher")
    poolSubstructure <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21db$database,
      host = tox21db$host,
      user = tox21db$uid,
      password = tox21db$pwd,
      idleTimeout = 3600000
    )
    
    # Sanitize input, convert InChI strings to SMILES
    # TODO: make it so the client doesn't have to request this, just do it all here
    if (grepl("InChI=", input, fixed=TRUE)) {
      input <- convertInchi(inchi=input)
      print("converted input")
      print(input)
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
    tox21db <- config::get("tox21enricher")
    poolSimilarity <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21db$database,
      host = tox21db$host,
      user = tox21db$uid,
      password = tox21db$pwd,
      idleTimeout = 3600000
    )
    
    # Sanitize input, convert InChI strings to SMILES
    # TODO: make it so the client doesn't have to request this, just do it all here
    if (grepl("InChI=", input, fixed=TRUE)) {
      input <- convertInchi(inchi=input)
      print("converted input")
      print(input)
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
    tox21db <- config::get("tox21enricher")
    poolCasrn <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21db$database,
      host = tox21db$host,
      user = tox21db$uid,
      password = tox21db$pwd,
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
    tox21db <- config::get("tox21enricher")
    poolInchi <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21db$database,
      host = tox21db$host,
      user = tox21db$uid,
      password = tox21db$pwd,
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
  tox21db <- config::get("tox21enricher")
  poolInchi <- dbPool(
    drv = dbDriver("PostgreSQL", max.con = 100),
    dbname = tox21db$database,
    host = tox21db$host,
    user = tox21db$uid,
    password = tox21db$pwd,
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
    tox21db <- config::get("tox21enricher")
    poolSvg <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21db$database,
      host = tox21db$host,
      user = tox21db$uid,
      password = tox21db$pwd,
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
    
    print("structures")
    print(structures)
    
    poolClose(poolSvg)
    return(structures)
  #})
}

## The following are various methods that work with the filesystem ##

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
#* Check Tox21 Enricher version
#* @get /getAppVersion
getAppVersion <- function(res, req){
  #future_promise({
    return(APP_VERSION)
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
    
    print("RECEIVED:")
    print(transactionId)
    print(enrichmentSets)
    print(setNames)
    
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
    
    print("!! enrichmentSets !!")
    print(enrichmentSets)
  
    
    # Create input directory
    inDir <- paste0(APP_DIR, "Input/", transactionId, "/")
    dir.create(inDir)
    # Create output directory
    outDir <- paste0(APP_DIR, "Output/", transactionId, "/")
    dir.create(outDir)
    
    # Connect to db
    tox21db <- config::get("tox21enricher")
    poolInput <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21db$database,
      host = tox21db$host,
      user = tox21db$uid,
      password = tox21db$pwd,
      idleTimeout = 3600000
    )
    
    # Add request to database
    #query <- sqlInterpolate(ANSI(), paste0("INSERT INTO enrichment_list(id, chemlist, type, node_cutoff, anno_select_str, timestamp_start, ip) VALUES('", transactionId, "','", enrichmentSets, "','", mode, "','", nodeCutoff, "','", annoSelectStr, "','", Sys.time(), "','", req$REMOTE_ADDR, "');"), id="addToDb")
    #outp <- dbGetQuery(poolInput, query)
  
    # Create input set txt for enrichment
    for (i in names(enrichmentSets)) {
      inFile <- file(paste0(inDir, i, ".txt"))
      outString <- ""
      for (j in enrichmentSets[[i]]) {
        query <- sqlInterpolate(ANSI(), paste0("SELECT testsubstance_chemname FROM chemical_detail WHERE CASRN LIKE '", j, "';"), id = j)
        outp <- dbGetQuery(poolInput, query)
        if (dim(outp)[1] > 0 & dim(outp)[2] > 0) {
          outString <- paste0(outString, j, "\t", outp, "\n\n")
        }
      }
      writeLines(outString,inFile)
      close(inFile)
    }
    poolClose(poolInput)
    return(TRUE)
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
    #if(missing(setName)) {
    print("===> SET NAME <===")
    print(setName)
    #if(nchar(setName) < 0) {
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
    print(">>>>> outDir")
    print(outDir)
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
    
    print("CANCELING:: ")
    print(requestToCancel)
    print(requestToCancelError)
    
    # If queue file exists, delete it
    if(length(requestToCancel) > 0){
      msg <- unlink(requestToCancel[1])
      print(paste0(">>>>>>>>>>>>", msg))
    } else {
      # Check if error file exists if queue file is gone and delete that too
      if(length(requestToCancelError) > 0){
        unlink(requestToCancelError[1])
      }
    }
    
    # Connect to db
    tox21db <- config::get("tox21enricher")
    poolCancel <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21db$database,
      host = tox21db$host,
      user = tox21db$uid,
      password = tox21db$pwd,
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
    
    print("chartForNetFile")
    print(chartForNetFile)
    
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
    tox21db <- config::get("tox21enricher")
    poolNetwork <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21db$database,
      host = tox21db$host,
      user = tox21db$uid,
      password = tox21db$pwd,
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


#* Perform enrichment analysis (for internal use by Tox21 Enricher only)
#* @param enrichmentUUID UUID for Input/Output directory on local machine, generated by Tox21 Enricher application.
#* @param annoSelectStr String, comma-delimited, containing all enabled annotations for this enrichment process. Passed from Tox21 Enricher application.
#* @param nodeCutoff numerical value between 1-100 for the max number to use in clustering.
#* @get /enrich
performEnrichment <- function(res, req, enrichmentUUID="-1", annoSelectStr="MESH=checked,PHARMACTIONLIST=checked,ACTIVITY_CLASS=checked,ADVERSE_EFFECT=checked,INDICATION=checked,KNOWN_TOXICITY=checked,MECH_LEVEL_1=checked,MECH_LEVEL_2=checked,MECH_LEVEL_3=checked,MECHANISM=checked,MODE_CLASS=checked,PRODUCT_CLASS=checked,STRUCTURE_ACTIVITY=checked,TA_LEVEL_1=checked,TA_LEVEL_2=checked,TA_LEVEL_3=checked,THERAPEUTIC_CLASS=checked,TISSUE_TOXICITY=checked,DRUGBANK_ATC=checked,DRUGBANK_ATC_CODE=checked,DRUGBANK_CARRIERS=checked,DRUGBANK_ENZYMES=checked,DRUGBANK_TARGETS=checked,DRUGBANK_TRANSPORTERS=checked,CTD_CHEM2DISEASE=checked,CTD_CHEM2GENE_25=checked,CTD_CHEMICALS_DISEASES=checked,CTD_CHEMICALS_GENES=checked,CTD_CHEMICALS_GOENRICH_CELLCOMP=checked,CTD_CHEMICALS_GOENRICH_MOLFUNCT=checked,CTD_CHEMICALS_PATHWAYS=checked,CTD_GOSLIM_BIOPROCESS=checked,CTD_PATHWAY=checked,HTS_ACTIVE=checked,LEADSCOPE_TOXICITY=checked,MULTICASE_TOX_PREDICTION=checked,TOXCAST_ACTIVE=checked,TOXINS_TARGETS=checked,TOXPRINT_STRUCTURE=checked,TOXREFDB=checked,", nodeCutoff=10) {
  
  # async
  #future_promise({
  
    ###### Variables ######
    
    print(paste0("Received ID: ", enrichmentUUID))
    print(paste0("Received Annotations: ", annoSelectStr))
    print(paste0("Received Cutoff: ", nodeCutoff))
    
    # Enrichment parameters
    annoSelectStrSplit <- strsplit(annoSelectStr,"=checked,",fixed=TRUE)
    annoSelectStrPrepared <- paste0(paste(unlist(annoSelectStrSplit), collapse="=checked "), "=checked")  # annotation selection string
    funCat2Selected <- list()
    annoIndex <- 1
    for(i in annoSelectStrSplit[[1]]) {
      funCat2Selected[[i]] <- 1
    }
    inDir <- paste0(APP_DIR, "Input/", enrichmentUUID)    # Directory for input files for enrichment set
    outDir <- paste0(APP_DIR, "Output/", enrichmentUUID)  # Directory for output files for enrichment set
    
    # CASRN count
    funCatTerm2CASRNCount <- list() 
    funCat2CASRNCount     <- list()
    funCat2termCount      <- list()
    
    # DSSTox Chart
    pvalueThresholdToDisplay	  <- 0.2		# p-value < 0.1 to be printed
    
    # DSSTox Clustering
    similarityTermOverlap		    <- 3
    similarityThreshold		      <- 0.50
    initialGroupMembership		  <- 3
    finalGroupMembership		    <- 3
    multipleLinkageThreshold	  <- 0.50
    EASEThreshold				        <- 1.0
    
  ###### Process ######
    
    
    # Calculate total CASRN count 
    tmp_funCat2CASRNCount <- list()
    tmp_funCat2termCount <- list()
    tmp_funCatTerm2CASRNCount <- list()
    tmp_innerFunCatTerm2CASRNCount <- list()
  
    funCat2CASRNCount <- lapply(names(funCat2Selected), function(funCat){
      tmpArray <- names(funCat2CASRN[[funCat]])
      tmp_funCat2CASRNCount[[funCat]] <- length(tmpArray)
      return(tmp_funCat2CASRNCount)
    })
    funCat2CASRNCount <- unlist(funCat2CASRNCount)
    
    funCat2termCount <- lapply(names(funCat2Selected), function(funCat){
      terms <- names(funCatTerm2CASRN[[funCat]])
      tmp_funCat2termCount[[funCat]] <- length(terms)
      return(tmp_funCat2termCount)
    })
    funCat2termCount <- unlist(funCat2termCount)
    
    funCatTerm2CASRNCount_lv1 <- lapply(names(funCat2Selected), function(funCat){
      tmpList <- vector("list", 1)
      tmpList[1] <- funCat
    })
    names(funCatTerm2CASRNCount_lv1) <- names(funCat2Selected)
    funCatTerm2CASRNCount <- lapply(funCatTerm2CASRNCount_lv1, function(funCat){
      tmp_funCatNames <- names(funCatTerm2CASRN[[funCat]])
  
      tmp_funCatTerm2CASRNCount <- lapply(tmp_funCatNames, function(term){
        return(length(names(funCatTerm2CASRN[[funCat]][[term]])))
      })
      names(tmp_funCatTerm2CASRNCount) <- names(funCatTerm2CASRN[[funCat]])
      return(tmp_funCatTerm2CASRNCount)
    })
    
    # Load input DSSTox ID or CASRN ID sets
    inputFiles <- list.files(path=paste0(APP_DIR, "Input/", enrichmentUUID), pattern="*.txt", full.names=TRUE)
    # Throw error if no input sets
    if(length(inputFiles) < 1){
      res$status = 500
      res$body = paste0("No valid input sets. Cannot perform enrichment analysis.")
      return(res) #return error message
    }
    
    ldf <- lapply(inputFiles, function(i){
        openInput <- tryCatch({
          read.delim(file=i, header=FALSE, sep="\t", comment.char="", fill=TRUE)
        }, error=function(e){
          return(NULL)
        })
    })
  
    # Check which input sets are good
    ldf <- lapply(1:length(ldf), function(x){
      return(ldf[[x]])
    })
    ldf <- ldf[!sapply(ldf, is.null)]
  
    # If there are no good input sets, crash gracefully. If we have at least one, 
    if(length(ldf) < 1){
      res$status = 500
      res$body = paste0("No lines available in any input file. Cannot perform enrichment analysis.")
      return(res) #return error message
    }
    
    tmp_inputIDListHash <- list()
    inputIDListHash <- lapply(ldf, function(i){
        res <- apply(i, 1, function(j){
          tmp_inputIDListHash[[j[1]]] <- 1
          return(tmp_inputIDListHash)
        })
    })
    print(paste0("Processing ", length(inputIDListHash), " set(s)."))
    
    # Perform EASE calculation
    setNameCounter <- 1
    
    outfileBaseNames <- lapply(1:length(ldf), function(setNameCtr){
      setNameItem <- str_remove(inputFiles[[setNameCtr]], paste0(APP_DIR, "Input/", enrichmentUUID, "/")) # Remove path
      setNameItem <- str_remove(setNameItem, ".txt") # Remove filename extension
    })
    names(inputIDListHash) <- outfileBaseNames
  
    # multi-core
    #enrichmentStatusComplete <- mclapply(outfileBaseNames, mc.cores=4, mc.silent=FALSE, function(i){
    enrichmentStatusComplete <- lapply(outfileBaseNames, function(i){
  
        # Get list of CASRN names
        CASRNS <- lapply(inputIDListHash[[i]], function(j){
            return(names(j))
        })
  
        # Extract file name from path
        outfileBase <- i
        
        # Check mapped CASRNS
        mappedCASRNs <- vector("list", length(CASRNS))
        mappedCASRNHash <- list()
        names(mappedCASRNs) <- CASRNS
        mappedCASRNs <- lapply(CASRNS, function(CASRN){
          if(is.null(CASRN2DSSTox[[CASRN]]) == FALSE){
            mappedCASRNHash[[CASRN]] <- 1
          }
          return(mappedCASRNHash)
        })
        mappedCASRNs <- unlist(mappedCASRNs)
  
        # Perform enrichment analysis
        print(paste0("Performing enrichment on ", outfileBase, "..."))
        enrichmentStatus <- perform_CASRN_enrichment_analysis(CASRNS, paste0(APP_DIR, "Output/", enrichmentUUID, "/"), outfileBase, mappedCASRNs, funCat2Selected, CASRN2funCatTerm, funCatTerm2CASRN, funCat2CASRNCount, funCat2termCount, funCatTerm2CASRNCount, pvalueThresholdToDisplay, similarityThreshold, initialGroupMembership, multipleLinkageThreshold, EASEThreshold)
    })
    
    ########################################################################################
    # Create individual GCT file
    #
    #
    ########################################################################################
    
    
    #if (not defined $ARGV[6])
    #{	die "\n!ERROR: Missing parameters\n".
    #  ">ThisScript.pl <Input Dir> <DAVID Result directory> <topTermCount per file>\n".
    #  "               <Sig-applying ALL or AT-LEAST-ONE data> <SigColumn> <SigCutOff> <ValueColumn>\n".
    #  "				[optional part of file name]\n\n".
    #  ">ThisScript.pl Input/ Output/ 10 ALL BH 0.05 P\n\n";
    #}
    
    ARGV_0 <- inDir
    ARGV_1 <- outDir 
    ARGV_2 <- nodeCutoff
    ARGV_3 <- "ALL"
    ARGV_4 <- "P"
    ARGV_5 <- 0.05
    ARGV_6 <- "P"
    
    
    # -------------------------------------
    # Get the corresponding directory names
    # -------------------------------------
    baseinputDirName <- inDir
    baseDirName			 <- outDir
    baseNameSplit		 <- str_split(baseDirName, "/")
    baseShortDirName <- ''
    baseOutputDir	<- paste0(baseDirName,"/gct_per_set/",sep="")
    baseOutputDirGct <- paste0(baseDirName,"/gct/",sep="")
    dir.create(baseOutputDir)
    print(paste0("created directory at: ", baseOutputDir, sep=""))
    
    # -------------------------------------
    # Load CASRN name
    # -------------------------------------
    CAS <- outpChemDetail
    CASRN2Name <- vector("list", length=nrow(CAS))
    CASRN2Name <- lapply(1:nrow(CAS), function(x){
      CAS[x,"testsubstance_chemname"]
    })
    names(CASRN2Name) <- lapply(1:nrow(CAS), function(x){
      CAS[x,"casrn"]
    })
    
    # Generate individual gct files
    process_variable_DAVID_CHART_directories_individual_file (baseinputDirName, baseDirName, baseOutputDir, '', '', ARGV_4, ARGV_5, ARGV_6, CASRN2Name)
    # Generate clustering images (heatmaps)
    create_clustering_images (baseOutputDir, "-color=BR")
    # Create DAVID Chart/Cluster files
    create_david_chart_cluster (baseDirName, nodeCutoff, "ALL", "P", 0.05, "P")
    # Generate heatmaps for multiple sets
    create_clustering_images (baseOutputDirGct, "-color=BR")
    # Create xlsx spreadsheets
    setNames <- lapply(Sys.glob(paste0(baseDirName, "/*.*"), dirmark=FALSE), function(resultFile){
      print(paste0("RF ==> ", resultFile))
      tmpSplit <- unlist(str_split(resultFile, paste0(baseDirName, "/"))) # remove path from file
      print("tmpSplit")
      print(tmpSplit)
      tmpSplitName <- unlist(str_split(tmpSplit[2], "__"))[1] # Get set name
      print("tmpSplitName")
      print(tmpSplitName)
      return(tmpSplitName)
    })
    setNames <- unique(unlist(setNames)) # Get only 1 copy of each set name
    
    print("setNames")
    print(setNames)
    
    # Create Excel spreadsheets of existing files
    for (i in setNames) {
      print(paste0(">>> ", paste0(baseDirName, "/", i, "__Chart.txt")))
      
      xlsxChart <- read.table(paste0(baseDirName, "/", i, "__Chart.txt"), header=TRUE, sep="\t", comment.char="", fill=FALSE)
      xlsxChartSimple <- read.table(paste0(baseDirName, "/", i, "__ChartSimple.txt"), header=TRUE, sep="\t", comment.char="", fill=FALSE)
      xlsxCluster <- read.table(paste0(baseDirName, "/", i, "__Cluster.txt"), header=FALSE, sep="\t", comment.char="", 
                                col.names=seq_len(13), fill=TRUE ) #this needs to be treated differently due to having different # of columns
      write.xlsx2(xlsxChart, paste0(baseDirName, "/", i, "__Chart.xlsx"), sheetName=paste0(i), col.names=TRUE, row.names=TRUE, append=FALSE)
      write.xlsx2(xlsxChartSimple, paste0(baseDirName, "/", i, "__ChartSimple.xlsx"), sheetName=paste0(i), col.names=TRUE, row.names=TRUE, append=FALSE)
      write.xlsx2(xlsxCluster, paste0(baseDirName, "/", i, "__Cluster.xlsx"), sheetName=paste0(i), col.names=TRUE, row.names=TRUE, append=FALSE)
    }
    
    # zip result files
    zipDir <- dir(baseDirName, recursive=TRUE, include.dirs=TRUE)
    filesToZip <- unlist(lapply(zipDir, function(x) paste0(baseDirName, "/", x)))
    system(paste0("cd ", baseDirName, "/ ; zip -r9X ./tox21enricher_", enrichmentUUID, ".zip ./*"))
    
    # Connect to db
    tox21db <- config::get("tox21enricher")
    poolUpdate <- dbPool(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = tox21db$database,
      host = tox21db$host,
      user = tox21db$uid,
      password = tox21db$pwd,
      idleTimeout = 3600000
    )
    
    # update database with ending timestamp for enrichment
    query <- sqlInterpolate(ANSI(), paste0("UPDATE enrichment_list SET timestamp_finish='", Sys.time(), "' WHERE id='", enrichmentUUID, "';"), id="addToDb")
    outp <- dbGetQuery(poolUpdate, query)
    
    # return UUID if successful enrichment
    return(enrichmentUUID)
    
  #})
}

#######################################################################################

################################## SUBROUTINES ########################################

###### Generate individual GCT files ######
process_variable_DAVID_CHART_directories_individual_file <- function(inputDirName, dirName, outputDir, extTag, additionalFileName, ARGV_4, ARGV_5, ARGV_6, CASRN2Name) {
  
  # Load the input file *******************************************************************Not sure if this GLOB works
  print("! Loading $inputDirName input files ...\n")
  setInputFiles <- Sys.glob(paste0(inputDirName,"/","*.txt", sep=""), dirmark=FALSE)
  
  # TODO -----------------------------------------------------
  # In case Scott want to include all CASRN in the input list, 
  # We need to reac the original input files
  
  # Check the directory for any file
  print ("! Processing $dirName CHART INDIVIDUAL ...\n")
  infilesAll <- Sys.glob(paste(dirName,"/","*_Chart.txt", sep=""), dirmark=FALSE)
  if (length(infilesAll) < 1) return() # Return if no files
  
  # Step0. Get only the files that actually have contents
  infiles <- lapply(infilesAll, function(infile) {
    #DATA <- read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors = FALSE, header=TRUE, fill=TRUE)
    #if(nrow(DATA) < 1){
    #  return(NULL)
    #} 
    return(infile)
  })
  infiles <- infiles[!sapply(infiles, is.null)]
  
  sigCutOff         <- ARGV_5
  sigColumnName     <- toupper(ARGV_4)
  sigColumnIndex	  <- get_column_index(sigColumnName)			# 5=p-value, 12=BH p-value, 13=FDR
  valueColumnName		<- toupper(ARGV_6)
  valueColumnIndex	<- get_column_index(valueColumnName)    # 5=p-value, 12=BH p-value, 13=FDR
  
  # Step1. Get the list of significant terms
  for (infile in infiles){
    print("checking file")
    print(infile)
  	tmp1 <- str_split(infile, "/")
  	tmp1[[1]][length(tmp1[[1]])] <- str_replace(tmp1[[1]][length(tmp1[[1]])], ".txt", "")
    tmp2 <- tmp1[[1]]
    
    DATA <- read.delim(file=infile, header=TRUE, sep="\t", comment.char="", fill=TRUE) 
    print("=== DATA ===")
    print(DATA)
    
    if(nrow(DATA) < 1){
      # if no rows, make blank .gct file
      print(paste0("GCT file at: ,", outputDir, tmp2[length(tmp2)], sep="\t"))
      OUTFILE           <- file(paste0(outputDir, tmp2[length(tmp2)], ".gct", sep=""))
      outputContent     <- paste0("#1.2\n", 0, "\t", 0, "\nCASRN\tName", sep="")
      writeLines(outputContent, OUTFILE) 
      close(OUTFILE)
    } else {
  
      term2pvalue <- lapply(1:nrow(DATA), function(line){
        tmpSplit <- vector("list", 13)
        tmpSplit[1]   <- as.character(DATA[line, "Category"])
        tmpSplit[2]   <- as.character(DATA[line, "Term"])
        tmpSplit[3]   <- as.character(DATA[line, "Count"])
        tmpSplit[4]   <- as.character(DATA[line, "X."])
        tmpSplit[5]   <- as.character(DATA[line, "PValue"])
        tmpSplit[6]   <- as.character(DATA[line, "CASRNs"])
        tmpSplit[7]   <- as.character(DATA[line, "List.Total"])
        tmpSplit[8]   <- as.character(DATA[line, "Pop.Hits"])
        tmpSplit[9]   <- as.character(DATA[line, "Pop.Total"])
        tmpSplit[10]  <- as.character(DATA[line, "Fold.Enrichment"])
        tmpSplit[11]  <- as.character(DATA[line, "Bonferroni"])
        tmpSplit[12]  <- as.character(DATA[line, "Benjamini"])
        tmpSplit[13]  <- as.character(DATA[line, "FDR"])
  
        if ( grepl("^\\D", tmpSplit[[sigColumnIndex]]) | as.double(tmpSplit[[sigColumnIndex]]) >= sigCutOff | as.double(tmpSplit[[10]]) < 1) {
          # skip
        } else {
          return(tmpSplit[[sigColumnIndex]])
        }
      })
      # Remove null elements
      term2pvalue <- lapply(term2pvalue, function(innerList) innerList[sapply(innerList, length) > 0])
      term2pvalue <- term2pvalue[!sapply(term2pvalue, is.null)]
      
      nameListTerm2pvalue <- lapply(1:nrow(DATA), function(line){
        tmpSplit <- vector("list", 13)
        tmpSplit[1]   <- as.character(DATA[line, "Category"])
        tmpSplit[2]   <- as.character(DATA[line, "Term"])
        tmpSplit[3]   <- as.character(DATA[line, "Count"])
        tmpSplit[4]   <- as.character(DATA[line, "X."])
        tmpSplit[5]   <- as.character(DATA[line, "PValue"])
        tmpSplit[6]   <- as.character(DATA[line, "CASRNs"])
        tmpSplit[7]   <- as.character(DATA[line, "List.Total"])
        tmpSplit[8]   <- as.character(DATA[line, "Pop.Hits"])
        tmpSplit[9]   <- as.character(DATA[line, "Pop.Total"])
        tmpSplit[10]  <- as.character(DATA[line, "Fold.Enrichment"])
        tmpSplit[11]  <- as.character(DATA[line, "Bonferroni"])
        tmpSplit[12]  <- as.character(DATA[line, "Benjamini"])
        tmpSplit[13]  <- as.character(DATA[line, "FDR"])
        
        if ( grepl("^\\D", tmpSplit[[sigColumnIndex]]) | as.double(tmpSplit[[sigColumnIndex]]) >= sigCutOff | as.double(tmpSplit[[10]]) < 1) {
          # skip
        } else {
          tmpTermKey <- paste0(tmpSplit[[2]], " | ", tmpSplit[[1]], sep="")
          return(tmpTermKey)
        }
      })
      # Remove null elements
      nameListTerm2pvalue <- lapply(nameListTerm2pvalue, function(innerList) innerList[sapply(innerList, length) > 0])
      nameListTerm2pvalue <- nameListTerm2pvalue[!sapply(nameListTerm2pvalue, is.null)]
      
      # Set names of term2pvalue
      names(term2pvalue) <- nameListTerm2pvalue
      
      CASRN2TermMatrix_lv1 <- lapply(1:nrow(DATA), function(line){
        tmpSplit <- vector("list", 13)
        tmpSplit[1]   <- as.character(DATA[line, "Category"])
        tmpSplit[2]   <- as.character(DATA[line, "Term"])
        tmpSplit[3]   <- as.character(DATA[line, "Count"])
        tmpSplit[4]   <- as.character(DATA[line, "X."])
        tmpSplit[5]   <- as.character(DATA[line, "PValue"])
        tmpSplit[6]   <- as.character(DATA[line, "CASRNs"])
        tmpSplit[7]   <- as.character(DATA[line, "List.Total"])
        tmpSplit[8]   <- as.character(DATA[line, "Pop.Hits"])
        tmpSplit[9]   <- as.character(DATA[line, "Pop.Total"])
        tmpSplit[10]  <- as.character(DATA[line, "Fold.Enrichment"])
        tmpSplit[11]  <- as.character(DATA[line, "Bonferroni"])
        tmpSplit[12]  <- as.character(DATA[line, "Benjamini"])
        tmpSplit[13]  <- as.character(DATA[line, "FDR"])
        
        if ( grepl("^\\D", tmpSplit[[sigColumnIndex]]) | as.double(tmpSplit[[sigColumnIndex]]) >= sigCutOff | as.double(tmpSplit[[10]]) < 1) {
          # skip
        } else {
          tmpTermKey <- paste0(tmpSplit[[2]], " | ", tmpSplit[[1]], sep="")
          CASRNs <- str_split(tmpSplit[[6]], ", ")
          tmpTermKeyList <- vector("list", length(unlist(CASRNs)))
          CASRN2TermMatrix_lv1_tmp <- lapply(tmpTermKeyList, function(x) tmpTermKey)
          names(CASRN2TermMatrix_lv1_tmp) <- unlist(CASRNs)
          return(CASRN2TermMatrix_lv1_tmp)
  
        }
      })
      
      #Get not null elements
      CASRN2TermMatrix_lv2 <- CASRN2TermMatrix_lv1[!sapply(CASRN2TermMatrix_lv1, is.null)]
      CASRN2TermMatrix_lv2 <- unlist(CASRN2TermMatrix_lv2, recursive = FALSE)
      
      # Merge same names
      CASRN2TermMatrix <- lapply(unique(names(CASRN2TermMatrix_lv2)), function(x) {
        unlist(unname(CASRN2TermMatrix_lv2[names(CASRN2TermMatrix_lv2) %in% x]))
      })
      names(CASRN2TermMatrix) <- unique(names(CASRN2TermMatrix_lv2))
                               
      # Now create new output files
      print(paste0("GCT file at: ,", outputDir, tmp2[length(tmp2)], sep="\t"))
      OUTFILE           <- file(paste0(outputDir, tmp2[length(tmp2)], ".gct", sep=""))
  
      if(typeof(CASRN2TermMatrix) == "list"){
        CASRNs            <- names(CASRN2TermMatrix)
      }
      else {
        CASRNs            <- colnames(CASRN2TermMatrix)
      }
      tmpTermKeys       <- names(term2pvalue)
      CASRNCount        <- length(CASRNs)
      tmpTermKeyCount   <- length(tmpTermKeys)
      outputContent     <- paste0("#1.2\n", CASRNCount, "\t", tmpTermKeyCount, "\nCASRN\tName", sep="")
      
      for(tmpTermKey in tmpTermKeys) {	
        outputContent <- paste0(outputContent, "\t", tmpTermKey, " | ", term2pvalue[tmpTermKey], sep="")
      }
      outputContent	<- paste0(outputContent, "\n", sep="")
      
      for(CASRN in unique(CASRNs)){
        outputContent	<- paste0(outputContent, CASRN, "\t", sep="")
        #print("^^")
        if (is.null(CASRN2Name[[CASRN]]) == FALSE){	
          termNameOut <- CASRN2Name[[CASRN]]
          if(nchar(termNameOut) > 30){ # If the term name is longer than 30 characters (arbitrary, can change later), truncate for readability on the heatmap images
            termNameOut <- paste0(substring(termNameOut, 1, 30), "...", sep="")
          }
          outputContent	<- paste0(outputContent, termNameOut, sep="")
        }
         
        for(tmpTermKey in tmpTermKeys){
          if (tmpTermKey %in% CASRN2TermMatrix[[CASRN]] == TRUE){
            outputContent	<- paste0(outputContent, "\t1", sep="")
          } else {
            outputContent	<- paste0(outputContent, "\t0", sep="")
          }
        }	
        outputContent	<- paste0(outputContent, "\n", sep="")
      }
                 
      writeLines(outputContent, OUTFILE) 
      close(OUTFILE)
    }
  }
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

###### Create clustering images ###### 
create_clustering_images <- function(outDir = "", imageFlags = "-color=BR"){
  
  # --------------------------------------
  # Define required variables - Clustering
  # --------------------------------------
  dirName					    <- outDir
  dirNameSplit				<- str_split(dirName, "/")[[1]]
  dirTypeTag					<- ""
  dirNameSplit <- dirNameSplit[nchar(dirNameSplit) > 0]
  if(grepl("CLUSTER", dirNameSplit[1], fixed=TRUE) == TRUE){
    dirTypeTag				<- "CLUSTER__"
  } else if(grepl("CHART", dirNameSplit[1], fixed=TRUE) == TRUE) {	
    dirTypeTag				<- "CHART__"
  } else if (grepl("PRESELECTED", dirNameSplit[1], fixed=TRUE) == TRUE){
    dirTypeTag				<- "PRESELECTED__"
  }
  
  outputBaseDir				      <- outDir
  libDir						        <- paste0(APP_DIR, "HClusterLibrary/")
  path_separator			      <- ';'
  log_transform				      <- "no"
  row_center					      <- "no"
  row_normalize				      <- "no"
  column_center				      <- "no"
  column_normalize		      <- "no"
  column_distance_measure	  <- "2"	#pearson correlation
  row_distance_measure		  <- "2"	#pearson correlation
  clustering_method			    <- "m"	#pairwise complete-linkage
  color_scheme				      <- "global"	# or "row normalized"
  color_palette				      <- ""
  
  # -------------------------
  # Handle additional options
  # -------------------------
  output_format			  <- "png"
  #-.jpeg, .png, .tiff, .bmp, .eps
  if (is.null(imageFlags) == FALSE & grepl("(jpeg|png|tiff|bmp|eps)", tolower(imageFlags), fixed=TRUE) == TRUE){	
    output_format			<- tolower(imageFlags)
  }
  color_palette <- paste0(libDir, "colorSchemeBlackRed.txt", sep="")

  # ---------------------------------------------
  # Define required variables - Clustering Images
  # ---------------------------------------------
  java_flags				    <- "-Djava.awt.headless=true -Xmx1024m"
  row_size				      <- "16"
  column_size			      <- "16"
  show_grid				      <- "yes"
  grid_color				    <- "0:0:0"
  show_row_description	<- "yes"
  show_row_names			  <- "yes"
  row_to_highlight		  <- ""
  row_highlight_color		<- ""
  use_color_gradient		<- "no"
  
  # ---------------------------
  # Check OS and define program - this is probably always going to be 64-bit Linux
  # ---------------------------
  cluster_program	      <- "clusterLinux64"
  
  # ------------------------------------------
  # Load directory list
  # ------------------------------------------
  baseSubDirs	      <- list()
  baseNameSplit	    <- str_split(dirName, "/")[[1]]
  baseNameSplit <- baseNameSplit[nchar(baseNameSplit) > 0]
  baseShortDirName  <- ''
  if (baseNameSplit[length(baseNameSplit)] == ""){
    baseShortDirName <- paste0(baseNameSplit[length(baseNameSplit)-1],'/')
  } else {
    baseShortDirName <- paste0(baseNameSplit[length(baseNameSplit)],'/')
  }
   
  DIR <- list.files(dirName)
  tmpDirs <- DIR
  
  # Remove current directory and previous directory
  baseSubDirs <- lapply(tmpDirs, function(x){
    if(x != "." & x != "..") {
      return(x)
    }
  })   
                     
                     
  # ------------------------------------------
  # Perform HClustering
  # ------------------------------------------
  perform_hclustering_per_directory (dirName, '', outputBaseDir, dirTypeTag, libDir, cluster_program, row_distance_measure, column_distance_measure, clustering_method, java_flags, output_format, column_size, row_size, show_grid, grid_color, show_row_description, show_row_names, row_to_highlight, row_highlight_color, color_scheme, color_palette, use_color_gradient)

  if (is.null(baseSubDirs[1]) == FALSE){
    #performHclusteringForEach <- mclapply(baseSubDirs, mc.cores=4, mc.silent=FALSE, function(x){
    performHclusteringForEach <- lapply(baseSubDirs, function(x){
      perform_hclustering_per_directory (paste0(dirName, '/', x, baseShortDirName, '/', sep=""), outputBaseDir, dirTypeTag, libDir, cluster_program, row_distance_measure, column_distance_measure, clustering_method, java_flags, output_format, column_size, row_size, show_grid, grid_color, show_row_description, show_row_names, row_to_highlight, row_highlight_color, color_scheme, color_palette, use_color_gradient)
    })
  }
  print("\n! COMPLETE ...\n")
  return("success")
}

# Generate heatmap images using HierarchicalClusteringImages (HCI)
perform_hclustering_per_directory <- function(givenDirName, additionalDirName, outputBaseDir, dirTypeTag, libDir, cluster_program, row_distance_measure, column_distance_measure, clustering_method, java_flags, output_format, column_size, row_size, show_grid, grid_color, show_row_description, show_row_names, row_to_highlight, row_highlight_color, color_scheme, color_palette, use_color_gradient) {

  # ------------------------------------------------------------------
  # Load GCT files for hierarchical clustering, in the given directory
  # ------------------------------------------------------------------
  gctFiles    <- Sys.glob(paste0(givenDirName,"/","*.gct", sep=""), dirmark=FALSE)
  
  tmp1			  <- str_split (givenDirName, "/")[[1]]
  tmp1        <- tmp1[nchar(tmp1) > 0]
  baseDirName	<- tmp1[length(tmp1)]
  outputDir		<- paste0(outputBaseDir, '/', sep="")
  create_sub_directory(outputDir)
   
  # TODO: Multithread this
  #generateImagesProcess <-mclapply (gctFiles, function(infile){
  generateImagesProcess <-lapply (gctFiles, function(infile){
    tmp1 <- str_split(infile, "/")[[1]]
    tmp1 <- tmp1[nchar(tmp1) > 0]
    tmp2 <- str_split(tmp1[length(tmp1)], ".gct")[[1]]
    tmp2 <- tmp2[nchar(tmp2) > 0]
                                         
    # Check gct file content and skip if there is less than 2 entries
    if(check_gct_contains_more_than_two_lines(infile) == FALSE){
      # Do nothing
    }
    else {
      output_base_name	  <- paste0(outputDir, dirTypeTag, tmp2[1])
      shorter_base_name		<- paste0(outputDir, dirTypeTag, tmp2[1])
      cluster_input_file	<- paste0(shorter_base_name,".txt")
     
      if (convert_gct_to_cluster_input_file(infile, cluster_input_file, outputDir) == TRUE){
        print(paste0("! clustering", infile, "\n", sep=""))
       
        system(paste0(libDir, cluster_program, " -f ", cluster_input_file, " -g ", row_distance_measure, " -e ", column_distance_measure, " -m ", clustering_method, sep=""))
        print("SYSTEM")
        print(paste0(libDir, cluster_program, " -f ", cluster_input_file, " -g ", row_distance_measure, " -e ", column_distance_measure, " -m ", clustering_method, sep=""))
        
        cdtFile	<- paste0(output_base_name, ".cdt", sep="")
        gtrFile	<- paste0(output_base_name, ".gtr", sep="")
        atrFile	<- paste0(output_base_name, ".atr", sep="")
       
        atrCmd <- ""
        gtrCmd <- ""
       
        if (row_distance_measure != 0){	
          gtrCmd <- paste0(" -x\"", gtrFile, "\"", sep="")
        }
  
        if (column_distance_measure != 0){	
          atrCmd <- paste0(" -y\"", atrFile, "\"", sep="")
        }
  
        #CLUS <- read.delim(cdtFile, sep="\t", comment.char="", quote="", stringsAsFactors = FALSE, header=TRUE, skip=2, fill=TRUE, check.names=FALSE)
       
        # Create heatmap image
        command	<- paste0("java ", java_flags, " -DlibDir=", libDir, " -jar ", libDir, "hclimage-o.jar \"", cdtFile, "\" \"", output_base_name, "\" ", output_format, " -c", column_size, " -r", row_size, " -g", show_grid, " -l", grid_color, " -a", show_row_description, " -s", show_row_names, " -n", color_scheme, " -m", color_palette, " -u", use_color_gradient, sep="")
        system(command)
       
      } else {
        print(paste0("conversion failed for ", infile, "\n", sep=""))
      }
    }
  })
}

convert_gct_to_cluster_input_file <- function(gctFile, clusterInputFile, outputDir){
  GCTFILE <- read.delim(gctFile, sep="\t", comment.char="", quote="", stringsAsFactors = FALSE, header=TRUE, skip=2, fill=TRUE, check.names=FALSE)
  print(paste0("reading: ", clusterInputFile))
  file.create(clusterInputFile)
  CLUSTER <- file(clusterInputFile)
  
  newHeader	  <- ""
  newHeader_tmp <- ""
  for (i in 3:length(colnames(GCTFILE))){
    newHeader_tmp <- paste0(newHeader_tmp, "\t1", sep="")
  }	
  newHeader <- paste("UNIQID\tNAME\tGWEIGHT\tGORDER\t", paste0(colnames(GCTFILE)[3:length(colnames(GCTFILE))], collapse="\t"), "\nEWEIGHT\t\t\t", newHeader_tmp, sep="")
  GORDER      <- 1
  newContent	<- ""
  newContent <- lapply(1:nrow(GCTFILE), function(i){
    line <- GCTFILE[i,]
    tmpSplit <- str_split(line, "\t")[[1]]
    tmpSplit <- tmpSplit[nchar(tmpSplit) > 0]
    stringToRet <- ""
    if(is.na(tmpSplit[1]) == FALSE){
      if(is.null(tmpSplit[1]) == FALSE & tmpSplit[1] != ""){
        stringToRet <- paste0(tmpSplit[1], "\t", "", "\t1\t", GORDER, "\t", paste0(line[3:length(line)], collapse="\t"))
      }
    }
    GORDER <<- GORDER + 1
    return(stringToRet)
    
  })	
  
  write(paste0(newHeader, "\n", paste0(newContent,collapse="\n"), sep=""), CLUSTER, append=TRUE)
  return(1)
}



create_sub_directory <- function(outputDir){
  tmp1 <- str_split(outputDir, "/")[[1]]
  tmp1 <- tmp1[nchar(tmp1) > 0]
  dirName <- paste0("/", tmp1, "/", sep="", collapse="")
  print("creating directory at: ")
  print(dirName)
  dir.create(dirName)
}

# TODO: Make sure this is working with files of different sizes
check_gct_contains_more_than_two_lines <- function(infile){
  print(paste0("in here? at: ", infile, sep=""))
  INFILE <- read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors = FALSE, header=TRUE, skip=2, fill=TRUE)
  lineCount	<- 0
  # TODO: calculate lineCount by taking length of list
  
  print("INFILE")
  print(INFILE)
  lineCount <- nrow(INFILE)
  
  #checkGct <- lapply(1:nrow(INFILE), function(line){
  #  INFILE[line,] <<- gsub("[[:space:]]", "", INFILE[line,]) 
  #  
  #  print("INFILE[line,]")
  #  print(INFILE[line,])
  #  
  #  if (paste0(INFILE[line,], collapse="\t") == ""){
  #    # Do nothing
  #  }	else {
  #    lineCount <<- lineCount + 1
  #  }
  #})
  
  
  if(lineCount >=2){
    return(1)
  } else {
    return(0)
  }
}

###### Create DAVID Chart and Cluster files ######
create_david_chart_cluster <- function(baseDirName="", topTermLimit=10, mode="ALL", sigColumnName="P", sigCutoff=0.05, valueColumnName="P"){
  
  # -------------------------------------
  # Get the corresponding directory names
  # -------------------------------------
  baseNameSplit <- str_split(baseDirName, "/")[[1]]
  baseNameSplit <- baseNameSplit[nchar(baseNameSplit) > 0] #remove "" elements in split list
  
  baseShortDirName <- ""
  if (baseNameSplit[length(baseNameSplit)] == ""){
    baseShortDirName <- paste0(baseNameSplit[length(baseNameSplit)-1], '/', sep="")
  } else {
    baseShortDirName <- paste0(baseNameSplit[length(baseNameSplit)], '/', sep="")
  }
  annotationBaseDir <- "Annotation/"
  baseOutputDir <- paste0(baseDirName, '/gct/', sep="")
  
  print("baseOutputDir")
  print(baseOutputDir)
  
     
  if (dir.exists(baseOutputDir)) {
    # do nothing if directory already exists (i.e., we are regenerating the network)
    print("Found existing directory: $baseOutputDir")
  } else {
    # else create the directory if this is the first time we are dealing with this data set
    print("No directory found for $baseOutputDir. Making...")
    dir.create(baseOutputDir)
  }

  # -------------------------------------
  # Load term annotation data
  # -------------------------------------
  
  # Get data saved from tox21enricher database
  
  # Load Annotations from data table
  className2classID	<- lapply(1:nrow(outpClasses), function(line) outpClasses[line, "annoclassid"])
  names(className2classID)	<- lapply(1:nrow(outpClasses), function(line) outpClasses[line, "annoclassname"])
  classID2className	<- lapply(1:nrow(outpClasses), function(line) outpClasses[line, "annoclassname"])
  names(classID2className) <- lapply(1:nrow(outpClasses), function(line) outpClasses[line, "annoclassid"])
  
  #TODO: Map
  classID2annotationTerm2termUniqueID_lv1 <- lapply(split(outpAnnoDetail, outpAnnoDetail$annoclassid), function(x) {
    return(split(x, x$annoterm))
  })
  classID2annotationTerm2termUniqueID <- lapply(classID2annotationTerm2termUniqueID_lv1, function(x) {
    return(lapply(x, function(y) {
      inner_classID2annotationTerm2termUniqueID <- lapply(y$annotermid, function(z) y$annotermid) 
      names(inner_classID2annotationTerm2termUniqueID) <- y$annotermid
      return(inner_classID2annotationTerm2termUniqueID)
    }))
  })
  
  # -------------------------------------
  # Enumerate all possible directories
  # -------------------------------------
                                                                                  # ARGV[1]       ARGV[2] ARGV[3]        ARGV[4]    ARGV[5]
  process_variable_DAVID_CHART_directories (baseDirName, baseOutputDir, "", "",   topTermLimit,   mode,   sigColumnName, sigCutoff, valueColumnName, className2classID, classID2annotationTerm2termUniqueID)
  process_variable_DAVID_CLUSTER_directories (baseDirName, baseOutputDir, "", "", topTermLimit,   mode,   sigColumnName, sigCutoff, valueColumnName, className2classID, classID2annotationTerm2termUniqueID)
  
}


process_variable_DAVID_CLUSTER_directories <- function(dirName, outputDir, extTag, additionalFileName, topTermLimit,   mode,   sigColumnName, sigCutoff, valueColumnName, className2classID, classID2annotationTerm2termUniqueID){
  
  # Check the directory for any file
  print("! Processing $dirName CLUSTER ...\n")
  infiles <- Sys.glob(paste0(dirName,"/*_Cluster.txt"))
  
  print("infiles")
  print(infiles)
  
  
  
  if (is.null(infiles[1])){
    return(FALSE)
  }
  
  # Define number of top clusters
  dir.create(outputDir)
  dirInputName	      <- dirName
  dirInputExpression	<- paste0(dirInputName, "ExpressionData/", sep="")
  sigColumnIndex			<- get_column_index(sigColumnName) # 5=p-value, 12=BH p-value, 13=FDR
  valueColumnIndex		<- get_column_index(valueColumnName)
  summaryFileNameBase	<- additionalFileName
  summaryFileNameExt	<- extTag
  dirNameSplit			  <- str_split(dirName, "/")[[1]]
  if (is.null(dirNameSplit[length(dirNameSplit)]) == FALSE) {
    summaryFileNameBase 	<- paste0(summaryFileNameBase, "Cluster_Top", topTermLimit, "_", mode, "__", sigColumnName, "_", sigCutoff, "_", valueColumnName, sep="")
    summaryFileNameExt		<- paste0(summaryFileNameExt,  "Cluster_Top", topTermLimit, "_", mode, "__", sigColumnName, "_", sigCutoff, "_", valueColumnName, sep="")
  } else {
    summaryFileNameBase 	<- paste0(summaryFileNameBase, "Cluster_Top", topTermLimit, "_", mode, "__", sigColumnName, "_", sigCutoff, "_", valueColumnName, sep="")
    summaryFileNameExt		<- paste0(summaryFileNameExt,  "Cluster_Top", topTermLimit, "_", mode, "__", sigColumnName, "_", sigCutoff, "_", valueColumnName, sep="")
  }
  
  
  # -----------------------------------------------------------------------------
  # Load DAVID cluster files
  
  ID2Term			    <- list()
  ID2Class		    <- list()
  fileHeaderNames <- list()
  pvalueMatrix	  <- list()
  fcMatrix		    <- list()
  upDownMatrix	  <- list()
  
  # Additional variables/hashes/arrays for expression details
  geneID2expDetails	  <- list()
  geneID2Description	<- list()
  allBaseFileNames	  <- list()
  gctFileHeader		    <- ""
  geneID2ExpProfile	  <- list()
  gctFileStatus		    <- 0
  
  # Load gct file, if available
  # check gct file
  gctFiles <- Sys.glob(paste0(dirInputName, "/*.gct"))
  if (length(gctFiles) > 0) {
    loadGctTmp <- load_gct_file_as_profile(gctFiles[1], geneID2ExpProfile)
    gctFileStatus <- loadGctTmp[1]
    gctFileHeader <- loadGctTmp[2]
    geneID2ExpProfile <- loadGctTmp[3]
  }
  
  # Step1. Get the list of significant terms
  # TODO: convert to multithreaded
  #getSigTerms <- mclapply(infiles, function(infile) {
  
  if(length(infiles) < 1){
    return(FALSE)
  }

  getSigTerms <- lapply(infiles, function(infile) {

    
    tmp1 <- str_split(infile, "/")[[1]]
    tmpNameSplit <- str_split(tmp1[length(tmp1)], "__Cluster.txt")[[1]]
    shortFileBaseName	<- tmpNameSplit[1]
    originalSourceFile <- paste0(dirInputName, "/", shortFileBaseName, ".txt")

    DATA <- read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors = FALSE, header=FALSE, fill=TRUE, col.names=c("Category","Term","Count","%","PValue","CASRNs","List","Total","Pop Hits","Pop Total","Fold Enrichment","Bonferroni","Benjamini","FDR"))
    
    lines <- DATA
    
    print("lines")
    print(lines)
    
    if(nrow(lines) < 1){
      return(FALSE)
    }
    
    tmpIDList <- lapply(1:nrow(lines), function(i){
      tmpSplit <- lapply(lines[i,], function(lineItem) {
        return(lineItem)
      })
      
      #print(">>> inner tmpSplit")
      #print(tmpSplit)
      
      if(grepl("^Annotation Cluster", tmpSplit[[1]])) {
        firstClusterIndex <- i + 2
        
        tmpSplitInner <- lapply(lines[firstClusterIndex,], function(lineItem) {
          return(lineItem)
        })
        
        print("inner index")
        print(tmpSplitInner[[sigColumnIndex]])
        print(sigColumnIndex)
        
        if (tmpSplitInner[[sigColumnIndex]] == "" | is.na(tmpSplitInner[[sigColumnIndex]]) == TRUE | grepl("^\\D", tmpSplitInner[[sigColumnIndex]]) == TRUE | as.double(tmpSplitInner[[sigColumnIndex]]) >= sigCutoff | as.double(tmpSplitInner[[10]]) < 1) {
          #    # do nothing
          return(NULL)
        } else {
          tmpID	<- paste0(tmpSplitInner[[1]], " | ", tmpSplitInner[[2]], sep="")
          return(tmpID)
        }
      }
    })
    
    #Get not null elements
    tmpIDList <- tmpIDList[!sapply(tmpIDList, is.null)]
    tmpIDList <- unlist(tmpIDList, recursive = FALSE)
    
    # Cut off entries over the limit
    if(length(tmpIDList) > as.double(topTermLimit)){
      tmpIDList <- tmpIDList[1:(as.double(topTermLimit))]
    }
    
    print("tmpIDList")
    print(tmpIDList)
    
    tmp_ID2Term <- lapply(1:nrow(lines), function(i){
      tmpSplit <- lapply(lines[i,], function(lineItem) {
        return(lineItem)
      })
      if(grepl("^Annotation Cluster", tmpSplit[[1]])) {
        firstClusterIndex <- i + 2
        tmpSplitInner <- lapply(lines[firstClusterIndex,], function(lineItem) {
          return(lineItem)
        })
        
        if (tmpSplitInner[[sigColumnIndex]] == "" | is.na(tmpSplitInner[[sigColumnIndex]]) == TRUE | grepl("^\\D", tmpSplitInner[[sigColumnIndex]]) == TRUE | as.double(tmpSplitInner[[sigColumnIndex]]) >= sigCutoff | as.double(tmpSplitInner[[10]]) < 1) {
          # do nothing
        } else {
          tmpTerm <- paste0(tmpSplitInner[[1]], " | ", tmpSplitInner[[2]])
          return(tmpTerm)
        }
      }
    })
    #Get not null elements
    tmp_ID2Term <- tmp_ID2Term[!sapply(tmp_ID2Term, is.null)]
    tmp_ID2Term <- unlist(tmp_ID2Term, recursive = FALSE)
    
    # Cut off entries over the limit
    if(length(tmp_ID2Term) > as.double(topTermLimit)){
      tmp_ID2Term <- tmp_ID2Term[1:(as.double(topTermLimit))]
    }
    
    tmp_ID2Class <- lapply(1:nrow(lines), function(i){
      tmpSplit <- lapply(lines[i,], function(lineItem) {
        return(lineItem)
      })
      if(grepl("^Annotation Cluster", tmpSplit[[1]])) {
        firstClusterIndex <- i + 2
        tmpSplitInner <- lapply(lines[firstClusterIndex,], function(lineItem) {
          return(lineItem)
        })
        
        if (tmpSplitInner[[sigColumnIndex]] == "" | is.na(tmpSplitInner[[sigColumnIndex]]) == TRUE | grepl("^\\D", tmpSplitInner[[sigColumnIndex]]) == TRUE | as.double(tmpSplitInner[[sigColumnIndex]]) >= sigCutoff | as.double(tmpSplitInner[[10]]) < 1) {
          # do nothing
        } else {
          return(tmpSplitInner[[1]])
        }
      }
    })
    #Get not null elements
    tmp_ID2Class <- tmp_ID2Class[!sapply(tmp_ID2Class, is.null)]
    tmp_ID2Class <- unlist(tmp_ID2Class, recursive = FALSE)
    
    # Cut off entries over the limit
    if(length(tmp_ID2Class) > as.double(topTermLimit)){
      tmp_ID2Class <- tmp_ID2Class[1:(as.double(topTermLimit))]
    }
    
    names(tmp_ID2Term) <- tmpIDList
    names(tmp_ID2Class) <- tmpIDList
    
    return(list(ID2Term=tmp_ID2Term, ID2Class=tmp_ID2Class))
    
  })
  
  getSigTermsFilteredTerms <- lapply(1:length(getSigTerms), function(i){
    return(getSigTerms[[i]]["ID2Term"])
  })
  
  getSigTermsFilteredClasses <- lapply(1:length(getSigTerms), function(i){
    return(getSigTerms[[i]]["ID2Class"])
  })
  
  #TODO: make this more readable
  ID2Term  <- unique(unlist(unname(unlist(getSigTermsFilteredTerms,   recursive = FALSE)), recursive = FALSE))
  ID2Class <- unique(unlist(unname(unlist(getSigTermsFilteredClasses, recursive = FALSE)), recursive = FALSE))
  # remove NA
  ID2Term <- ID2Term[!sapply(ID2Term, is.na)]
  ID2Class <- ID2Class[!sapply(ID2Class, is.na)]
  
  #IDs <- names(ID2Term)
  # For cluster, we want to append the class name before the term as a given term may appear in multiple classes. If that happens, heatmap generation will be messed up.
  IDs <- ID2Term
  
  for(infile in infiles) {
    tmp1 <- str_split(infile, "/")[[1]]
    tmpNameSplit <- str_split(tmp1[length(tmp1)], "__Cluster.txt")
    shortFileBaseName	<- tmpNameSplit[1]
    
    tmp2 <- str_split(tmp1[length(tmp1)], ".xls")[[1]]
    if(length(tmp2[1]) == length(tmp1[length(tmp1)])) {
      tmp2 <- str_split(tmp1[length(tmp1)], ".txt")[[1]]
    }
                                        
    tmp3 <- str_split(tmp2[1], "__Cluster")[[1]]
    fileHeaderNames <- append(fileHeaderNames, tmp3[1])
                                        
    # ------------------------------------------------------------
    # Load expression data if any
    expressionDataExist <- 0
    expressionData		  <- list()
    expressionDataFC	  <- list()
    expressionHeader    <- ""
    
    # ------------------------------------------------------------
    # Check term file and load
    # TODO: Do we need to do this again here? -> amendment: probably not
    # try this with a trycatch

    DATA <- tryCatch(read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors = FALSE, header=TRUE, fill=TRUE, skip=1),
                     error=function(cond) {
                       print("no lines in input file")
                       return(data.frame())
                     })

    #if(nrow(DATA) < 1) {
    #  return(FALSE)
    #}

    if(nrow(DATA) > 0){
      #TODO: clean this up like the above code
      for(line in 1:nrow(DATA)) {
        tmpSplit <- lapply(DATA[line,], function(lineItem) {
          return(lineItem)
        })
      
        if (tmpSplit[[sigColumnIndex]] == "" | is.null(tmpSplit[[sigColumnIndex]]) == TRUE | is.null(tmpSplit[[10]]) == TRUE | grepl("^\\D", tmpSplit[[sigColumnIndex]]) == TRUE | (mode == "ALL" & as.double(tmpSplit[[sigColumnIndex]]) >= sigCutoff) | as.double(tmpSplit[[10]]) < 1) {
          # Do nothing
        } else {
          tmpID	<- paste0(tmpSplit[[1]], " | ", tmpSplit[[2]])
          if (is.null(ID2Term[tmpID]) == FALSE) {
            pvalueMatrix[[tmpID]][tmp3[1]]  <- -1 * log10(as.double(tmpSplit[[valueColumnIndex]]))
            fcMatrix[[tmpID]][tmp3[1]]      <- tmpSplit[10]
          }
        }
      }
    }
  }
  
  
  
  # Create a summary file
  summaryFileName <- paste0(summaryFileNameBase, "__ValueMatrix.txt", sep="")
  
  file.create(paste0(outputDir, summaryFileName, sep=""))
  SUMMARY <- file(paste0(outputDir, summaryFileName, sep=""))
  
  fileHeaderNames <- unlist(fileHeaderNames)
  fileHeaderNames <- sort_by_file_number(fileHeaderNames)
  
  print(paste0("! Creating summary file at: ", paste0(outputDir, summaryFileName)))
  writeToSummary <- paste0("GROUP\tID\tTerms\t", paste0(fileHeaderNames, collapse="\t"), "\n", sep="")
  writeToSummary2 <- ""
  
  #Get not null elements
  IDs <- IDs[!sapply(IDs, is.na)]
  IDs <- unlist(IDs, recursive = FALSE)

  # TODO: This, but efficiently
  for(ID in IDs) {	
    # TODO: to make this faster, we could create a data frame in here and then just write that to the file at the end
    
    writeToSummary2 <- paste0(writeToSummary2, ID2Class[ID],"\t",ID,"\t")
    IDSplit <- str_split(ID, "\\|")[[1]]
    for(header in fileHeaderNames){
      if (is.null(pvalueMatrix[[ID]][header]) == FALSE | is.na(pvalueMatrix[[ID]][header]) == FALSE){
        if(header %in% names(pvalueMatrix[[ID]])) {
          writeToSummary2 <- paste0(writeToSummary2, "\t", pvalueMatrix[[ID]][header])
        } else {
          writeToSummary2 <- paste0(writeToSummary2, "\t0")
        }
      } else {
        writeToSummary2 <- paste0(writeToSummary2, "\t")
      }
    }
    writeToSummary2 <- paste0(writeToSummary2, "\n")
  }	
  
  writeToSummaryList <- paste0(unique(unlist(str_split(writeToSummary2, "\n"))), collapse="\n")
  write(paste0(writeToSummary, writeToSummaryList, sep=""), SUMMARY, append=TRUE)
  
  close(SUMMARY)
  
  # Create a network summary file for Cluster
  ForNetworkFile <- paste0(summaryFileNameBase, "__ValueMatrix.ForNet")
  file.create(paste0(outputDir, ForNetworkFile))
  NETWORK <- file(paste0(outputDir, ForNetworkFile))
  writeToNetwork <- paste0("GROUPID\tUID\tTerms\t", paste0(fileHeaderNames, collapse="\t"), "\n", sep="")
  writeToNetwork2 <- ""
  # TODO: This, but efficiently
  
  for(ID in IDs) {
    idTermSplits <- str_split(ID, " \\| ")[[1]]
    tmpHashRef	 <- classID2annotationTerm2termUniqueID[[className2classID[[idTermSplits[1]]]]]
    
    # TODO want to have the names for this just be the terms and not the classes
    
    writeToNetwork2 <- paste0(writeToNetwork2, className2classID[[idTermSplits[1]]], "\t", tmpHashRef[[idTermSplits[2]]], "\t", idTermSplits[2])
    for (header in fileHeaderNames) {
      if (is.na(unname(pvalueMatrix[[ID]][header])) == FALSE) {
        writeToNetwork2 <- paste0(writeToNetwork2, "\t", pvalueMatrix[[ID]][header])
      } else {
        writeToNetwork2 <- paste0(writeToNetwork2, "\t")
      }
    }
    writeToNetwork2 <- paste0(writeToNetwork2, "\n")
  }
  # remove trailing newline
  writeToNetwork2 <- substr(writeToNetwork2, 1, nchar(writeToNetwork2)-1)
  
  write(paste0(writeToNetwork, writeToNetwork2), NETWORK, append=TRUE)
  close(NETWORK)
  
  # Create a gct file from ValueMatrix
  INFILE <- read.delim(paste0(outputDir, summaryFileNameBase, "__ValueMatrix.txt"), sep="\t", comment.char="", quote="", stringsAsFactors = FALSE, header=TRUE, fill=TRUE)
  
  file.create(paste0(outputDir, summaryFileNameBase, "__ValueMatrix.gct"))
  OUTFILE <- file(paste0(outputDir, summaryFileNameBase, "__ValueMatrix.gct"))
  
  #headerLine	<- paste0(INFILE[1,], collapse="\t")
  headerLine	<- paste0(colnames(INFILE), collapse="\t")
  headerSplit	<- str_split(headerLine, "\t")[[1]]
  sampleCnt	  <- length(headerSplit) - 3
  geneCnt  <- 0
  content	<- ""
  
  if(nrow(INFILE) > 0){
    for(l in 1:nrow(INFILE)) {
      line <- paste0(INFILE[l,], collapse="\t")
      if (line == "") {
        # Do nothing
      } else {
        tmpSplit <- str_split(line, "\t")[[1]]
        for (i in 4:(sampleCnt+3)) {
          if (tmpSplit[i] == "NA" | is.na(tmpSplit[i]) == TRUE | length(tmpSplit[i]) < 1 | is.null(tmpSplit[i]) == TRUE) {
            tmpSplit[i] <- 0
          }
        }
        
        tmpSplit <- tmpSplit[-(1:1)] #Shift
        content <- paste0(content, paste0(tmpSplit, collapse="\t"), "\n")
        geneCnt <- geneCnt + 1
      }
    }	
  }
  write(paste0("#1.2\n", geneCnt, "\t", sampleCnt, "\n", paste0(colnames(INFILE)[-(1:1)],collapse="\t"), "\n", content), OUTFILE, append=TRUE)
  close(OUTFILE)
}	

process_variable_DAVID_CHART_directories <- function(dirName, outputDir, extTag, additionalFileName, topTermLimit,   mode,   sigColumnName, sigCutoff, valueColumnName, className2classID, classID2annotationTerm2termUniqueID){
  
  # Check the directory for any file
  print(paste0("! Processing $dirName CHART ...", dirName))
  infilesAll <- Sys.glob(paste0(dirName,"/*_Chart.txt"))
  
  print("infilesAll")
  print(infilesAll)
  
  if (is.null(infilesAll[1])){
    return(FALSE)
  }
  
  # Define number of top clusters
  dir.create(outputDir)
  dirInputName	      <- dirName
  dirInputExpression	<- paste0(dirInputName, "ExpressionData/", sep="")
  sigColumnIndex			<- get_column_index(sigColumnName) # 5=p-value, 12=BH p-value, 13=FDR
  valueColumnIndex		<- get_column_index(valueColumnName)
  summaryFileNameBase	<- additionalFileName
  summaryFileNameExt	<- extTag
  dirNameSplit			  <- str_split(dirName, "/")[[1]]
  if (is.null(dirNameSplit[length(dirNameSplit)]) == FALSE) {
    summaryFileNameBase 	<- paste0(summaryFileNameBase, "Chart_Top", topTermLimit, "_", mode, "__", sigColumnName, "_", sigCutoff, "_", valueColumnName, sep="")
    summaryFileNameExt		<- paste0(summaryFileNameExt,  "Chart_Top", topTermLimit, "_", mode, "__", sigColumnName, "_", sigCutoff, "_", valueColumnName, sep="")
  } else {
    summaryFileNameBase 	<- paste0(summaryFileNameBase, "Chart_Top", topTermLimit, "_", mode, "__", sigColumnName, "_", sigCutoff, "_", valueColumnName, sep="")
    summaryFileNameExt		<- paste0(summaryFileNameExt,  "Chart_Top", topTermLimit, "_", mode, "__", sigColumnName, "_", sigCutoff, "_", valueColumnName, sep="")
  }
  
  # -----------------------------------------------------------------------------
  # Load DAVID Chart files
  ID2Term			    <- list()
  ID2Class		    <- list()
  fileHeaderNames <- list()
  pvalueMatrix	  <- list()
  fcMatrix		    <- list()
  upDownMatrix	  <- list()
  
  # Additional variables/hashes/arrays for expression details
  geneID2expDetails	  <- list()
  geneID2Description	<- list()
  allBaseFileNames	  <- list()
  gctFileHeader		    <- ""
  geneID2ExpProfile	  <- list()
  gctFileStatus		    <- 0

  
  
  
  # Load gct file, if available
  # check gct file
  gctFiles <- Sys.glob(paste0(dirInputName, "/*.gct"))
  
  print("gctFiles")
  print(gctFiles)

  #if (is.null(gctFiles[1]) == FALSE) {
  if (length(gctFiles) > 0) {
    # TODO: fix this vvv
    loadGctTmp <- load_gct_file_as_profile(gctFiles[1], geneID2ExpProfile)
    gctFileStatus <- loadGctTmp[1]
    gctFileHeader <- loadGctTmp[2]
    geneID2ExpProfile <- loadGctTmp[3]
  }
  
  # Step0. Get only the files that actually have contents
  infiles <- lapply(infilesAll, function(infile) {
    DATA <- read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors = FALSE, header=TRUE, fill=TRUE)
    #if(nrow(DATA) < 1){
      # make blank cluster file and return null here
    #  return(NULL)
    #} 
    return(infile)
  })
  infiles <- infiles[!sapply(infiles, is.null)]
  print("infiles")
  print(infiles)
  
  
  
  if (length(infiles) < 1) {
    return (FALSE)
  }
                            
  # Step1. Get the list of significant terms
  
  # TODO: convert to multithreaded
  #getSigTerms <- mclapply(infiles, function(infile) {
  getSigTerms <- lapply(infiles, function(infile) {
    tmp1 <- str_split(infile, "/")[[1]]
    tmpNameSplit <- str_split(tmp1[length(tmp1)], "__Chart.txt")[[1]]
    shortFileBaseName	<- tmpNameSplit[1]
    originalSourceFile <- paste0(dirInputName, "/", shortFileBaseName, ".txt")
    DATA <- read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors = FALSE, header=TRUE, fill=TRUE)
    lines <- DATA
    
    if(nrow(lines) < 1){
      return(FALSE)
    }

    tmpIDList <- lapply(1:nrow(lines), function(i){
      tmpSplit <- lapply(lines[i,], function(lineItem) {
        return(lineItem)
      })
      if (tmpSplit[[sigColumnIndex]] == "" | is.na(tmpSplit[[sigColumnIndex]]) == TRUE | grepl("^\\D", tmpSplit[[sigColumnIndex]]) == TRUE | tmpSplit[[sigColumnIndex]] >= sigCutoff | tmpSplit[[10]] < 1) {
        #    # do nothing
      } else {
        
        tmpID	<- paste0(tmpSplit[[2]],sep="")
      }
    })
    
    #Get not null elements
    tmpIDList <- tmpIDList[!sapply(tmpIDList, is.null)]
    tmpIDList <- tmpIDList[!sapply(tmpIDList, is.na)]
    tmpIDList <- unlist(tmpIDList, recursive = FALSE)
    
    # Cut off entries over the limit
    if(length(tmpIDList) > as.double(topTermLimit)){
      tmpIDList <- tmpIDList[1:as.double(topTermLimit)]
    }
    
    tmp_ID2Term <- lapply(1:nrow(lines), function(i){
      tmpSplit <- lapply(lines[i,], function(lineItem) {
        return(lineItem)
      })

      if (tmpSplit[[sigColumnIndex]] == "" | is.na(tmpSplit[[sigColumnIndex]]) == TRUE | grepl("^\\D", tmpSplit[[sigColumnIndex]]) == TRUE | tmpSplit[[sigColumnIndex]] >= sigCutoff | tmpSplit[[10]] < 1) {
        # do nothing
      } else {
        tmpTerm <- paste0(tmpSplit[[1]], "|", tmpSplit[[2]])
        return(tmpTerm)
      }
    })
    
    
    
    #Get not null elements
    tmp_ID2Term <- tmp_ID2Term[!sapply(tmp_ID2Term, is.null)]
    tmp_ID2Term <- tmp_ID2Term[!sapply(tmp_ID2Term, is.na)]
    tmp_ID2Term <- unlist(tmp_ID2Term, recursive = FALSE)
    
    # Cut off entries over the limit
    if(length(tmp_ID2Term) > as.double(topTermLimit)){
      tmp_ID2Term <- tmp_ID2Term[1:as.double(topTermLimit)]
    }

    tmp_ID2Class <- lapply(1:nrow(lines), function(i){
      tmpSplit <- lapply(lines[i,], function(lineItem) {
        return(lineItem)
      })
      
      if (tmpSplit[[sigColumnIndex]] == "" | is.na(tmpSplit[[sigColumnIndex]]) == TRUE | grepl("^\\D", tmpSplit[[sigColumnIndex]]) == TRUE | tmpSplit[[sigColumnIndex]] >= sigCutoff | tmpSplit[[10]] < 1) {
        # do nothing
      } else {
        return(tmpSplit[[1]])
      }
    })
    #Get not null elements
    tmp_ID2Class <- tmp_ID2Class[!sapply(tmp_ID2Class, is.null)]
    tmp_ID2Class <- tmp_ID2Class[!sapply(tmp_ID2Class, is.na)]
    tmp_ID2Class <- unlist(tmp_ID2Class, recursive = FALSE)
    
    # Cut off entries over the limit
    if(length(tmp_ID2Class) > as.double(topTermLimit)){
      tmp_ID2Class <- tmp_ID2Class[1:as.double(topTermLimit)]
    }
    
    names(tmp_ID2Term) <- tmpIDList
    names(tmp_ID2Class) <- tmpIDList
    
    return(list(ID2Term=tmp_ID2Term, ID2Class=tmp_ID2Class))
    
  })
  
  #print("getSigTerms before")
  #print(getSigTerms)
  
  #getSigTerms <- getSigTerms[1:as.double(topTermLimit)]
  
  print("getSigTerms after")
  print(getSigTerms)
  
  getSigTermsFilteredTerms <- lapply(1:length(getSigTerms), function(i){
    return(getSigTerms[[i]]["ID2Term"])
  })
  
  getSigTermsFilteredClasses <- lapply(1:length(getSigTerms), function(i){
    return(getSigTerms[[i]]["ID2Class"])
  })

  ID2Term <- unlist(unname(unlist(getSigTermsFilteredTerms, recursive = FALSE)), recursive = FALSE)
  ID2Class <- unlist(unname(unlist(getSigTermsFilteredClasses, recursive = FALSE)), recursive = FALSE)
  # remove NA
  ID2Term <- ID2Term[!sapply(ID2Term, is.na)]
  ID2Class <- ID2Class[!sapply(ID2Class, is.na)]
  
  #TODO: this works differently for Chart
  print("ID2Class")
  print(ID2Class)
  if (length(ID2Class) > 0){
    IDs <- paste0(ID2Class, " | ", names(ID2Class))
  } else {
    IDs <- list()
  }
  
  for(infile in infiles) {
    tmp1 <- str_split(infile, "/")[[1]]
    tmpNameSplit <- str_split(tmp1[length(tmp1)], "__Chart.txt")
    shortFileBaseName	<- tmpNameSplit[1]
    
    tmp2 <- str_split(tmp1[length(tmp1)], ".xls")[[1]]
    if(length(tmp2[1]) == length(tmp1[length(tmp1)])) {
      tmp2 <- str_split(tmp1[length(tmp1)], ".txt")[[1]]
    }
    
    tmp3 <- str_split(tmp2[1], "__Chart")[[1]]
    fileHeaderNames <- append(fileHeaderNames, tmp3[1])
    
    # ------------------------------------------------------------
    # Load expression data if any
    expressionDataExist <- 0
    expressionData		  <- list()
    expressionDataFC	  <- list()
    expressionHeader    <- ""
    
    # ------------------------------------------------------------
    # Check term file and load
    # TODO: Do we need to do this again here? -> amendment: probably not
    DATA <- read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors = FALSE, header=TRUE, fill=TRUE)

    #TODO: clean this up like the above code
    if (nrow(DATA) > 0){
      for(line in 1:nrow(DATA)) {
        tmpSplit <- lapply(DATA[line,], function(lineItem) {
          return(lineItem)
        })
        
        if (tmpSplit[[sigColumnIndex]] == "" | is.null(tmpSplit[[sigColumnIndex]]) == TRUE | is.null(tmpSplit[[10]]) == TRUE | grepl("^\\D", tmpSplit[[sigColumnIndex]]) == TRUE | (mode == "ALL" & tmpSplit[[sigColumnIndex]] >= sigCutoff) | tmpSplit[[10]] < 1) {
          # Do nothing
        } else {
          tmpID	<- paste0(tmpSplit[[1]], " | ", tmpSplit[[2]])
          if (is.null(ID2Term[tmpID]) == FALSE) {
            pvalueMatrix[[tmpID]][tmp3[1]]  <- -1 * log10(as.double(tmpSplit[[valueColumnIndex]]))
            fcMatrix[[tmpID]][tmp3[1]]      <- tmpSplit[[10]]
          }
        }
      }
    }
  }
  
  # Create a summary file
  summaryFileName <- paste0(summaryFileNameBase, "__ValueMatrix.txt", sep="")
  
  file.create(paste0(outputDir, summaryFileName, sep=""))
  SUMMARY <- file(paste0(outputDir, summaryFileName, sep=""))

  fileHeaderNames <- unlist(fileHeaderNames)
  fileHeaderNames <- sort_by_file_number(fileHeaderNames)
  
  print(paste0("! Creating summary file at: ", paste0(outputDir, summaryFileName)))
  writeToSummary <- paste0("GROUP\tID\tTerms\t", paste0(fileHeaderNames, collapse="\t"), "\n", sep="")
  writeToSummary2 <- ""
  
  print("IDs")
  print(IDs)
  
  # TODO: This, but efficiently
  for(ID in IDs) {	
    
    writeToSummary2 <- paste0(writeToSummary2, ID2Class[ID],"\t",ID,"\t")
    # IDToUse == ID if doing Chart because of the way the file is formatted
    IDToUse <- ID
    
    print("pvalueMatrix")
    print(pvalueMatrix)
    
    for(header in fileHeaderNames){
      print("pvalueMatrix[[IDToUse]]")
      print(IDToUse)
      print(pvalueMatrix[[IDToUse]])
      if (is.null(pvalueMatrix[[IDToUse]][header]) == FALSE | is.na(pvalueMatrix[[IDToUse]][header]) == FALSE){
        if(header %in% names(pvalueMatrix[[IDToUse]])) {
          writeToSummary2 <- paste0(writeToSummary2, "\t", pvalueMatrix[[IDToUse]][header])
        } else {
          writeToSummary2 <- paste0(writeToSummary2, "\t0")
        }
      } else {
        writeToSummary2 <- paste0(writeToSummary2, "\t")
      }
    }
    writeToSummary2 <- paste0(writeToSummary2, "\n")
  }	
  
  # Delete duplicate lines
  writeToSummaryList <- paste0(unique(unlist(str_split(writeToSummary2, "\n"))), collapse="\n")
  print("writeToSummaryList")
  print(writeToSummaryList)
  
  print("paste0(writeToSummary, writeToSummary2)")
  print(paste0(writeToSummary, writeToSummary2))
  
  write(paste0(writeToSummary, writeToSummaryList), SUMMARY, append=TRUE)
  
  close(SUMMARY)

  
  # Create a network summary file for Chart
  ForNetworkFile <- paste0(summaryFileNameBase, "__ValueMatrix.ForNet")
  file.create(paste0(outputDir, ForNetworkFile))
  NETWORK <- file(paste0(outputDir, ForNetworkFile))
  writeToNetwork <- paste0("GROUPID\tUID\tTerms\t", paste0(fileHeaderNames, collapse="\t"), "\n", sep="")
  writeToNetwork2 <- ""
  # TODO: This, but efficiently
  for(ID in IDs) {
    idTermSplits <- str_split(ID, " \\| ")[[1]]
    tmpHashRef	 <- classID2annotationTerm2termUniqueID[[className2classID[[idTermSplits[1]]]]]
    
    writeToNetwork2 <- paste0(writeToNetwork2, className2classID[[idTermSplits[1]]], "\t", tmpHashRef[[idTermSplits[2]]], "\t", idTermSplits[2])
    for (header in fileHeaderNames) {
      IDToUse <- ID
      if (is.na(unname(pvalueMatrix[[IDToUse]][header])) == FALSE) {
        writeToNetwork2 <- paste0(writeToNetwork2, "\t", pvalueMatrix[[IDToUse]][header])
      } else {
        writeToNetwork2 <- paste0(writeToNetwork2, "\t")
      }
    }
    writeToNetwork2 <- paste0(writeToNetwork2, "\n")
  }
  # remove trailing newline
  writeToNetwork2 <- substr(writeToNetwork2, 1, nchar(writeToNetwork2)-1)
  
  write(paste0(writeToNetwork, writeToNetwork2), NETWORK, append=TRUE)
  close(NETWORK)

  
  # Create a gct file from ValueMatrix
  INFILE <- read.delim(paste0(outputDir, summaryFileNameBase, "__ValueMatrix.txt"), sep="\t", comment.char="", quote="", stringsAsFactors = FALSE, header=TRUE, fill=TRUE)
  
  print("INFILE")
  print(INFILE)
  
  file.create(paste0(outputDir, summaryFileNameBase, "__ValueMatrix.gct"))
  OUTFILE <- file(paste0(outputDir, summaryFileNameBase, "__ValueMatrix.gct"))
  
  headerLine	<- paste0(colnames(INFILE), collapse="\t")
  headerSplit	<- str_split(headerLine, "\t")[[1]]
  sampleCnt	  <- length(headerSplit) - 3
  geneCnt  <- 0
  content	<- ""
  if(nrow(INFILE) > 0) {
    for(l in 1:nrow(INFILE)) {
      line <- paste0(INFILE[l,], collapse="\t")
      
      if (line == "") {
        # Do nothing
      } else {
        tmpSplit <- str_split(line, "\t")[[1]]
        for (i in 4:(sampleCnt+3)) {
          if (tmpSplit[i] == "NA" | is.na(tmpSplit[i]) == TRUE | length(tmpSplit[i]) < 1 | is.null(tmpSplit[i]) == TRUE) {
            tmpSplit[i] <- 0
          }
        }
        tmpSplit <- tmpSplit[-(1:1)] #Shift
        
        # TODO: Replaces long annotation name with shorthand
        #tmpSplitName <- str_split(tmpSplit[1], " | "
        #queryAnnotationClassIDString <- outpClasses[, outpClasses$annoclassname=tmpSplitName[1]]
        
        #my $queryAnnotationClassID = $dbConnection->prepare($queryAnnotationClassIDString);
        #$queryAnnotationClassID->execute();
        #my @tmpSplitClassID;
        #while(my $annotationClassID = $queryAnnotationClassID->fetchrow_hashref()) {
        #  $tmpSplitClassID[0] = $annotationClassID->{'annoclassid'};
        #}
        #my $codedAnnotationName = "CLASS ".$tmpSplitClassID[0]." | ".$tmpSplitName[1];
        #$tmpSplit[0] = $codedAnnotationName;
        
        print(">>> tmpSplit")
        print(tmpSplit)
        
        content <- paste0(content, paste0(tmpSplit, collapse="\t"), "\n")
        geneCnt <- geneCnt + 1
      }
    }	
  }
  
  write(paste0("#1.2\n", geneCnt, "\t", sampleCnt, "\n", paste0(colnames(INFILE)[-(1:1)],collapse="\t"), "\n", content), OUTFILE, append=TRUE)
  close(OUTFILE)
  
}	

# Load GCT file as profile
load_gct_file_as_profile <- function(infile, geneID2ExpProfileRef) {
  INFILE <- read.delim(infile, sep="\t", comment.char="", quote="", stringsAsFactors = FALSE, header=TRUE, fill=TRUE)
  header <- INFILE[1,]
  if (grepl("^#1", header) == FALSE) {
      # this is not a gct file
      return(0)
  }
  header <- INFILE[3,]

  headerSplit <- str_split(header, "\t")[[1]]
  maxValue	<- 0
  lineCount	<- 0
  for(line in 4:nrow(INFILE)) {
    tmpSplit <- str_split(line, "\t")[[1]]
    if (grepl("_at", tmpSplit[1]) == TRUE) {
      # TODO: This ^^
    }
    # TODO: reference
    geneID2ExpProfileRef[tmpSplit[1]] <- paste0(tmpSplit, collapse="\t")
    if (lineCount < 10) {
      for (i in 3:length(tmpSplit)) {
        if(tmpSplit[i] > maxValue) {
          maxValue <- tmpSplit[i]
        }
      }
    }
    lineCount <- lineCount + 1
  }	
  
  # check if the gct file is not log-transformed
  if(maxValue > 20) {
    for (geneID in names(geneID2ExpProfileRef)) {
      tmpSplit <- str_split(geneID2ExpProfileRef[geneID], "/\t/")[[1]]
      for (i in 3:length(tmpSplit)) {
        tmpSplit[i] <- log(tmpSplit[i])/log(2)
      }
      geneID2ExpProfileRef[geneID] <- paste0(tmpSplit, collapse="\t")
    }
  }
  return(list(1, header))
}

# Sort by file numbers (this is for the situation where the set names are like <Set 1, Set 2, Set 3, etc.>)
sort_by_file_number <- function(originalArray) {
  originalCount   <- length(originalArray)
  sortedArray 	  <- list()
  number2original <- list()
  
  if (is.null(originalArray[1]) == FALSE & grepl("\\w\\d+$", originalArray[1]) == TRUE) {

    # Number
    number2originalNames <- lapply(1:length(originalArray), function(name){
      if (grepl("\\w(\\d+)$", originalArray[name]) == TRUE) {
        return(name)
      }
    })
    number2originalNames <- unlist(number2originalNames, recursive=FALSE)
    # TODO: fix naming for this list ^^
    
    # Original name
    number2original <- lapply(originalArray, function(name){
      if (grepl("\\w(\\d+)$", name) == TRUE) {
        return(name)
      }
    })
    number2original <- unlist(number2original, recursive=FALSE)
    
    sortedNumbers <- number2original[order(unlist(number2original), decreasing = FALSE)]

    if (length(sortedNumbers) != length(originalArray)) {
        return (originalArray)
    } else {
      sortedArray <- lapply(sortedNumbers, function(num){
        return(num)
      })
      sortedArray<- unlist(sortedArray, recursive=FALSE)
      return(sortedArray)
    }
  }
  return(originalArray)
}




###### Perform enrichment analysis ######
perform_CASRN_enrichment_analysis <- function(CASRNRef, outputBaseDir, outfileBase, mappedCASRNsFromProcess, funCat2Selected, CASRN2funCatTerm, funCatTerm2CASRN, funCat2CASRNCount, funCat2termCount, funCatTerm2CASRNCount, pvalueThresholdToDisplay, similarityThreshold=0.50, initialGroupMembership, multipleLinkageThreshold, EASEThreshold){

  # Define output file names
  outfileChart    <- paste0(outputBaseDir, outfileBase, "__Chart.txt")
  outfileSimple		<- paste0(outputBaseDir, outfileBase, "__ChartSimple.txt")
  outfileMatrix		<- paste0(outputBaseDir, outfileBase, "__Matrix.txt")
  
  # Create directories if they don't exist
  dir.create(paste0(APP_DIR, outputBaseDir))
  
  # Open file connections to output files and initialize headers
  OUTFILE         <- file(outfileChart)
  SIMPLE          <- file(outfileSimple)
  MATRIX          <- file(outfileMatrix)
  writeLines("Category	Term	Count	%	PValue	CASRNs	List Total	Pop Hits	Pop Total	Fold Enrichment	Bonferroni	Benjamini	FDR", OUTFILE)
  writeLines("Category	Term	Count	%	PValue	Fold Enrichment	Benjamini", SIMPLE)
  
  # Calculate EASE score
  inputCASRNs   				<- CASRNRef
  inputCASRNsCount      <- length(inputCASRNs)
  term2Contents			    <- list()
  term2Pvalue				    <- list()
  sigTerm2CASRNMatrix   <- list()
  mappedCASRNs			    <- mappedCASRNsFromProcess # Among the CASRNS, use only those included in the full Tox21 list
  datArray	      <- list()
  annoArray	      <- list()
  hmt <- 0


  funCat2SelectedProcessed <- lapply(names(funCat2Selected), function(funCat){
      # Calculate the CASRN counts for the given categories
      if (is.null(funCat2Selected[[funCat]]) == FALSE & funCat2Selected[[funCat]] != ""){
          # Variables
          
          localTerms  <- lapply(names(mappedCASRNs), function(CASRN){
            if (is.null(CASRN2funCatTerm[[CASRN]][[funCat]]) == FALSE){
              return(names(CASRN2funCatTerm[[CASRN]][[funCat]]))
            }
          })
          localTerms <- localTerms[!sapply(localTerms, is.null)]
          totalCount <- length(localTerms)
          
          print("localTerms")
          print(localTerms)
          print(totalCount)
          
          targetTotalTermCount          <- length(localTerms)
          targetTotalCASRNInFunCatCount <- totalCount
      
          
          funCatTerms         <- names(funCatTerm2CASRN[[funCat]]) 
          localTerm2content	  <- list()
          localTerm2pvalue	  <- list()
          localPvalue2term	  <- list()
              
          tmp_datArray <- NULL
          tmp_datArrayInner <- list()
          tmp_annoArray <- NULL
          tmp_annoArrayInner <- list()
          
          tmp_datArray <- lapply(funCatTerms, function(term){
            
            if (is.null(funCatTerm2CASRN[[funCat]][term]) == FALSE & funCatTerm2CASRN[[funCat]][term] != ""){	
              # This is a valid term, check if the CASRN count is more than 1
              targetCASRNs <- lapply(names(mappedCASRNs), function(CASRN) {
                #TODO: I think problem here?
                if (is.null(funCatTerm2CASRN[[funCat]][[term]][[CASRN]]) == FALSE){
                  return(CASRN)
                }
              })
              targetCASRNs <- targetCASRNs[!sapply(targetCASRNs, is.null)]
              CASRNCount   <- length(targetCASRNs)
              
              sigTermResults <- lapply(names(mappedCASRNs), function(CASRN) {
                #TODO: I think problem here?
                if (is.null(funCatTerm2CASRN[[funCat]][[term]][[CASRN]]) == FALSE){
                  # Populate sigTerm2CASRNMatrix
                  if(length(sigTerm2CASRNMatrix[[paste0(funCat, "|", term)]]) < 1) {
                    sigTerm2CASRNMatrix[[paste0(funCat, "|", term)]] <<- list()
                  }
                  sigTerm2CASRNMatrix[[paste0(funCat, "|", term)]][CASRN] <<- 1
                }
              })
              
              #tmp_sigTerm2CASRNMatrix <- lapply(names(mappedCASRNs), function(CASRN) {
              ##  #TODO: I think problem here?
              #  if (is.null(funCatTerm2CASRN[[funCat]][[term]][[CASRN]]) == FALSE){
              #    CASRNlistToReturn <- list(1)
              #    names(CASRNlistToReturn) <- CASRN
              #    return(CASRNlistToReturn)
              #  }
              #})
              #tmp_sigTerm2CASRNMatrix <- tmp_sigTerm2CASRNMatrix[!sapply(tmp_sigTerm2CASRNMatrix, is.null)]
              
              #sigTerm2CASRNMatrixNames <- lapply(names(mappedCASRNs), function(CASRN) {
              ##  #TODO: I think problem here?
              #  if (is.null(funCatTerm2CASRN[[funCat]][[term]][[CASRN]]) == FALSE){
              #    return(paste0(funCat, "|", term))
              #  }
              #})
              #sigTerm2CASRNMatrixNames <- sigTerm2CASRNMatrixNames[!sapply(sigTerm2CASRNMatrixNames, is.null)]
              #names(tmp_sigTerm2CASRNMatrix) <- sigTerm2CASRNMatrixNames
              
              #sigTerm2CASRNMatrixNames <<- append(sigTerm2CASRNMatrixNames, tmp_sigTerm2CASRNMatrix)
              
              targetCASRNsRef   <- targetCASRNs
              targetCASRNCount  <- CASRNCount

              hmt <<- hmt + 1
              
              # Calculate the EASE score
              if (targetCASRNCount > 1){
                  np1 <- targetTotalCASRNInFunCatCount - 1
                  n11	<- targetCASRNCount - 1
                  npp <- funCat2CASRNCount[[funCat]]  
                  n1p <- funCatTerm2CASRNCount[[funCat]][[term]]
                    
                  # skip any under-represented terms
                  foldenrichment <- (targetCASRNCount/length(targetTotalCASRNInFunCatCount))/(n1p/npp)
                    
                  pvalue <- 1
                    
                  datArrayLine  <- c(n11, (n1p-n11), (np1-n11), (npp-n1p-np1+n11))
                  tmp_datArrayInner  <- append(tmp_datArrayInner, datArrayLine)
              }
            }
            return(tmp_datArrayInner)
          })
          
          datArray <<- append(datArray, tmp_datArray)

          tmp_annoArray <- lapply(funCatTerms, function(term){
            if (is.null(funCatTerm2CASRN[[funCat]][term]) == FALSE & funCatTerm2CASRN[[funCat]][term] != ""){	
              
              # This is a valid term, check if the CASRN count is more than 1
              # TODO: calculate CASRNCount by getting length instead of this
              CASRNCount   <- 0
              targetCASRNs <- list()
              mappedCASRNsProcess <- lapply(names(mappedCASRNs), function(CASRN) {
                #TODO: I think problem here?
                if (is.null(funCatTerm2CASRN[[funCat]][[term]][[CASRN]]) == FALSE){
                  CASRNCount <<- CASRNCount + 1
                  targetCASRNs <<- append(targetCASRNs, CASRN)
                }
              })
              
              targetCASRNsRef   <- targetCASRNs
              targetCASRNCount  <- CASRNCount
              
              # Calculate the EASE score
              if (targetCASRNCount > 1){
                np1 <- targetTotalCASRNInFunCatCount - 1
                n11	<- targetCASRNCount - 1
                npp <- funCat2CASRNCount[[funCat]]  
                n1p <- funCatTerm2CASRNCount[[funCat]][[term]]

                # skip any under-represented terms
                foldenrichment <- (targetCASRNCount/length(targetTotalCASRNInFunCatCount))/(n1p/npp)
                pvalue <- 1
                
                # truncate pvalue digits after decimal to 15
                annoArrayLine	<- c(funCat, term, targetCASRNCount, round((targetCASRNCount/inputCASRNsCount*100), digits=15),
                                   1, paste(unlist(unname(sapply(targetCASRNsRef, paste, collapse=", "))), collapse=', '), targetTotalCASRNInFunCatCount, 
                                   n1p, npp, (targetCASRNCount/targetTotalCASRNInFunCatCount)/(n1p/npp), 
                                   (1-(1-pvalue)^targetTotalTermCount))
                tmp_annoArrayInner <- append(tmp_annoArrayInner, annoArrayLine)
              }
            }
            return(tmp_annoArrayInner)
          })
          annoArray <<- append(annoArray, tmp_annoArray)
          
      }
  })

  # Save the data file -> create "RINPUT" but as a data frame here
  RINPUT_index1 <- list()
  RINPUT_index2 <- list()
  RINPUT_index3 <- list()
  RINPUT_index4 <- list()
  RINPUT <- lapply(datArray, function(arrayRef) {
    if(length(arrayRef) > 0){
      RINPUT_index1 <<- append(RINPUT_index1, arrayRef[[1]])
      RINPUT_index2 <<- append(RINPUT_index2, arrayRef[[2]])
      RINPUT_index3 <<- append(RINPUT_index3, arrayRef[[3]])
      RINPUT_index4 <<- append(RINPUT_index4, arrayRef[[4]])
    }
  })
  
  RINPUT_df <- data.frame(X1=unlist(RINPUT_index1), X2=unlist(RINPUT_index2), X3=unlist(RINPUT_index3), X4=unlist(RINPUT_index4))
  
  print("RINPUT_df")
  print(RINPUT_df)
  
  print("sigTerm2CASRNMatrix")
  print(sigTerm2CASRNMatrix)

  
  
  if(nrow(RINPUT_df) < 2){
    
    # Print out the matrix file
    sortedHeaderTerms <- sigTerm2CASRNMatrix[order(unlist(sigTerm2CASRNMatrix), decreasing = FALSE)]
    # Clean out NA values (TODO: Probably a better way to do this?)
    sortedHeaderTerms <- sortedHeaderTerms[lengths(sortedHeaderTerms)!=0]
    
    matrixHeader <- paste(names(sortedHeaderTerms), collapse='\t')
    matrixOutput <- paste0("CASRN\t", matrixHeader, "\n\n")
    
    matrixPrintToFile <- lapply(names(mappedCASRNs), function(tmpCasrn){	
      matrixOutput <<- paste(matrixOutput, tmpCasrn, sep="")
      tmpMatrixHeaderProcess <- lapply(names(sortedHeaderTerms), function(tmpMatrixHeader){
        if (is.null(sigTerm2CASRNMatrix[[tmpMatrixHeader]][[tmpCasrn]]) == FALSE & tmpMatrixHeader != "NA"){
          matrixOutput <<- paste(matrixOutput,"\t1",sep="")
        }
        else{
          matrixOutput <<- paste(matrixOutput,"\t0",sep="")
        }
      })
      matrixOutput <<- paste(matrixOutput,"\n",sep="")
    })	
    
    write(matrixOutput, outfileMatrix, append=TRUE)
    
    close(OUTFILE)
    close(SIMPLE)
    close(MATRIX)
    
    # Open and create a blank cluster file
    outfileCluster		<- paste0(outputBaseDir, outfileBase, "__Cluster.txt")
    file.create(outfileCluster)
    
    return(FALSE)
  }
  # create a R command file -> now just work on data frame
  p.value <- apply(RINPUT_df, 1, function(x) {
    fisher.test(matrix(unlist(x), nrow=2))$p.value
  })
  ROUTPUT <- data.frame(p.value, fdr=p.adjust(p.value))
  
  # Load the output file 
  ROutputData <- apply(ROUTPUT, 1, function(line){
    if (length(line) > 0) {
      ROutputLineSplit <- list(line[['p.value']], line[['fdr']])
      return(ROutputLineSplit)
    }
  })
  
  # Integrate ROutput into the main hashes/arrays
  # TODO: Error checking/handling for the case the number of lines are different between 
  #             @annoArray and @ROutputData
  
  # Remove empty elements in annoArray
  annoArray <- annoArray[lengths(annoArray) > 0L]
  
  #TODO: Make this better
  annoArrayIndex <- 1
  pvalueUpdateProcess <- lapply(annoArray, function(i){
    tmp <- c("p.value", "fdr")
     # update the p-value
     annoArray[[annoArrayIndex]][5] <<- ROutputData[[annoArrayIndex]][[1]]
     
     # add FDR to the array 
     annoArray[[annoArrayIndex]][12] <<- ROutputData[[annoArrayIndex]][[2]]
     annoArray[[annoArrayIndex]][13] <<- ROutputData[[annoArrayIndex]][[1]]
     
     # finalize the hashes
     term2Contents[[paste0(annoArray[[annoArrayIndex]][1], "|", annoArray[[annoArrayIndex]][2])]]	<<- paste0(annoArray[[annoArrayIndex]])
     term2Pvalue[[paste0(annoArray[[annoArrayIndex]][1], "|", annoArray[[annoArrayIndex]][2])]]	<<- ROutputData[[annoArrayIndex]][1]
     
     annoArrayIndex <<- annoArrayIndex + 1
  })
  
  # Sort by the p-values across multiple funCat
  sortedFunCatTerms <- term2Pvalue[order(unlist(term2Pvalue), decreasing = FALSE)]
  sortedFunCatTermsCount <- length(sortedFunCatTerms)
  simpleFunCatTermCount	<- list()
  funCatSimpleContent <- list()
  
  #TODO: fill this list in a better way vvv
  
  sortFunCatProcess <- lapply(names(sortedFunCatTerms), function(funCatTerm){ 
  	if (term2Pvalue[[funCatTerm]] <= pvalueThresholdToDisplay){	
  	  # write to OUTFILE
  	  outputLine <- paste(term2Contents[[funCatTerm]], collapse='\t')
  	  write(outputLine, outfileChart, append=TRUE)

      toSimple <- 0
      tmpSplit <- term2Contents[[funCatTerm]]
      
      if (tmpSplit[[10]] > 1){
        localFunCat <- get_funCat_from_funCatTerm(funCatTerm)
        if (is.null(simpleFunCatTermCount[[localFunCat]]) == TRUE){
          simpleFunCatTermCount[[localFunCat]] <<- 1
          toSimple <- 1
        } 
        else if(simpleFunCatTermCount[[localFunCat]] < 11){
          simpleFunCatTermCount[[localFunCat]] <<- simpleFunCatTermCount[[localFunCat]] + 1
          toSimple <- 1
        }
          
        if(toSimple > 0){
          funCatSimpleContent[[localFunCat]] <<- append(funCatSimpleContent[[localFunCat]], paste0(tmpSplit[[1]], "\t", tmpSplit[[2]], "\t", tmpSplit[[3]], "\t", tmpSplit[[4]], "\t", tmpSplit[[5]], "\t", tmpSplit[[10]], "\t", tmpSplit[[12]], "\n"))
        }
      }
  	}
  })
  
  
  writeToSimple <- lapply(funCatSimpleContent, function(funCat){
    write(funCat, outfileSimple, append=TRUE)
  })
  
  close(OUTFILE)
  close(SIMPLE)
  
  # Print out the matrix file
  sortedHeaderTerms <- sigTerm2CASRNMatrix[order(unlist(sigTerm2CASRNMatrix), decreasing = FALSE)]
  # Clean out NA values (TODO: Probably a better way to do this?)
  sortedHeaderTerms <- sortedHeaderTerms[lengths(sortedHeaderTerms)!=0]
  
  matrixHeader <- paste(names(sortedHeaderTerms), collapse='\t')
  matrixOutput <- paste0("CASRN\t", matrixHeader, "\n\n")
  
  matrixPrintToFile <- lapply(names(mappedCASRNs), function(tmpCasrn){	
    matrixOutput <<- paste(matrixOutput, tmpCasrn, sep="")
    tmpMatrixHeaderProcess <- lapply(names(sortedHeaderTerms), function(tmpMatrixHeader){
      if (is.null(sigTerm2CASRNMatrix[[tmpMatrixHeader]][[tmpCasrn]]) == FALSE & tmpMatrixHeader != "NA"){
        matrixOutput <<- paste(matrixOutput,"\t1",sep="")
      }
      else{
        matrixOutput <<- paste(matrixOutput,"\t0",sep="")
      }
    })
    matrixOutput <<- paste(matrixOutput,"\n",sep="")
  })	
  
  write(matrixOutput, outfileMatrix, append=TRUE)
  close(MATRIX)
  
  # ----------------------------------------------------------------------
  #	Perform functional term clustering
  # ----------------------------------------------------------------------

  # Calculate enrichment score
  df<-read.delim(outfileChart, sep="\t", comment.char="", quote="", stringsAsFactors = FALSE)
  res<-kappa_cluster(x=df, outputBaseDir=outputBaseDir, outfileBase=outfileBase, sortedFunCatTerms=sortedFunCatTerms, sigTerm2CASRNMatrix=sigTerm2CASRNMatrix, sortedFunCatTermsCount=sortedFunCatTermsCount, inputCASRNsCount=inputCASRNsCount, similarityThreshold=similarityThreshold, initialGroupMembership=initialGroupMembership, multipleLinkageThreshold=multipleLinkageThreshold, EASEThreshold=EASEThreshold, term2Pvalue=term2Pvalue, term2Contents=term2Contents)
}

calculate_funcat_mapped_total_CASRN_count <- function(mappedCASRNsRef, funCat, CASRN2funCatTerm){
  totalCount    <- 0
  terms <- list()

  CASRNResults  <- lapply(names(mappedCASRNsRef), function(CASRN){
    if (is.null(CASRN2funCatTerm[[CASRN]][funCat]) == FALSE){
      totalCount <<- totalCount + 1
      terms <- names(CASRN2funCatTerm[[CASRN]][[funCat]])
    }
    return(terms)
  })
  localTerms  <- CASRNResults

  if(length(c(unlist(localTerms))) > 0){
    return(data.frame(localTerms = localTerms, totalCount = totalCount))  
  } else {
    return(data.frame(localTerms = c("NA"), totalCount = totalCount))  
  }
  
}

#TODO: FIX THIS - populate sigTerm2CASRNMatrixRef
calculate_funcat_mapped_CASRN_count <- function(mappedCASRNsRef, funCat, term, sigTerm2CASRNMatrixRef, CASRN2funCatTerm, funCatTerm2CASRN){
  CASRNCount   <- 0
  targetCASRNs <- list()
  sigTerm2CASRNMatrixTmp <- list()
  
  mappedCASRNsProcess <- lapply(names(mappedCASRNsRef), function(CASRN) {
    if (is.null(funCatTerm2CASRN[[funCat]][[term]][[CASRN]]) == FALSE){
        CASRNCount <<- CASRNCount + 1
        targetCASRNs <<- append(targetCASRNs, CASRN)
        sigTerm2CASRNMatrixTmp[[paste0(funCat, "|", term)]] <<- list()
        sigTerm2CASRNMatrixTmp[[paste0(funCat, "|", term)]][CASRN] <<- 1
    }
  })
  
  if(length(targetCASRNs) > 0){
    return (data.frame(targetCASRNs = c(unlist(targetCASRNs)), sigTerm2CASRNMatrixTmp = c(unlist(sigTerm2CASRNMatrixTmp)), CASRNCount = CASRNCount))  
  } 
  else{
    return (data.frame(targetCASRNs = c("NA"), sigTerm2CASRNMatrixTmp = c("NA"), CASRNCount = CASRNCount))
  }
  
}

check_CASRN <- function(CASRN, CASRN2DSSTox){
  if(is.null(CASRN2DSSTox[[CASRN]])==FALSE){
    return(TRUE)
  }
}

get_funCat_from_funCatTerm <- function(funCatTerm){
  tmpSplit <- str_split(funCatTerm, '\\|')
  return(tmpSplit[[1]][1])
}

kappa_cluster <- function(x, deg=NULL, useTerm=FALSE, cutoff=0.5, overlap=0.5, minSize=5, escore=3, outputBaseDir, outfileBase, sortedFunCatTerms, sigTerm2CASRNMatrix, sortedFunCatTermsCount, inputCASRNsCount=0, similarityThreshold=0.50, initialGroupMembership, multipleLinkageThreshold=0.5, EASEThreshold=1.0, term2Pvalue, term2Contents) {
  # ----------------------------------------------------------------------
  #	Perform functional term clustering
  # ----------------------------------------------------------------------
  # 	Step#1: Calculate kappa score
  # ----------------------------------------------------------------------
  
  mappedCASRNCheck	<- list()
  mappedCASRNIDs		<- list()
  posTermCASRNCount	<- list()
  
  posTermCASRNCount <- lapply(names(sortedFunCatTerms), function(funCatTerm){
    localCASRNIDs <- sigTerm2CASRNMatrix[[funCatTerm[[1]]]]
    return(length(localCASRNIDs))
  })
  
  # Set names
  names(posTermCASRNCount) <- lapply(names(sortedFunCatTerms), function(funCatTerm){
    return(funCatTerm[[1]])
  })
  
  tmp_mappedCASRNCheck <- lapply(names(sortedFunCatTerms), function(funCatTerm){
    localCASRNIDs <- names(sigTerm2CASRNMatrix[funCatTerm[[1]]])
    tmp_inner_mappedCASRNCheck <- lapply(localCASRNIDs, function(CASRNID){
      return(1)
    })
    tmp_inner_mappedCASRNCheck <- unlist(tmp_inner_mappedCASRNCheck, recursive = FALSE)
    tmp_names_mappedCASRNCheck <- lapply(localCASRNIDs, function(CASRNID){
      return(CASRNID)
    })
    tmp_names_mappedCASRNCheck <- unlist(tmp_names_mappedCASRNCheck, recursive = FALSE)
    
    return(list(CASRNID=tmp_names_mappedCASRNCheck, content=tmp_inner_mappedCASRNCheck))
  })
  
  mappedCASRNCheckNames <- lapply(1:length(tmp_mappedCASRNCheck), function(i){
    return(tmp_mappedCASRNCheck[[i]]["CASRNID"])
  })
  mappedCASRNCheckContent <- lapply(1:length(tmp_mappedCASRNCheck), function(i){
    return(tmp_mappedCASRNCheck[[i]]["content"])
  })
  
  # TODO: better way to do this??
  mappedCASRNCheck        <- unlist(unname(unlist(mappedCASRNCheckContent, recursive=FALSE)))
  names(mappedCASRNCheck) <- unlist(unname(unlist(mappedCASRNCheckNames, recursive=FALSE)))
  
  mappedCASRNIDs				  <- names(mappedCASRNCheck)
  totalMappedCASRNIDCount	<- length(mappedCASRNIDs)
  
  # Calculate kappa score for each term pair
  termpair2kappa						        <- list()
  termpair2kappaOverThresholdCount	<- list()
  termpair2kappaOverThreshold			  <- list()
  
  # TODO: Clean this up earlier but this should clean things up from this point and down
  sortedFunCatTerms <- names(sortedFunCatTerms)
  print("sortedFunCatTerms")
  print(sortedFunCatTerms)
  
  for (i in (1:(sortedFunCatTermsCount-1))) {
    for (j in ((i+1):(sortedFunCatTermsCount))) {
      #calculate_kappa_statistics 
      term1term2		<- 0
      term1only			<- 0
      term2only			<- 0
      term1term2Non	<- 0

      posTerm1Total	<- posTermCASRNCount[[sortedFunCatTerms[i]]]
      posTerm2Total	<- posTermCASRNCount[[sortedFunCatTerms[j]]]
      negTerm1Total	<- inputCASRNsCount - posTerm1Total			# note that the total is inputCASRNsCount not the mapped total
      negTerm2Total	<- inputCASRNsCount - posTerm2Total			# note that the total is inputCASRNsCount not the mapped total
    
      # Get number of chemicals that are shared or not for term1 and term2
      sharedTerms <- intersect(names(sigTerm2CASRNMatrix[[sortedFunCatTerms[i]]]), names(sigTerm2CASRNMatrix[[sortedFunCatTerms[j]]]))
      
      term1term2		<- length(sharedTerms)
      term1only			<- length(names(sigTerm2CASRNMatrix[[sortedFunCatTerms[i]]])) - length(sharedTerms)
      term2only			<- length(names(sigTerm2CASRNMatrix[[sortedFunCatTerms[j]]])) - length(sharedTerms)
      term1term2Non	<- inputCASRNsCount - term1term2 - term1only - term2only

      # Calculate the kappa score 
      # http://david.abcc.ncifcrf.gov/content.jsp?file=linear_search.html
      Oab					    <- (term1term2 + term1term2Non)/inputCASRNsCount
      Aab					    <- ((posTerm1Total * posTerm2Total) + (negTerm1Total * negTerm2Total))/(inputCASRNsCount * inputCASRNsCount)
      
      if (Aab == 1) {
        # Do nothing
      } else {
        Kappa	<- as.double(sprintf("%.2f", (Oab - Aab)/(1 - Aab)))
        
        termpair2kappa[[sortedFunCatTerms[i]]][sortedFunCatTerms[j]] <- Kappa
        termpair2kappa[[sortedFunCatTerms[j]]][sortedFunCatTerms[i]] <- Kappa

        if (Kappa > similarityThreshold) {
          if(is.null(termpair2kappaOverThresholdCount[[sortedFunCatTerms[i]]]) == TRUE){
            termpair2kappaOverThresholdCount[[sortedFunCatTerms[i]]] <- 1
          } else {
            termpair2kappaOverThresholdCount[[sortedFunCatTerms[i]]] <- termpair2kappaOverThresholdCount[[sortedFunCatTerms[i]]] + 1
          }
          if(is.null(termpair2kappaOverThresholdCount[[sortedFunCatTerms[j]]]) == TRUE){
            termpair2kappaOverThresholdCount[[sortedFunCatTerms[j]]] <- 1
          } else {
            termpair2kappaOverThresholdCount[[sortedFunCatTerms[j]]] <- termpair2kappaOverThresholdCount[[sortedFunCatTerms[j]]] + 1
          }
          termpair2kappaOverThreshold[[sortedFunCatTerms[i]]][[sortedFunCatTerms[j]]] <- 1
          termpair2kappaOverThreshold[[sortedFunCatTerms[j]]][[sortedFunCatTerms[i]]] <- 1
        }
      }
    }
  }
  
  # TODO: speed this up somehow... ^^^
  
  # ----------------------------------------------------------------------
  # 	Step#2: Create qualified initial seeding groups
  # ----------------------------------------------------------------------
  #	Each term could form a initial seeding group (initial seeds) 
  #   as long as it has close relationships (kappa > 0.35 or any designated number) 
  #   with more than > 2 or any designated number of other members. 
  
  print("termpair2kappaOverThreshold")
  print(termpair2kappaOverThreshold)
  
  if(length(termpair2kappaOverThreshold) > 0){
    termpair2kappaOverThreshold <- termpair2kappaOverThreshold[order(unlist(termpair2kappaOverThreshold), decreasing = TRUE)]
  }

  qualifiedSeeds <- lapply((1:sortedFunCatTermsCount), function(i){
    # Seed condition#1: initial group membership
    if (is.null(termpair2kappaOverThresholdCount[[sortedFunCatTerms[i]]]) == FALSE) {
      if(termpair2kappaOverThresholdCount[[sortedFunCatTerms[i]]] >= (initialGroupMembership-1)) {
        # Seed condition#2: majority of the members 
        results_calculate_percentage_of_membership_over_threshold <- calculate_percentage_of_membership_over_threshold (termpair2kappaOverThreshold, sortedFunCatTerms[i])
        over_percentage <- results_calculate_percentage_of_membership_over_threshold["overPercentage"]
        term2sRef <- results_calculate_percentage_of_membership_over_threshold["term2s"]
        
        if (as.double(unlist(over_percentage)) > as.double(multipleLinkageThreshold)) {
          # this seed group is qualified
          return(unlist(unname(term2sRef)))
        }
      }
    }
    return(NULL)
  })
  
  # remove empty nested lists
  qualifiedSeeds <- lapply(qualifiedSeeds, function(innerList) innerList[sapply(innerList, length) > 0])
  qualifiedSeeds <- qualifiedSeeds[!sapply(qualifiedSeeds, is.null)]
  
  # ----------------------------------------------------------------------
  # 	Step#3: Iteratively merge qualifying seeds
  # ----------------------------------------------------------------------
  remainingSeeds	<- qualifiedSeeds
  finalGroups <- vector("list", length(remainingSeeds))
  
  print("remainingSeeds length")
  print(length(remainingSeeds))
  
  finalGroupsIndex <- 1

  while(is.null(unlist(remainingSeeds[1])) == FALSE){
    currentSeedRef	 <- remainingSeeds[[1]]
    remainingSeeds	 <- remainingSeeds[-1]
    newSeeds <- list()

    while(TRUE) {
      # TODO: I don't think 'get_the_best_seed' is working right
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
    #print(paste0("currentSeedRef: ", currentSeedRef, " | index: ", finalGroupsIndex))
    
    finalGroupsIndex <- finalGroupsIndex+ 1

  }
  
  # Remove null elements
  finalGroups <- lapply(finalGroups, function(innerList){
    if(length(innerList) > 0) {
      return(innerList)
    }
  })
  finalGroups <- finalGroups[!sapply(finalGroups, is.null)]
  
  # ----------------------------------------------------------------------
  # 	Step#4: Calculate enrichment score and print out the results
  # ----------------------------------------------------------------------
  
  outfileCluster	<- paste0(outputBaseDir, outfileBase, "__Cluster.txt")
  CLUSTER         <- file(outfileCluster)
  
  clusterHeader <- "Category	Term	Count	%	PValue	CASRNs	List Total	Pop Hits	Pop Total	Fold Enrichment	Bonferroni	Benjamini	FDR\n"
  EASEScore	    <- list()
  
  if(length(finalGroups) > 0){
    EASEScore <- lapply(1:length(finalGroups), function(i){
      return(calculate_Enrichment_Score (finalGroups[i], term2Pvalue))
    })
    EASEScore <- unlist(EASEScore, recursive=FALSE)
  }
  
  # Sort
  clusterNumber	<- 0
  sortedIndex <- finalGroups
  names(sortedIndex) <- EASEScore
  sortedIndex <- sortedIndex[order(names(sortedIndex), decreasing=TRUE)]
  
  writeToCluster <- ""
  if(length(sortedIndex) > 0){
    for(myIndex in 1:length(sortedIndex)) {
      if(length(finalGroups[[myIndex]] > 0)){
        
        writeToCluster <- paste0(writeToCluster, "Annotation Cluster ", (clusterNumber+1), "\tEnrichment Score: ", names(sortedIndex)[myIndex], "\n", sep="")
        clusterNumber <- clusterNumber + 1
        
        writeToCluster <- paste0(writeToCluster, clusterHeader)
       
        # sort terms again by p-value
        finalGroups2Pvalue <- lapply(sortedIndex[[myIndex]], function(term){
          return(term2Pvalue[[term]])
        })
        finalGroups2Pvalue <- unlist(finalGroups2Pvalue, recursive=FALSE)
        names(finalGroups2Pvalue) <- sortedIndex[[myIndex]]
        sortedFunCatTerms <- finalGroups2Pvalue[order(unlist(finalGroups2Pvalue), decreasing = FALSE)]
        
        writeTermsToCluster <- lapply(names(sortedFunCatTerms), function(myTerm) return(paste0(term2Contents[[myTerm]], collapse="\t")))
        writeToCluster <- paste0(writeToCluster, paste0(writeTermsToCluster, collapse="\n"), sep="")
        writeToCluster <- paste0(writeToCluster, "\n\n")
  
      }
    }
  }
  
  write(writeToCluster, CLUSTER, append=TRUE)
  close(CLUSTER)
  return(1)
}

calculate_Enrichment_Score <- function(tmp_groupRef, term2PvalueRef) {
  EASESum <- 0
  groupRef <- tmp_groupRef[[1]]
  for (termTmp in groupRef) {
    term <- termTmp[[1]]
    if (term2PvalueRef[[term]][[1]] == 0) {
      EASESum <- EASESum +  16
    } else {
      EASESum <- EASESum + -log(term2PvalueRef[[term]][[1]])/log(10)
    }
  }
  
  enrichmentScore <- EASESum / length(groupRef)
  return(enrichmentScore)
}

calculate_percentage_of_membership_over_threshold <- function(termpair2kappaOverThresholdRef, currentTerm) {
  term2s <- names(termpair2kappaOverThresholdRef[[currentTerm]])
  term2s <- c(currentTerm, term2s)
  
  # calculate 
  totalPairs <- 0
  passedPair <- 0
  
  #TODO: use intersect here - somehow?? find better way
  for (i in 1:(length(term2s)-1)) {
    for (j in ((i+1):length(term2s))) {
      if (is.na(termpair2kappaOverThresholdRef[[term2s[i]]][term2s[j]]) == FALSE) {
        passedPair <- passedPair + 1
      }
    }
  }
  
  #use n choose k to calculate total number of unique pairs
  totalPairs <- choose(length(term2s), 2)

  return(list(overPercentage=(passedPair/totalPairs), term2s=term2s))
}

get_the_best_seed <- function(currentSeedRef, remainingSeedsRef, newSeedRef, multipleLinkageThreshold, index) {
  bestOverlapping		    <- 0
  bestSeedIndex			    <- ""
  currentSeedTerms	    <- currentSeedRef
  currentSeedTermCount	<- length(currentSeedTerms)
  
  if(length(remainingSeedsRef) > 1){
    for (i in 1:(length(remainingSeedsRef))) {
      # calculate the overlapping
      secondSeedTerms	<- remainingSeedsRef[[i]]
      commonCount		  <- length(intersect(secondSeedTerms, currentSeedTerms))
      totalCount		  <- length(secondSeedTerms)
      overlapping	<- 2*commonCount / (currentSeedTermCount + totalCount)

      # !CHECK! '>' or '>='
      if (overlapping > multipleLinkageThreshold) {
        if (bestOverlapping < overlapping) {
          bestOverlapping 	<- overlapping
          bestSeedIndex		  <- i
        }
      }
    } 
  }

  #print(paste0("down here best overlapping", bestOverlapping))
  if (bestOverlapping == 0) {
    # no more merging is possible
    return(list(remainingSeedsRef=remainingSeedsRef, newSeedRef=newSeedRef, finished=0))
  } else {
    #print(paste0("bestOverlapping: ", bestOverlapping))
    
    # best mergable seed found
    newSeedRef <- union(currentSeedTerms, remainingSeedsRef[[bestSeedIndex]])

    #splice
    remainingSeedsRef <- remainingSeedsRef[-bestSeedIndex]
    
    return(list(remainingSeedsRef=remainingSeedsRef, newSeedRef=newSeedRef, finished=1))
  }
}

########################################################################################################################################################################################################################################################

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
    print(paste0("New enrichment input: ", transactionId))
    
    # Open pool for PostgreSQL
    tox21db <- config::get("tox21enricher")
    poolTerms <- dbPool(
      drv = dbDriver("PostgreSQL",max.con = 100),
      dbname = tox21db$database,
      host = tox21db$host,
      user = tox21db$uid,
      password = tox21db$pwd,
      idleTimeout = 3600000
    )
    
    # Set tanimoto threshold
    print(paste0("Setting tanimoto threshold... ",tanimoto))
    queryTanimoto <- sqlInterpolate(ANSI(), paste0("set rdkit.tanimoto_threshold=", tanimoto, ";"))
    outpTanimoto <- dbGetQuery(poolTerms, queryTanimoto)
    print("set tanimoto")
    print(outpTanimoto)
  
    inputList <- list()
    currentSet <- ""
    
    inputListTry <- tryCatch({
      # should be of format [:digit:]+-[:digit:]+-[:digit:]+
      inputListTmp <- str_split(inputStr, " ")
      
      inputListTmp <- unlist(inputListTmp)
      print(paste0("Received: ", inputList))
      print("inputListTmp")
      print(inputListTmp)
      
      # Grab CASRNs if type is Substructure
      if(enrichmentType == "Substructure"){
        print("===> Substructure")
        setNameIndex <- 1
        substructureMainProcess <- lapply(inputListTmp, function(i){
          print("Checking")
          print(i)
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
            print("outpGetCasFromSubstructure")
            print(outpGetCasFromSubstructure)
            
            if(length(outpGetCasFromSubstructure) > 0){ 
              currentSet <<- paste0("Set",setNameIndex)
              substructureProcess <- lapply(1:nrow(outpGetCasFromSubstructure), function(index){
                
                print("adding casrn")
                
                print(currentSet)
                #inputList[[currentSet]] <<- casrn
                casrn <- paste0(outpGetCasFromSubstructure[index, "casrn"])
                print(casrn)
                return(casrn)
                
              })
              print("substructureProcess")
              print(substructureProcess)
              inputList[[currentSet]] <<- unlist(substructureProcess)
              setNameIndex <<- setNameIndex + 1
            }
          }
        })
      }
      
      # Grab CASRNs if type is Similarity
      else if(enrichmentType == "Similarity"){
        print("===> Similarity")
        setNameIndex <- 1
        similarityMainProcess <- lapply(inputListTmp, function(i){
          print("Checking")
          print(i)
          goodToAdd <- TRUE #Set this to false and don't add if InChI is bad
          
          # First, check if we have any InChIs, and convert to SMILES
          if (grepl("InChI=",i,fixed=TRUE)) {
            queryInchi <- sqlInterpolate(ANSI(), paste0("SELECT smiles FROM chemical_detail WHERE inchis = '", i, "';"),
                                         id = "smilesResults")
            outpInchi <- dbGetQuery(poolTerms, queryInchi)
            print("outpInchi")
            print(outpInchi)
            if(length(outpInchi) > 0){
              i <- outpInchi[[1]]
            } else {
              goodToAdd <- FALSE
            }
          }
          
          if (goodToAdd == TRUE) {
            queryGetCasFromSimilarity <- sqlInterpolate(ANSI(), paste0("SELECT * FROM get_mfp2_neighbors('", i, "');"))
            outpGetCasFromSimilarity <- dbGetQuery(poolTerms, queryGetCasFromSimilarity)
            print("outpGetCasFromSimilarity")
            print(outpGetCasFromSimilarity)
            if (length(outpGetCasFromSimilarity) > 0) {
              currentSet <<- paste0("Set",setNameIndex)
              similarityProcess <- lapply(1:nrow(outpGetCasFromSimilarity), function(index){
                
                print("adding casrn")
                
                print(currentSet)
                #inputList[[currentSet]] <<- casrn
                casrn <- paste0(outpGetCasFromSimilarity[index, "casrn"])
                print(casrn)
                return(casrn)
                
              })
              print("similarityProcess")
              print(similarityProcess)
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
          print(paste0("Checking: ", i))
          print(grepl("\\d+-\\d+-\\d+", i))
          
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
    
    print("inputList")
    print(inputList)
    
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
        print(paste0(">>> ", fetchedTerm))
        # Validate list - remove anything that's not in Tox21
        if (length(fetchedTerm) == 1) {
          writeInput <<- paste0(writeInput, j, "\t", fetchedTerm, "\n\n")  
        }
        
      })
      
      # Write to input file
      print(writeInput)
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

# TODO: UPDATE THIS!!!!!!!!!!!
#* Download enrichment results for a given uuid
#* @serializer contentType list(type="application/zip")
#* @param id The UUID of the enrichment process to download.
#* @get /download
function(id="-1", res) {
  # async
  #future_promise({
    dirToReturn <- dir(paste0(APP_DIR, "Output/",id,"/",sep=""), full.names=TRUE)
    dlFile <- zip(zipfile=paste0(APP_DIR, "Output/",id,"/tox21enricher.zip",sep=""), files=dirToReturn)
    fName <- paste0(APP_DIR, "Output/",id,"/tox21enricher.zip",sep="")
    readBin(fName, 'raw', n = file.info(fName)$size)
  #})
}
#######################################################################################


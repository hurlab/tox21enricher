#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic
shinyServer(function(input, output, session) {
    # List of observers
    setFilesObservers <- reactiveValues(observers = list())
  
    # Theme info
    theme <- reactiveValues(textcolor = "#000000")
    
    # Load theme file
    localDir <- paste0("./www/local/")
    if(file.exists(paste0(localDir, "theme-dark"))){
      theme$textcolor = "#FFFFFF"
      updateCheckboxInput(session, inputId="changeThemeToggle", value=TRUE)
    } else {
      theme$textcolor = "#000000"
      updateCheckboxInput(session, inputId="changeThemeToggle", value=FALSE)
    }
  
    # API connectivity details
    # Change host address and port in config.yml
    tox21config <- config::get("tox21enricher-client")
    API_HOST <- tox21config$host
    API_PORT <- tox21config$port

    # Display enrichment type on title
    titleStatus <- reactiveValues(option = character())
    observeEvent(input$enrich_from, {
      if(input$enrich_from == "View annotations for Tox21 chemicals") {
        # for grammatical reasons
        output$selected_enrich_from <- renderText({
          paste(input$enrich_from)
        }) 
      } else {
        output$selected_enrich_from <- renderText({
          paste("Enrich from ", input$enrich_from)
        })  
      }
    })
    
    
    # Get list of annotation classes & types from Postgres database
    # TODO: Make the API do this
    get_annotations <- function(){
        # Query API to get list of annotation classes and types
        resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="initAnnotations")
        if(resp$status_code != 200){
          return(list())
        }
        outputAnnotationClass <- unlist(lapply(content(resp), function(x){
          return(x$annoclassname)
        }))
        outputAnnotationType <- unlist(lapply(content(resp), function(x){
          return(x$annotype)
        }))
        outputAnnotationDesc <- unlist(lapply(content(resp), function(x){
          return(x$annodesc)
        }))
        outputAnnotations <- data.frame(annoclassname=outputAnnotationClass, annotype=outputAnnotationType, annodesc=outputAnnotationDesc, stringsAsFactors=FALSE)
        return(outputAnnotations)
    }
    annoClasses <- reactiveValues(classes = c())
    
    # Get total # of requests
    getEnrichmentCount <- function(){
      tryEnrichmentCount <- tryCatch(
        {
          # Query API to get list of annotation classes and types
          resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="total")
          if(resp$status_code != 200){
            return(0)
          }
          return(unname(unlist((content(resp)))))
        }, error=function(cond){
          return(0)
        }
      )
    }
    
    # Display number of total enrichments performed
    output$totalEnrichments <- renderUI({
      totalEnrichments <- HTML(paste0("<br>Total requests serviced by Tox21 Enricher this month: <b>", getEnrichmentCount(), "</b>"))
      return(totalEnrichments)
    })
    
    # Serve user manual
    observeEvent(input$manualLink, {
      # First, query API to get most recent version of manual
      resp <- NULL
      tryManual <- tryCatch(
        {
          resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="getAppVersion")    
          return(TRUE)
        }, error=function(cond){
          return(FALSE)
        }
      )
      if(tryManual == FALSE){
        shinyjs::show(id="manualError")
        output$manualError<- renderUI({
          HTML("<div class=\"text-danger\">Error: Could not retrieve manual from server.</div>")
        })
        return(FALSE)
      }
      appVersion <- content(resp)
      # Next, check if we have previously downloaded the manual (Tox21 Enricher will cache previously-downloaded manuals). If yes, do nothing. If no, download the manual from the Plumber server
      # This should always get the most recent manual revision
      if(file.exists(paste0("./www/tmp/docs/Tox21Enricher_Manual_v", appVersion, ".pdf")) == FALSE){
        # TODO: error check if download fails
        download.file(paste0("http://", API_HOST, ":", API_PORT, "/serveManual"), destfile=paste0("./www/tmp/docs/Tox21Enricher_Manual_v", appVersion, ".pdf"))
      }
      # Next, use custom JavaScript to open manual from disk in new tab
      js$browseURL(paste0("tmp/docs/Tox21Enricher_Manual_v", appVersion,".pdf"))
    })
    
    searchStatus <- reactiveValues(option = character())
    searchStatus$option <- "search"
    searchCheckboxes<- reactiveValues(checkboxes = NULL)
    # Open search enrichment menu
    observeEvent(input$searchButton, {
      loadEnrichList()
    })
    
    loadEnrichList <- function(){
      if(searchStatus$option == "search"){
        # Hide main page and show search page
        shinyjs::show(id = "searchForm")
        shinyjs::hide(id = "enrichmentForm")
        shinyjs::hide(id = "warningSearchColumn")
        shinyjs::enable(id = "searchForm")
        shinyjs::disable(id = "enrichmentForm")
        shinyjs::disable(id = "enrich_from")
        
        # Hide Select enrichment type selector and show buttons
        shinyjs::hide(id = "enrich_from")
        shinyjs::show(id = "searchButtonsMenu")
        
        updateActionButton(session, "searchButton", label = "Perform enrichment", icon=icon("undo"))  
        searchStatus$option <- "enrich"
        
        # Load search previous enrichment menu
        tmpDir <- paste0("./www/tmp/transaction/")
        enrichmentList <- Sys.glob("./www/tmp/transaction/*")
        
        if(length(enrichmentList) == 0) { # No previous enrichment
          shinyjs::disable(id = "searchPrevButton")
          shinyjs::disable(id = "searchDeleteAll")
          shinyjs::disable(id = "searchDeleteSelected")
          
          output[["enrichmentTable"]] <- renderUI(
            column(12,
              h4("No previous enrichment records!")
            )
          )
          
        } else {
          shinyjs::enable(id = "searchPrevButton")
          shinyjs::enable(id = "searchDeleteAll")
          shinyjs::enable(id = "searchDeleteSelected")
          
          enrichmentListDisplay <- lapply(enrichmentList, function(x){
            enrichDataRaw <- read.table(x, sep="\t", comment.char="", stringsAsFactors = FALSE, fill=TRUE)

            originalMode <- enrichDataRaw[1, 1]
            mode <- enrichDataRaw[1, 2]
            transactionId <- enrichDataRaw[1, 3]
            annoSelectStr <- enrichDataRaw[1, 4]
            nodeCutoff <- enrichDataRaw[1, 5]
            enrichmentSets <- enrichDataRaw[1, 6]
            submitTime <- enrichDataRaw[1, 10]
            
            beginTime <- "not started"
            endTime <- "incomplete"
            if(ncol(enrichDataRaw) < 12){
              # Get timestamps for missing entries
              # Get ending timestamp to put in file
              resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="getTimestamp", query=list(transactionId=transactionId))
              # TODO: error check
              if(resp$status_code != 200){
                return(NULL)
              }
              tmpContent <- unlist(content(resp))
              
              #Placeholder if hasn't started yet
              if(is.null(tmpContent)) {
                beginTime <- "not started"
                endTime <- "incomplete"
              } else {
                if("timestamp_start" %in% names(tmpContent["timestamp_start"])){
                  beginTime <- tmpContent[["timestamp_start"]]
                }
                if("timestamp_finish" %in% names(tmpContent["timestamp_finish"])){
                  endTime <- tmpContent[["timestamp_finish"]]
                }
              }

            } else {
              beginTime <- enrichDataRaw[1, 11]
              endTime <- enrichDataRaw[1, 12]  
            }
            
            cleanedMode <- ""
            if(originalMode == "similarity"){
              if(mode == "similarity") {
                cleanedMode <- "Enrich from Chemicals with Structural Similarity"
              } else { # re-enrichment
                cleanedMode <- "Re-enrich from Chemicals with Structural Similarity"
              }
            } else if (originalMode == "substructure"){
              if(mode == "substructure") {
                cleanedMode <- "Enrich from Chemicals with Shared Substructures"
              } else { # re-enrichment
                cleanedMode <- "Re-enrich from Chemicals with Shared Substructures"
              }
            } else if (originalMode == "casrn"){
              cleanedMode <- "Enrich from User-Provided CASRN List"
            } else { #annotation
              cleanedMode <- "View Annotations for Tox21 Chemicals"
            }
            
            # Clean up enrichment sets for display in the DataTable
            cleanedEnrichmentSets <- unlist(str_split(enrichmentSets, "\\|"))
            cleanedEnrichmentSetsDisplayNames <- unlist(lapply(cleanedEnrichmentSets, function(x){
              return(unlist(str_split(x, "__"))[2])
            }))
            cleanedEnrichmentSetsDisplayNames <- unique(cleanedEnrichmentSetsDisplayNames)
            
            cleanedEnrichmentSetsDisplay <- lapply(cleanedEnrichmentSetsDisplayNames, function(x){
              innerList <- unlist(lapply(cleanedEnrichmentSets, function(y){
                casrnNoSet <- unlist(str_split(y, "__"))
                if(casrnNoSet[2] == x) {
                  return(casrnNoSet[1])
                }
                return(NULL)
              }))
              innerList <- innerList[!sapply(innerList, is.null)]
              
              return(paste0(x, ": ", paste0(innerList, collapse=", ")))
              
            })
            
            # Clean up annoSelectStr
            annoSelectStr <- unlist(str_split(annoSelectStr, ","))
            annoSelectStr <- paste0(annoSelectStr, collapse=", ")
            annoSelectStr <- gsub("=checked", "", annoSelectStr)
            
            searchCheckbox <- paste0(checkboxInput(inputId = paste0("cb_search__", transactionId), label=NULL, width="4px"))
            
            enrichData <- data.frame("Mode"=cleanedMode, "UUID"=transactionId, "Annotation Selection String"=annoSelectStr, "Node Cutoff"=nodeCutoff, "Enrichment Sets"=paste0(cleanedEnrichmentSetsDisplay, collapse="\n"), "Time Submitted"=submitTime, "Time Started"=beginTime, "Time Completed"=endTime, stringsAsFactors = FALSE)
            enrichData <- data.frame("Select"=searchCheckbox, enrichData, stringsAsFactors = FALSE)
          })
          
          # Save checkbox names so we can reference later
          searchCheckboxNames <- unlist(lapply(enrichmentList, function(x){
            enrichDataRaw <- read.table(x, sep="\t", comment.char="", stringsAsFactors = FALSE)
            transactionId <- enrichDataRaw[1, 3]
            searchCheckboxName <- paste0("cb_search__", transactionId)
          }))
          
          enrichmentListDisplay <- bind_rows(enrichmentListDisplay)
          
          searchCheckboxes$checkboxes <- searchCheckboxNames
          
          output[["enrichmentTable"]] <- renderUI(
            column(12,
               DT::datatable({enrichmentListDisplay}, 
                             escape = FALSE,
                             rownames = FALSE,
                             class = "row-border stripe compact",
                             style = "bootstrap",
                             select = "none",
                             options = list( 
                               paging = TRUE,
                               #autoWidth = FALSE,
                               #columnDefs = list(list(width="200px", targets="_all")),
                               preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'), 
                               drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
                               dom = "Bfrtip",
                               buttons = list(list( extend="colvis", columns=as.vector(1:(ncol(enrichmentListDisplay)-1)) ))  
                               #                                                           ^^ this is so we always keep the select checkboxes in the table (user can't hide them)
                             ),
                             extensions = "Buttons"
               )
            )
          )
        }
      } else {
        # Show main page and hide search page
        shinyjs::hide(id = "searchForm")
        shinyjs::show(id = "enrichmentForm")
        shinyjs::disable(id = "searchForm")
        shinyjs::enable(id = "enrichmentForm")
        shinyjs::enable(id = "enrich_from")
        shinyjs::reset(id = "searchForm")
        
        # Show Select enrichment type selector and hide buttons
        shinyjs::hide(id = "searchButtonsMenu")
        shinyjs::show(id = "enrich_from")
        
        updateActionButton(session, "searchButton", label = "View previous results", icon=icon("search"))  
        searchStatus$option <- "search"
      }
    }
    
    # Search for selected request
    observeEvent(input$searchPrevButton, {
      setToFetch <- ""
      selectedSets <- lapply(searchCheckboxes$checkboxes, function(i){
        if(is.null(input[[i]]) == FALSE) {
          if(input[[i]] == TRUE) {
            return(unlist(str_split(i, "__"))[2])
          } else {
            return(NULL)
          }
        } else {
          return(NULL)
        }
      })
      selectedSets <- selectedSets[!sapply(selectedSets, is.null)]

      if(length(selectedSets) > 1){ # too many sets selected
        shinyjs::show(id="warningSearchColumn")
        output$searchWarning <- renderUI({
          HTML(paste0("<div class=\"text-danger\">Error: Only one request may be selected.</div>"))
        })
      } else if(length(selectedSets) < 1){ # no sets selected
        shinyjs::show(id="warningSearchColumn")
        output$searchWarning <- renderUI({
          HTML(paste0("<div class=\"text-danger\">Error: No requests selected.</div>"))
        })
      } else {
      
        setToFetch <- selectedSets[1]
        params <- read.table(paste0("./www/tmp/transaction/", setToFetch), sep="\t", comment.char="", stringsAsFactors = FALSE)
        originalMode <- params[1, 1]
        mode <- params[1, 2]
        transactionId <- params[1, 3]
        annoSelectStr <- params[1, 4]
        nodeCutoff <- params[1, 5]
        colorsList <- params[1, 9]
        
        # Check if result (Input/Output) files exist on the server
        resp <- NULL
        tryPrevious <- tryCatch(
          {
            resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="exists", query=list(transactionId=transactionId))    
            return(TRUE)
          }, error=function(cond){
            return(FALSE)
          }
        )
        if(tryPrevious == FALSE){
          # error here
          shinyjs::show(id="warningSearchColumn")
          output$searchWarning <- renderUI({
            HTML(paste0("<div class=\"text-danger\">Error: Cannot connect to Tox21 Enricher server.</div>"))
          })
          return(FALSE)
        }
        
        # TODO: error check
        if(resp$status_code != 200){
          # error here
          shinyjs::show(id="warningSearchColumn")
          output$searchWarning <- renderUI({
            HTML(paste0("<div class=\"text-danger\">Error: Problem fetching results from server.</div>"))
          })
          return(FALSE)
        } else {
          if(content(resp) == FALSE){
            #error here
            shinyjs::show(id="warningSearchColumn")
            output$searchWarning <- renderUI({
              HTML(paste0("<div class=\"text-danger\">Error: The results for this request are missing on the Tox21 Enricher server. The files may have been deleted, or the request may have failed. Please try again.</div>"))
            })
            return(FALSE)
          }
        }
        
        # Check if request has actually finished yet
        if(length(params) < 12){
          beginTime <- NULL
          endTime <- NULL
          # Get timestamps for missing entries - check if enrichment has completed
          resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="getTimestamp", query=list(transactionId=transactionId))
          # TODO: error check
          if(resp$status_code != 200){
            return(NULL)
          }
          tmpContent <- unlist(content(resp))
          if(is.null(tmpContent)) {
            beginTime <- NULL
            endTime <- NULL
          } else {
            if("timestamp_start" %in% names(tmpContent["timestamp_start"])){
              beginTime <- tmpContent[["timestamp_start"]]
            }
            if("timestamp_finish" %in% names(tmpContent["timestamp_finish"])){
              endTime <- tmpContent[["timestamp_finish"]]
            }
          }
          
          if(is.null(endTime) | is.null(beginTime)){
            shinyjs::show(id="warningSearchColumn")
            output$searchWarning <- renderUI({
              HTML(paste0("<div class=\"text-danger\">Error: Request has not completed.</div>"))
            })
            return() 
          }
        }
        
        # reconstruct enrichment sets
        setNames <- unlist(str_split(params[1, 6], "\\|"))
        setNames <- unlist(lapply(setNames, function(x){
          return(unlist(str_split(x, "__"))[2])
        }))
        setNames <- unique(setNames)
        enrichmentSets <- lapply(setNames, function(x){
          innerSet <- unlist(lapply(unlist(str_split(params[1, 6], "\\|")), function(y){
            tmpSplit <- unlist(str_split(y, "__"))
            if(tmpSplit[2] == x){
              return(tmpSplit[1])
            }
            return(NULL)
          }))
          innerSet <- innerSet[!sapply(innerSet, is.null)]
        })
        names(enrichmentSets) <- setNames
        originalNames <- unlist(str_split(params[1, 7], "\\|"))
        reenrichResults <- NULL
        if(!is.na(params[1, 8])){
          reenrichResultsSets <- unlist(str_split(params[1, 8], "\\|"))
          reenrichResultsCols <- lapply(reenrichResultsSets, function(x){
            return(unlist(str_split(x, "__")))
          })
          reenrichResultsMats <- lapply(reenrichResultsCols, function(x){
            innerList <- lapply(2:length(x), function(y){ # start at 2 to skip set name
              return(unlist(str_split(x[y], ";")))
            })
            # if similarity
            if(length(innerList) == 7){
              return(data.frame("casrn"=innerList[[1]], "m"=innerList[[2]], "similarity"=innerList[[3]], "cyanide"=innerList[[4]], "isocyanate"=innerList[[5]], "aldehyde"=innerList[[6]], "epoxide"=innerList[[7]], stringsAsFactors=FALSE))
            }
            # if anything else
            else {
              return(data.frame("casrn"=innerList[[1]], "m"=innerList[[2]], "cyanide"=innerList[[3]], "isocyanate"=innerList[[4]], "aldehyde"=innerList[[5]], "epoxide"=innerList[[6]], stringsAsFactors=FALSE))
            }
          })
          reenrichResultsNames <- lapply(reenrichResultsCols, function(x){
            return(x[1])
          })
          reenrichResults <- reenrichResultsMats
          names(reenrichResults) <- reenrichResultsNames
        }
        
        # Get colors list
        colorsList <- unlist(str_split(colorsList, "\\|"))
        colorsListSetNames <- lapply(colorsList, function(x){
          unlist(str_split(x, "__"))[1]
        })
        colorsListSetItems <- lapply(colorsList, function(x){
          unlist(str_split(x, "__"))[2]
        })
        colorsList <- colorsListSetItems
        names(colorsList) <- colorsListSetNames
        colorsList <- unlist(colorsList)
        
        shinyjs::hide(id = "searchForm")
        shinyjs::disable(id = "searchForm")
        future({
          enrichmentResults(mode, transactionId, annoSelectStr, nodeCutoff, enrichmentSets, originalNames, reenrichResults, originalMode, colorsList)
        })
      }
    })
    
    # Open settings modal menu
    observeEvent(input$settingsButton, {
      shinyjs::disable(id = "sidebar")
      showModal(
        modalDialog(
          title="Settings",
          footer=actionButton(inputId="closeSettingsButton", label="Save & Exit"),
          size="l",
          fluidRow(
            column(12, 
              h4("Clear Local Cache"),
              actionButton(inputId="clearCacheButton", label="Clear local cache", icon=icon("trash")),
              br(),
              p("This will clear the Tox21 Enricher client application's local storage and delete files like enrichment results and the manual. These files will have to be redownloaded in the future. This cannot be undone.")       
            )
          ),
          br(),
          br(),
        )
      )
    })
    
    observeEvent(input$closeSettingsButton, {
      removeModal()
      shinyjs::enable(id = "sidebar")
    })
    
    # update theme data when checkbox is clicked
    observeEvent(input$changeThemeToggle, {
      tmpDir <- paste0("./www/local/")
      if(input$changeThemeToggle == TRUE){ # dark
        theme$textcolor <- "#FFFFFF"
        file.create(paste0(tmpDir, "theme-dark"))
      } else { # light
        theme$textcolor <- "#000000"
        unlink(paste0(tmpDir, "theme-dark"))
      }
    }, ignoreInit=TRUE, ignoreNULL=TRUE)
    
    # Clear cache when button is pressed
    observeEvent(input$clearCacheButton, {
      showModal(
        modalDialog(
          title="Warning",
          footer=NULL,
          size="l",
          fluidRow(
            column(12, 
              HTML(paste0("<p>You are about to clear the application's local storage at: <p class=\"text-danger\">", getwd(), "/www/tmp/</p>This action cannot be undone. Continue?</p>")),
              column(6, actionButton(inputId="clearCacheButtonConfirm", label="Yes, clear the cache.") ),
              column(6, actionButton(inputId="clearCacheButtonCancel", label="Close") )
            )
          ),
          fluidRow(
            column(12,
              uiOutput("cacheConfirmation")
            )
          )
        )
      )
    })
    
    # Clear local cache on confirmation of above ^^
    observeEvent(input$clearCacheButtonConfirm, {
      cacheDir <- paste0(getwd(), "/www/tmp/")
      # Clear input dir
      unlink(paste0(cacheDir, "input/*"), recursive=TRUE)
      # Clear output dir
      unlink(paste0(cacheDir, "output/*"), recursive=TRUE)
      # Clear docs dir
      unlink(paste0(cacheDir, "docs/*"), recursive=TRUE)
      
      # Render confirmation text
      output$cacheConfirmation <- renderUI({
        HTML(paste0("<div class=\"text-danger\">Cache cleared.</div>"))
      })
    })
    
    
    # Close modal on cancellation of above ^^
    observeEvent(input$clearCacheButtonCancel, {
      # Clear previous message (if applicable)
      output$cacheConfirmation <- renderUI({
        paste0()
      })
      # Close modal
      removeModal()
    })
    
    
    # Clear previous enrichments when button is pressed
    observeEvent(input$searchDeleteAll, {
      showModal(
        modalDialog(
          title="Warning",
          footer=NULL,
          size="l",
          fluidRow(
            column(12, 
                   HTML(paste0("<p>You are about to delete all records of your previous requests. This action cannot be undone. Continue?</p>")),
                   column(6, actionButton(inputId="searchDeleteAllConfirm", label="Yes, delete all records.") ),
                   column(6, actionButton(inputId="searchDeleteAllCancel", label="Close") )
            )
          ),
          fluidRow(
            column(12,
                   uiOutput("searchDeleteAllConfirmation")
            )
          )
        )
      )
    })
    
    # Clear local cache on confirmation of above ^^
    observeEvent(input$searchDeleteAllConfirm, {
      cacheDir <- paste0(getwd(), "/www/tmp/")
      # Clear transaction dir
      unlink(paste0(cacheDir, "transaction/*"), recursive=TRUE)
      
      # Render confirmation text
      output$searchDeleteAllConfirmation <- renderUI({
        HTML(paste0("<div class=\"text-danger\">All records deleted.</div>"))
      })
    })
    
    # Close modal on cancellation of above ^^
    observeEvent(input$searchDeleteAllCancel, {
      # Clear previous message (if applicable)
      output$searchDeleteAllConfirmation <- renderUI({
        paste0()
      })
      
      # Refresh page
      searchStatus$option <- "search"
      loadEnrichList()
      
      # Close modal
      removeModal()
    })
    
    
    selectedSetsReactive <- reactiveValues(sets=NULL)
    # Delete records for selected requests
    observeEvent(input$searchDeleteSelected, {
      selectedSets <- lapply(searchCheckboxes$checkboxes, function(i){
        if(input[[i]] == TRUE) {
          return(unlist(str_split(i, "__"))[2])
        } else {
          return(NULL)
        }
      })
      selectedSets <- selectedSets[!sapply(selectedSets, is.null)]
      selectedSetsReactive$sets <- selectedSets
      
      if(length(selectedSets) < 1){
        shinyjs::show(id="warningSearchColumn")
        output$searchWarning <- renderUI({
          HTML(paste0("<div class=\"text-danger\">Error: No requests selected.</div>"))
        })
      } else {
        showModal(
          modalDialog(
            title="Warning",
            footer=NULL,
            size="l",
            fluidRow(
              column(12, 
                     HTML(paste0("<p>You are about to delete the records of the following requests:<br><div class=\"text-danger\">", paste0(selectedSets, collapse="<br>"), "</div><br>This action cannot be undone. Continue?</p>")),
                     column(6, actionButton(inputId="searchDeleteSelectedConfirm", label="Yes, delete the selected records.") ),
                     column(6, actionButton(inputId="searchDeleteSelectedCancel", label="Close") )
              )
            ),
            fluidRow(
              column(12,
                     uiOutput("searchDeleteSelectedConfirmation")
              )
            )
          )
        )
      }
    })
    
    # Clear local cache on confirmation of above ^^
    observeEvent(input$searchDeleteSelectedConfirm, {
      cacheDir <- paste0(getwd(), "/www/tmp/")
      # Clear transaction dir
      for (x in selectedSetsReactive$sets){
        unlink(paste0(cacheDir, "transaction/", x), recursive=TRUE)  
      }
      
      # Render confirmation text
      output$searchDeleteSelectedConfirmation <- renderUI({
        HTML(paste0("<div class=\"text-danger\">Selected records deleted.</div>"))
      })
    })
    
    # Close modal on cancellation of above ^^
    observeEvent(input$searchDeleteSelectedCancel, {
      # Clear previous message (if applicable)
      output$searchDeleteSelectedConfirmation <- renderUI({
        paste0()
      })
      
      # Refresh page
      searchStatus$option <- "search"
      loadEnrichList()
      
      # Close modal
      removeModal()
    })
    
    
    # Display API connection status
    pingAPI <- tryCatch(
      {
        resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="ping")
        if(resp$status_code != 200) {
          output$apiConnection <- renderUI({
            HTML(paste0("<div class=\"text-danger\">Could not connect to Tox21 Enricher server!</div>"))
          })
        } else {
          output$apiConnection <- renderUI({
            HTML(paste0("<div class=\"text-success\">Connection with Tox21 Enricher server successfully established.</div>"))
          })
        } 
      }, error=function(cond){
        output$apiConnection <- renderUI({
          HTML(paste0("<div class=\"text-danger\">Could not connect to Tox21 Enricher server!</div>"))
        })
      }
    )
    
    # Display list of annotations to select
    output$annotations <- renderUI({
      annoListFull <- list()
      getAnnotationsFromServer <- tryCatch(
        {
          annoListFull <- get_annotations()   
        }, error=function(cond){
          return(HTML("<div class=\"text-danger\">Error: Could not load any annotation classes.</div>"))
        }
      )
      if(length(annoListFull) < 1) {
        return(HTML("<div class=\"text-danger\">Error: Could not load any annotation classes.</div>"))
      }
      
      annoList <- annoListFull[1]
      selectedAnnoList <- reactiveValues()
      
      # Lists for each class type
      classPubChem <- annoListFull[annoListFull$annotype %in% c("PubChem Compound Annotation"), ]
      classPubChem <- classPubChem[, "annoclassname"]
      descPubChem <- annoListFull[annoListFull$annotype %in% c("PubChem Compound Annotation"), ]
      descPubChem <- descPubChem[, "annodesc"]
      descPubChem <- unlist(lapply(1:length(descPubChem), function(x){
        return(paste0(descPubChem[x]))
      }))

      classDrugMatrix <- annoListFull[annoListFull$annotype %in% c("DrugMatrix Annotation"), ]
      classDrugMatrix <- classDrugMatrix[, "annoclassname"]
      descDrugMatrix <- annoListFull[annoListFull$annotype %in% c("DrugMatrix Annotation"), ]
      descDrugMatrix <- descDrugMatrix[, "annodesc"]
      descDrugMatrix <- unlist(lapply(1:length(descDrugMatrix), function(x){
        return(paste0(descDrugMatrix[x]))
      }))
      
      classDrugBank <- annoListFull[annoListFull$annotype %in% c("DrugBank Annotation"), ]
      classDrugBank <- classDrugBank[, "annoclassname"]
      descDrugBank <- annoListFull[annoListFull$annotype %in% c("DrugBank Annotation"), ]
      descDrugBank <- descDrugBank[, "annodesc"]
      descDrugBank <- unlist(lapply(1:length(descDrugBank), function(x){
        return(paste0(descDrugBank[x]))
      }))
      
      classCTD <- annoListFull[annoListFull$annotype %in% c("CTD Annotation"), ]
      classCTD <- classCTD[, "annoclassname"]
      descCTD <- annoListFull[annoListFull$annotype %in% c("CTD Annotation"), ]
      descCTD <- descCTD[, "annodesc"]
      descCTD <- unlist(lapply(1:length(descCTD), function(x){
        return(paste0(descCTD[x]))
      }))
      
      classOther <- annoListFull[annoListFull$annotype %in% c("Other"), ]
      classOther <- classOther[, "annoclassname"]
      descOther <- annoListFull[annoListFull$annotype %in% c("Other"), ]
      descOther <- descOther[, "annodesc"]
      descOther <- unlist(lapply(1:length(descOther), function(x){
        return(paste0(descOther[x]))
      }))
      annodescList <- annoListFull[, "annodesc"]
      names(annodescList) <- annoListFull[, "annoclassname"]
      
      # Create list of annotation tooltips (help from https://stackoverflow.com/questions/36670065/tooltip-in-shiny-ui-for-help-text)
      ttPubChem <- lapply(1:length(descPubChem), function(x){
        tipify(bsButtonRight(paste0("tt_", classPubChem[x]), icon("question-circle"), style="inverse", size="extra-small"), descPubChem[x], placement="right")
      })
      ttDrugMatrix <- lapply(1:length(descDrugMatrix), function(x){
        tipify(bsButtonRight(paste0("tt_", classDrugMatrix[x]), icon("question-circle"), style="inverse", size="extra-small"), descDrugMatrix[x], placement="right")
      })
      ttDrugBank <- lapply(1:length(descDrugBank), function(x){
        tipify(bsButtonRight(paste0("tt_", classDrugBank[x]), icon("question-circle"), style="inverse", size="extra-small"), descDrugBank[x], placement="right")
      })
      ttCTD <- lapply(1:length(descCTD), function(x){
        tipify(bsButtonRight(paste0("tt_", classCTD[x]), icon("question-circle"), style="inverse", size="extra-small"), descCTD[x], placement="right")
      })
      ttOther <- lapply(1:length(descOther), function(x){
        tipify(bsButtonRight(paste0("tt_", classOther[x]), icon("question-circle"), style="inverse", size="extra-small"), descOther[x], placement="right")
      })
      
      annoClassList = list()
      annoClassList[[1]] <- classPubChem
      annoClassList[[2]] <- classDrugMatrix
      annoClassList[[3]] <- classDrugBank
      annoClassList[[4]] <- classCTD
      annoClassList[[5]] <- classOther
      annoClasses$classes <- annoClassList
      
      # Need to handle CTD annotations a little differently so that GOFAT_BP is not selected by default
      names(classCTD) <- lapply(1:length(classCTD), function(x){
        # Show warning for CTD_GOFAT_BIOPROCESS
        if(classCTD[[x]] == "CTD_GOFAT_BIOPROCESS"){
          return(paste0("CTD_GOFAT_BIOPROCESS (Very slow, unchecked by default)"))
        } else {
          return(classCTD[[x]])
        }
      })
      classCTDSelected <- lapply(names(classCTD), function(x){
        if(classCTD[[x]] == "CTD_GOFAT_BIOPROCESS"){
          return(NULL)
        } else {
          return(classCTD[[x]])
        }
      })
      names(classCTDSelected) <- names(classCTD)
      
      # Observers for deselect/select buttons for individual annotation categories
      selectPubChemValue <- reactiveValues(value="deselect")
      observeEvent(input$selectPubChem, {
        if(selectPubChemValue$value == "deselect"){
          updateCheckboxGroupInput(session, inputId="checkboxPubChem", selected=character(0))
          updateActionButton(session, inputId="selectPubChem", label="Select all PubChem annotations")
          selectPubChemValue$value <- "select"
        } else {
          updateCheckboxGroupInput(session, inputId="checkboxPubChem", selected=classPubChem)
          updateActionButton(session, inputId="selectPubChem", label="Deselect all PubChem annotations")
          selectPubChemValue$value <- "deselect"
        }
      })
      
      selectDrugMatrixValue <- reactiveValues(value="deselect")
      observeEvent(input$selectDrugMatrix, {
        if(selectDrugMatrixValue$value == "deselect"){
          updateCheckboxGroupInput(session, inputId="checkboxDrugMatrix", selected=character(0))
          updateActionButton(session, inputId="selectDrugMatrix", label="Select all DrugMatrix annotations")
          selectDrugMatrixValue$value <- "select"
        } else {
          updateCheckboxGroupInput(session, inputId="checkboxDrugMatrix", selected=classDrugMatrix)
          updateActionButton(session, inputId="selectDrugMatrix", label="Deselect all DrugMatrix annotations")
          selectDrugMatrixValue$value <- "deselect"
        }
      })
      
      selectDrugBankValue <- reactiveValues(value="deselect")
      observeEvent(input$selectDrugBank, {
        if(selectDrugBankValue$value == "deselect"){
          updateCheckboxGroupInput(session, inputId="checkboxDrugBank", selected=character(0))
          updateActionButton(session, inputId="selectDrugBank", label="Select all DrugBank annotations")
          selectDrugBankValue$value <- "select"
        } else {
          updateCheckboxGroupInput(session, inputId="checkboxDrugBank", selected=classDrugBank)
          updateActionButton(session, inputId="selectDrugBank", label="Deselect all DrugBank annotations")
          selectDrugBankValue$value <- "deselect"
        }
      })
      
      selectCTDValue <- reactiveValues(value="deselect")
      observeEvent(input$selectCTD, {
        if(selectCTDValue$value == "deselect"){
          updateCheckboxGroupInput(session, inputId="checkboxCTD", selected=character(0))
          updateActionButton(session, inputId="selectCTD", label="Select all CTD annotations")
          selectCTDValue$value <- "select"
        } else {
          updateCheckboxGroupInput(session, inputId="checkboxCTD", selected=classCTD)
          updateActionButton(session, inputId="selectCTD", label="Deselect all CTD annotations")
          selectCTDValue$value <- "deselect"
        }
      })
      
      selectOtherValue <- reactiveValues(value="deselect")
      observeEvent(input$selectOther, {
        if(selectOtherValue$value == "deselect"){
          updateCheckboxGroupInput(session, inputId="checkboxOther", selected=character(0))
          updateActionButton(session, inputId="selectOther", label="Select all Other annotations")
          selectOtherValue$value <- "select"
        } else {
          updateCheckboxGroupInput(session, inputId="checkboxOther", selected=classOther)
          updateActionButton(session, inputId="selectOther", label="Deselect all Other annotations")
          selectOtherValue$value <- "deselect"
        }
      })

      # Render checkboxes for each annotation class
      column(12,
          tabsetPanel(id="annotationClasses",
              tabPanel("PubChem Compound Annotations", 
                fluidRow(
                  column(12,
                    extendedCheckboxGroupInput("checkboxPubChem", "PubChem Compound Annotations", choices=classPubChem, selected=classPubChem, extensions=ttPubChem)
                  ),
                  column(12,
                    actionButton(inputId="selectPubChem", label="Deselect all PubChem annotations")       
                  )
                )
              ),
              tabPanel("DrugMatrix Annotations",
                fluidRow(
                  column(12,
                    extendedCheckboxGroupInput("checkboxDrugMatrix", "DrugMatrix Annotations", choices=classDrugMatrix, selected=classDrugMatrix, extensions=ttDrugMatrix)
                  ),
                  column(12,
                    actionButton(inputId="selectDrugMatrix", label="Deselect all DrugMatrix annotations")       
                  )
                )
              ),
              tabPanel("DrugBank Annotations",
                fluidRow(
                  column(12,
                    extendedCheckboxGroupInput("checkboxDrugBank", "DrugBank Annotations", choices=classDrugBank, selected=classDrugBank, extensions=ttDrugBank)
                  ),
                  column(12,
                    actionButton(inputId="selectDrugBank", label="Deselect all DrugBank annotations")       
                  )
                )
              ),
              tabPanel("CTD Annotations",
                fluidRow(
                  column(12,
                    extendedCheckboxGroupInput("checkboxCTD", "CTD Annotations", width="500px", choices=classCTD, selected=classCTDSelected, extensions=ttCTD)
                  ),
                  column(12,
                    actionButton(inputId="selectCTD", label="Deselect all CTD annotations")       
                  )
                )
              ),
              tabPanel("Other Annotations",
                fluidRow(
                  column(12,
                    extendedCheckboxGroupInput("checkboxOther", "Other Annotations", choices=classOther, selected=classOther, extensions=ttOther)
                  ),
                  column(12,
                    actionButton(inputId="selectOther", label="Deselect all Other annotations")       
                  )
                )
              )
          )
        )
    })
    
    #Create tooltip buttons for annotations (solution from https://stackoverflow.com/questions/36670065/tooltip-in-shiny-ui-for-help-text)
    bsButtonRight <- function(...) {
      btn <- bsButton(...)
      # Directly inject the style into the shiny element.
      btn$attribs$style <- "float: right;"
      btn
    }
    extendedCheckboxGroupInput <- function(..., extensions = list()) {
      cbg <- checkboxGroupInput(...)
      nExtensions <- length(extensions)
      nChoices <- length(cbg$children[[2]]$children[[1]])
      
      if (nExtensions > 0 && nChoices > 0) {
        lapply(1:min(nExtensions, nChoices), function(i) {
          # For each Extension, add the element as a child (to one of the checkboxes)
          cbg$children[[2]]$children[[1]][[i]]$children[[2]] <<- extensions[[i]]
        })
      }
      cbg
    }
    
    # Display chemical input type
    output$input_type <- renderText({
        if(input$enrich_from == "user-provided CASRN list" | input$enrich_from == "View annotations for Tox21 chemicals") {
            paste("Input CASRNs")
        } else {
            paste("Input SMILE/InChI Strings")
        }
    })
    
    # Change enrichment settings
    observeEvent(input$enrich_from, {
        if(input$enrich_from == "user-provided CASRN list") {
          enrichmentType$enrichType <- "casrn"
          shinyjs::show(id = "casrnExamples")
          shinyjs::hide(id = "smilesExamples")
          shinyjs::enable(id = "nodeCutoff")
          shinyjs::hide(id = "tanimotoThreshold")
          
          output[["inputInstructions"]] <- renderUI(
            p("Add \"#SetName\" before each set if using multiple sets at once. Set names may only be alphanumeric characters (A-Z, a-z, and 0-9) and spaces are ignored.")
          )
          
        } else if (input$enrich_from == "View annotations for Tox21 chemicals") {
          enrichmentType$enrichType <- "annotation"
          shinyjs::show(id = "casrnExamples")
          shinyjs::hide(id = "smilesExamples")
          # Disable node cutoff slider
          shinyjs::disable(id = "nodeCutoff")
          shinyjs::hide(id = "tanimotoThreshold")
          
          output[["inputInstructions"]] <- renderUI(
            p("Enter the CASRNs for ", tags$a(href="https://comptox.epa.gov/dashboard/chemical_lists/TOX21SL", "chemicals in the Tox21 screening library"),  " (one per line) to view each of their associated annotations in Tox21 Enricher. Add \"#SetName\" before each set if using multiple sets at once. Set names may only be alphanumeric characters (A-Z, a-z, and 0-9) and spaces are ignored.")
          )
          
        } else {
          if(input$enrich_from == "chemicals with shared substructures") {
            enrichmentType$enrichType <- "substructure"
            shinyjs::hide(id = "tanimotoThreshold")
          } else {
            enrichmentType$enrichType <- "similarity"
            shinyjs::show(id = "tanimotoThreshold")
          }
          shinyjs::hide(id = "casrnExamples")
          shinyjs::show(id = "smilesExamples")
          shinyjs::enable(id = "nodeCutoff")
          
          output[["inputInstructions"]] <- renderUI(
            p("Enter partial or complete SMILES or InChI strings, one per line.")
          )
          
        }
    })
    
    # Toggle Select/Deselect all for annotation classes
    selectStatus <- reactiveValues(option = character())
    selectStatus$option <- "deselect"
    observeEvent(input$select_all_annotations, {
        # Grab list of annotation classes
            # 1 = PubChem
            # 2 = DrugMatrix
            # 3 = DrugBank
            # 4 = CTD
            # 5 = Other
        annoClassList = annoClasses$classes
        if(selectStatus$option == "deselect") {
            updateCheckboxGroupInput(session, "checkboxPubChem", selected = "")
            updateCheckboxGroupInput(session, "checkboxDrugMatrix", selected = "")
            updateCheckboxGroupInput(session, "checkboxDrugBank", selected = "")
            updateCheckboxGroupInput(session, "checkboxCTD", selected = "")
            updateCheckboxGroupInput(session, "checkboxOther", selected = "")
            updateActionButton(session, "select_all_annotations", label = "Select all")
            selectStatus$option <- "select"
        } else {
            updateCheckboxGroupInput(session, "checkboxPubChem", selected = annoClassList[[1]])
            updateCheckboxGroupInput(session, "checkboxDrugMatrix", selected = annoClassList[[2]])
            updateCheckboxGroupInput(session, "checkboxDrugBank", selected = annoClassList[[3]])
            updateCheckboxGroupInput(session, "checkboxCTD", selected = annoClassList[[4]])
            updateCheckboxGroupInput(session, "checkboxOther", selected = annoClassList[[5]])
            updateActionButton(session, "select_all_annotations", label = "Deselect all")
            selectStatus$option <- "deselect"
        }
        
    })
    
    # Provide CASRNs example set (single) when button is clicked
    observeEvent(input$example_casrns, {
        updateTextAreaInput(session, "submitted_chemicals", value = "965-90-2\n50-50-0\n979-32-8\n4245-41-4\n143-50-0\n17924-92-4\n297-76-7\n152-43-2\n313-06-4\n4956-37-0\n112400-86-9")
    })
    
    # Provide CASRNs example set (multiple) when button is clicked
    observeEvent(input$example_casrnsMulti, {
        updateTextAreaInput(session, "submitted_chemicals", value = "#BPA analogs\n2081-08-5\n2467-02-9\n1478-61-1\n41481-66-7\n5613-46-7\n57-63-6\n620-92-8\n77-40-7\n79-94-7\n79-95-8\n79-97-0\n80-05-7\n80-09-1\n843-55-0\n94-18-8\n#Flame retardants\n115-86-6\n115-96-8\n1241-94-7\n1330-78-5\n13674-87-8\n29761-21-5\n5436-43-1\n56803-37-3\n68937-41-7\n78-30-8\n79-94-7\n#PAH\n120-12-7\n129-00-0\n191-24-2\n206-44-0\n218-01-9\n50-32-8\n53-70-3\n56-55-3\n83-32-9\n85-01-8\n")
    })
    
    # Provide SMILES example set when button is clicked
    observeEvent (input$example_smiles, {
        updateTextAreaInput(session, "submitted_chemicals", value = "ClCC1=CC=CC=C1\nN#CSCC1=CC=CC=C1\nInChI=1S/C8H11N/c1-9(2)8-6-4-3-5-7-8/h3-7H,1-2H3")
    })
    
    # Clear CASRNs input box
    observeEvent(input$clear_casrns, {
        updateTextAreaInput(session, "submitted_chemicals", value = "")
    })
    
    # Show/hide JSME input
    jsmeState <- reactiveValues(jsmeShowState = "")
    jsmeState$jsmeShowState <- "show"
    
    observeEvent(input$jsme_button, {
        if (jsmeState$jsmeShowState == "show") {
            shinyjs::show("jsmeInput")
            updateActionButton(session, "jsme_button", label = "Hide JSME")
            jsmeState$jsmeShowState <- "hide"
        }
        else {
            shinyjs::hide("jsmeInput")
            updateActionButton(session, "jsme_button", label = "Draw molecules with JSME")
            jsmeState$jsmeShowState <- "show"
        }
    })
    
    # Update waiting page/queue position
    observeEvent(input$refreshWaitingPageButton, {
      transactionId <- reactiveTransactionId$id
      queuePos <- -1
      resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="getQueuePos", query=list(transactionId=transactionId))
      if(resp$status_code != 200) {
        output$results_error_box <- renderUI({
          paste0("<div class=\"text-danger\">Error: Could not fetch queue position for this request.</div>")
        })
      } else {
        queuePos <- content(resp)  
      }
      waitingData <- inputSetList$inputSet
      enrichmentDisplayType <- waitingData[1,"Mode"]
      splitAnnotationsString <- waitingData[1,"Selected.Annotations"]
      cutoff <- waitingData[1,"Node.Cutoff"]
      casrnBox <- waitingData[1,"Input"]
      
      # Check reenrichment or not
      reenrichFlag <- reenrichFlagReactive$reenrichFlagReactive
      
      # Cleaned enrichment type strings
      if(reenrichFlag == TRUE){
        if(originalEnrichModeList$originalEnrichMode == "similarity") {
          enrichmentDisplayType <- "Re-enrich from chemicals with structural similarity" 
        } else if (originalEnrichModeList$originalEnrichMode == "substructure") { # substructure
          enrichmentDisplayType <- "Re-enrich from chemicals with shared substructures"
        } else { #casrn network node update
          enrichmentDisplayType <- "Enrich from user-provided CASRN list"
        }
      } else {
        if(enrichmentDisplayType == "casrn") {
          enrichmentDisplayType <- "Enrich from user-provided CASRN list"
        } else if(enrichmentDisplayType == "annotation") {
          enrichmentDisplayType <- "View annotations for Tox21 chemicals"
        } else if(enrichmentDisplayType == "similarity") {
          enrichmentDisplayType <- "Enrich from chemicals with structural similarity"
        } else { # substructure
          enrichmentDisplayType <- "Enrich from chemicals with shared substructures"
        }
      }
        
      # update waitingData with cleaned output
      waitingData <- data.frame("Position"=c(queuePos), "Mode"=c(enrichmentDisplayType), "UUID"=c(transactionId), "Selected Annotations"=c(splitAnnotationsString), "Node Cutoff"=c(cutoff), "Input"=c(casrnBox), stringsAsFactors=FALSE)
      
      # Clean up column name formatting
      colnames(waitingData) <- list("Queue Position", "Request Mode", "Request UUID", "Selected Annotations", "Node Cutoff", "User Input")

      
      output[["waitingTable"]] <- renderUI(
        column(12,
               DT::datatable({waitingData}, 
                             escape = FALSE,
                             rownames = FALSE,
                             class = "row-border stripe compact",
                             style = "bootstrap",
                             select = "none"
               )
        )
      )
      
    })
    
    # Copies transaction ID to user's clipboard
    output$clipboard <- renderUI({
      rclipButton("copyUUIDButton", "Copy UUID to clipboard", reactiveTransactionId$id, icon("clipboard"))
    })
    
    # Refresh enrichment form
    observeEvent(input$refresh, {
        # Initialize all annotation classes to checked and button mode to "deselect"
        annoClassList = annoClasses$classes
        updateCheckboxGroupInput(session, "checkboxPubChem", selected = annoClassList[[1]])
        updateCheckboxGroupInput(session, "checkboxDrugMatrix", selected = annoClassList[[2]])
        updateCheckboxGroupInput(session, "checkboxDrugBank", selected = annoClassList[[3]])
        updateCheckboxGroupInput(session, "checkboxCTD", selected = annoClassList[[4]])
        updateCheckboxGroupInput(session, "checkboxOther", selected = annoClassList[[5]])
        updateActionButton(session, "select_all_annotations", label = "Deselect all")
        selectStatus$option <- "deselect"
        shinyjs::reset(id = "enrichmentForm")
        shinyjs::reset(id = "resultsContainer")
        shinyjs::show(id = "enrichmentForm")
        shinyjs::enable(id = "enrichmentForm")
        shinyjs::reset(id = "select_all_annotations")
        shinyjs::reset(id = "enrich_from")
        shinyjs::reset(id = "submitted_chemicals")
        
        # Clear the chemical submission text area
        updateTextAreaInput(session, "submitted_chemicals", value="")
        
        # Allow user to change enrichment type
        shinyjs::enable(id = "enrich_from")
        
        # Hide the refresh button
        shinyjs::hide(id = "refresh")
        
        # Re-enable View previous results button
        shinyjs::enable(id="searchButton")
        updateActionButton(session, "searchButton", label = "View previous results", icon=icon("search"))
        searchStatus$option <- "search"
        
        # Reset reenrichResultsList$reenrichResults
        reenrichResultsList$reenrichResults <- NULL
        
        # clear the previous enrichment's results and hide
        shinyjs::hide("resultsContainer")
        shinyjs::reset("resultsContainer")
        output[["resultsTabset"]] <- renderUI(
            div(id="resultsTmp")
        )

        # refresh session entirely
        #session$reload()
    })
    
    # Keep track of what enrichment type is currently selected
    enrichmentType <- reactiveValues(enrichType = character())
    enrichmentType$enrichType <- "casrn"
    
    # Perform CASRN enrichment analysis when submit button is pressed
    observeEvent(input$submit, {
      output$error_box <- renderUI({
        p()
      })
      # First, check if user filled out text input form  
      # Remove whitespace for checking
      validatedInput <- gsub("\\s*", "", input$submitted_chemicals)
      if(validatedInput == "") {
        shinyjs::show(id="errorBox")
        output$error_box <- renderUI({
          HTML(paste0("<div class=\"text-danger\">Error: No input lines.</div>")) 
        })
        return(FALSE)
      }
      
      casrnValidatedInput <- ""
      errorCasrns <- list()
      inputToSubmit <- input$submitted_chemicals
      
      # Validate Input
      
      # 1) strip horizontal whitespace and condense multiple newlines
      casrnValidatedInput <- gsub(" ", "", input$submitted_chemicals)
      casrnValidatedInput <- gsub("\\t", "", casrnValidatedInput)
      casrnValidatedInput <- gsub("\\n+", "\n", casrnValidatedInput)
      
      # If CASRN input, do the following:
      if(enrichmentType$enrichType == "casrn" | enrichmentType$enrichType == "annotation") {

        # 2a) check if of the form ###-###-### or setname
        casrnValidatedInput <- unlist(str_split(casrnValidatedInput, "\n"))
        errorCasrnsIndex <- 1
        for(i in 1:length(casrnValidatedInput)) {
          if(!grepl("^#[A-Za-z0-9]+|[0-9]+-[0-9]+-[0-9]+", casrnValidatedInput[i], ignore.case=TRUE)) {
            errorCasrns[errorCasrnsIndex] <- i
            errorCasrnsIndex <- errorCasrnsIndex + 1
          }
        }
        # If there are errors
        if(length(errorCasrns) > 0){
          shinyjs::show(id="errorBox")
          output$error_box <- renderUI({
            HTML(paste0("<div class=\"text-danger\">Error: Incorrect CASRN or set name formatting on input line(s): <b>", paste0(errorCasrns, collapse=", "), "</b>. Please check your input and try again.</div>")) 
          })
          return(FALSE) 
        }
        inputToSubmit <- paste0(casrnValidatedInput, collapse="\n")
        
        # 3a) check if missing first set name, if you are using set names
        usingSetNames <- FALSE
        for(i in 1:length(casrnValidatedInput)) {
          if(grepl("^#[A-Za-z0-9]+", casrnValidatedInput[i], ignore.case=TRUE)) { # Detect if we are using set names
            usingSetNames <- TRUE
            break
          }
        }
        if(!grepl("^#[A-Za-z0-9]+", casrnValidatedInput[1], ignore.case=TRUE) & usingSetNames == TRUE) {
          shinyjs::show(id="errorBox")
          output$error_box <- renderUI({
            HTML(paste0("<div class=\"text-danger\">Error: It appears you are using set names but have not provided a name for the first input set. Please check your input and try again.</div>")) 
          })
          return(FALSE)
        }
      }
      performEnrichment(inputToSubmit, reenrichFlag=FALSE)  
    })
    
    # Create reactive variable to store chemicals to re-enrich
    checkboxList <- reactiveValues(checkboxes = NULL)
    finalTableToDisplay <- reactiveValues(table = NULL)
    
    # Create reactive variable to store reenrich flag
    reenrichFlagReactive <- reactiveValues(reenrichFlag = FALSE)
    
    # Create reactive variable to store chemicals with reactive structure warnings
    warningList <- reactiveValues(warnings = NULL)

    # Create reactive variable to store data related to input set
    inputSetList <- reactiveValues(inputSet = NULL)
    enrichmentSetsList <- reactiveValues(enrichmentSets = NULL)
    
    # Create reactive variable to store original input set names
    originalNamesList <- reactiveValues(originalNames = NULL)
    
    # Create reactive variable to store original enrichment mode for re-enrichment
    originalEnrichModeList <- reactiveValues(originalEnrichMode = NULL)
    
    reenrichResultsList <- reactiveValues(reenrichResults = NULL)
    
    # Create reactive variable to store transaction ID
    reactiveTransactionId <- reactiveValues(id = NULL)
    
    # Create reactive variables to store data related to bargraphs
    bgChartFullReactive <- reactiveValues(bgChartFull = NULL)
    bgChartAllCategoriesReactive <- reactiveValues(bgChartAllCategories = NULL)
    
    # Create reactive variable to store color data for plotting for each input set
    setColors <- reactiveValues(color = NULL)
    
    performEnrichment <- function(casrnBox, reenrichFlag=FALSE, originalNamesToReturn=NULL) {
        # If reenriching, always do CASRN
        if(reenrichFlag == FALSE){
          reenrichFlagReactive$reenrichFlagReactive <- FALSE
          originalEnrichModeList$originalEnrichMode <- enrichmentType$enrichType
        }
        else {
          reenrichFlagReactive$reenrichFlagReactive <- TRUE
          enrichmentType$enrichType <- "casrn"
        }
        # Hide waiting page warnings
        output$results_error_box <- renderText(paste0(""))
      
        # Hide original form when done with enrichment
        shinyjs::hide(id="enrichmentForm")
      
        # Disable changing input type when button is clicked
        shinyjs::disable(id="enrich_from")
        
        # Disable & Hide results page
        shinyjs::disable(id="enrichmentResults")
        shinyjs::hide(id="resultsContainer")
        
        # Show 'Restart' button, disable by default so user can't interfere with enrichment process
        shinyjs::show(id="refresh")
        shinyjs::disable(id="refresh")
        
        # Disable View previous results button
        shinyjs::disable(id="searchButton")

        # Hide any previous errors
        shinyjs::hide(id="errorBox")
        
        # Reset checkboxList$checkboxes
        checkboxList$checkboxes <- NULL
      
        # Get node cutoff. If re-enriching, set cutoff to value set by re-enrichment slider
        if(reenrichFlag == TRUE){
          cutoff <- input$nodeCutoffRe
        } else {
          cutoff <- input$nodeCutoff
        }
        
        # If TRUE, signifies that client app cannot connect to API
        badConnectionFlag <- FALSE
        
        # Initialize list to hold result chemicals for reenrichment (Substructure & Similarity only)
        reenrichResults <- list()
        casrnBoxSplit <- unlist(str_split(casrnBox, "\n"))

        # Split CASRNBox to get each line and organize into sets
        if (enrichmentType$enrichType == "casrn" | enrichmentType$enrichType == "annotation") {
            enrichmentSets <- list()
            if (grepl("#", casrnBox, fixed=TRUE)) { # If more than 1 set
                setName <- ""
                enrichmentSets <- unlist(str_split(casrnBox, "#"))
                enrichmentSets <- lapply(enrichmentSets, function(i){ 
                  unlist(str_split(i, "\n"))
                })
                names(enrichmentSets) <- lapply(enrichmentSets, function(i){
                  return(i[1])
                })
                enrichmentSets <- lapply(enrichmentSets, function(i){
                  return(i[-1]) # Remove names from inside each list
                })
                enrichmentSets <- lapply(enrichmentSets, function(i){
                  if(length(i) < 1) {
                    return(NULL)
                  } else {
                    return(i)
                  }
                })
                enrichmentSets <- enrichmentSets[!sapply(enrichmentSets, is.null)]
            } else { # Give arbitrary name if user only submitted a single set
                enrichmentSets <- str_split(casrnBox, "\n")
                names(enrichmentSets) <- list("Set1")
            }
            # Remove empty elements
            enrichmentSets <- lapply(enrichmentSets, function(i) return(i[lapply(i, nchar) > 0]))

            # If re-enriching, create reenrichResults list
            if(originalEnrichModeList$originalEnrichMode == "substructure" | originalEnrichModeList$originalEnrichMode == "similarity"){
              reenrichResults <- lapply(1:length(enrichmentSets), function(casrnSet){
                innerCasrnSet <- sapply(enrichmentSets[[casrnSet]], function(casrn){
                  queryReenrichCasrns <- NULL
                  if(originalEnrichModeList$originalEnrichMode == "substructure"){
                    # Query API to get list of annotation classes and types
                    resp <- NULL
                    trySubstructure <- tryCatch(
                      {
                        resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="substructure", query=list(input=casrn, reenrich=TRUE))
                        if(resp$status_code != 200){
                          return(FALSE)
                        }
                      }, error=function(cond){
                        return(FALSE)
                      }
                    )
                    
                    outputSubCasrns <- unlist(lapply(content(resp), function(x){
                      return(x$casrn)
                    }))
                    outputSubM <- unlist(lapply(content(resp), function(x){
                      return(x$m)
                    }))
                    outputSubCyanide <- unlist(lapply(content(resp), function(x){
                      return(x$cyanide)
                    }))
                    outputSubIsocyanate <- unlist(lapply(content(resp), function(x){
                      return(x$isocyanate)
                    }))
                    outputSubAldehyde <- unlist(lapply(content(resp), function(x){
                      return(x$aldehyde)
                    }))
                    outputSubEpoxide <- unlist(lapply(content(resp), function(x){
                      return(x$epoxide)
                    }))
                    outpReenrichCasrns <- data.frame(casrn=outputSubCasrns, m=outputSubM, cyanide=outputSubCyanide, isocyanate=outputSubIsocyanate, aldehyde=outputSubAldehyde, epoxide=outputSubEpoxide, stringsAsFactors=FALSE)
                    
                  } else if(originalEnrichModeList$originalEnrichMode == "similarity"){
                    
                    # Set Tanimoto threshold for similarity search
                    threshold <- as.numeric(input$tanimotoThreshold)/100
                    resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="similarity", query=list(input=originalNamesToReturn[[names(enrichmentSets)[casrnSet]]], threshold=threshold))
                    
                    if(resp$status_code != 200){
                      return(NULL)
                    }
                    
                    outputSimCasrns <- unlist(lapply(content(resp), function(x){
                      return(x$casrn)
                    }))
                    outputSimM <- unlist(lapply(content(resp), function(x){
                      return(x$m)
                    }))
                    outputSimSimilarity <- unlist(lapply(content(resp), function(x){
                      return(x$similarity)
                    }))
                    outputSimCyanide <- unlist(lapply(content(resp), function(x){
                      return(x$cyanide)
                    }))
                    outputSimIsocyanate <- unlist(lapply(content(resp), function(x){
                      return(x$isocyanate)
                    }))
                    outputSimAldehyde <- unlist(lapply(content(resp), function(x){
                      return(x$aldehyde)
                    }))
                    outputSimEpoxide <- unlist(lapply(content(resp), function(x){
                      return(x$epoxide)
                    }))
                    
                    reenrichSimilarityResults <- data.frame(casrn=outputSimCasrns, m=outputSimM, similarity=outputSimSimilarity, cyanide=outputSimCyanide, isocyanate=outputSimIsocyanate, aldehyde=outputSimAldehyde, epoxide=outputSimEpoxide, stringsAsFactors=FALSE)
                    reenrichSimilarityResultsReturn <- unlist(sapply(1:nrow(reenrichSimilarityResults), function(j){
                      if(reenrichSimilarityResults[j, "casrn"] == casrn) {
                        return(reenrichSimilarityResults[j,])
                      } else {
                        return(NULL)
                      }
                    }))
                    reenrichSimilarityResultsReturn <- reenrichSimilarityResultsReturn[!sapply(reenrichSimilarityResultsReturn, is.null)]
                  }
                  
                })
                innerCasrnSet <- as.data.frame(t(innerCasrnSet), stringsAsFactors=FALSE)
                row.names(innerCasrnSet) <- 1:nrow(innerCasrnSet)
                return(innerCasrnSet)
              })
              names(reenrichResults) <- names(enrichmentSets)
            }
        } else {
            enrichmentSets <- list()
            setName <- ""

            reenrichResults <- lapply(casrnBoxSplit, function(i){
                setName <- i
                if(setName != ""){
                  outpSmiles <- NULL
                  if (enrichmentType$enrichType == "substructure") {
                      resp <- NULL
                      trySubstructure <- tryCatch(
                        {
                          resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="substructure", query=list(input=setName, reenrich=FALSE))
                          if(resp$status_code != 200){
                            return(FALSE)
                          }
                        }, error=function(cond){
                          return(FALSE)
                        }
                      )

                      if(!is.null(trySubstructure)) {
                        if(trySubstructure == FALSE){
                          badConnectionFlag <<- TRUE
                          return(NULL)
                        }
                      }
                      
                      if(length(content(resp)) < 1){
                        return(NULL)
                      }    
                      
                      outputSubCasrns <- unlist(lapply(content(resp), function(x){
                        return(x$casrn)
                      }))
                      outputSubM <- unlist(lapply(content(resp), function(x){
                        return(x$m)
                      }))
                      outputSubCyanide <- unlist(lapply(content(resp), function(x){
                        return(x$cyanide)
                      }))
                      outputSubIsocyanate <- unlist(lapply(content(resp), function(x){
                        return(x$isocyanate)
                      }))
                      outputSubAldehyde <- unlist(lapply(content(resp), function(x){
                        return(x$aldehyde)
                      }))
                      outputSubEpoxide <- unlist(lapply(content(resp), function(x){
                        return(x$epoxide)
                      }))
                      
                      outpSmiles <- data.frame(casrn=outputSubCasrns, m=outputSubM, cyanide=outputSubCyanide, isocyanate=outputSubIsocyanate, aldehyde=outputSubAldehyde, epoxide=outputSubEpoxide, stringsAsFactors=FALSE)
                      
                  } else {
                    # Set Tanimoto threshold for similarity search
                    threshold <- as.numeric(input$tanimotoThreshold)/100
                    resp <- NULL
                    trySimilarity <- tryCatch(
                      {
                        resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="similarity", query=list(input=setName, threshold=threshold))
                        if(resp$status_code != 200){
                          return(FALSE)
                        }    
                      }, error=function(cond){
                        return(FALSE)
                      }
                    )

                    if(!is.null(trySimilarity)){
                      if(trySimilarity == FALSE){
                        badConnectionFlag <<- TRUE
                        return(NULL)
                      }
                    }

                    outputSimCasrns <- unlist(lapply(content(resp), function(x){
                      return(x$casrn)
                    }))
                    outputSimM <- unlist(lapply(content(resp), function(x){
                      return(x$m)
                    }))
                    outputSimSimilarity <- unlist(lapply(content(resp), function(x){
                      return(x$similarity)
                    }))
                    outputSimCyanide <- unlist(lapply(content(resp), function(x){
                      return(x$cyanide)
                    }))
                    outputSimIsocyanate <- unlist(lapply(content(resp), function(x){
                      return(x$isocyanate)
                    }))
                    outputSimAldehyde <- unlist(lapply(content(resp), function(x){
                      return(x$aldehyde)
                    }))
                    outputSimEpoxide <- unlist(lapply(content(resp), function(x){
                      return(x$epoxide)
                    }))
                    outpSmiles <- data.frame(casrn=outputSimCasrns, m=outputSimM, similarity=outputSimSimilarity, cyanide=outputSimCyanide, isocyanate=outputSimIsocyanate, aldehyde=outputSimAldehyde, epoxide=outputSimEpoxide, stringsAsFactors=FALSE)
                  }
                  
                  # Throw error if we have no good sets
                  if(is.null(outpSmiles) == TRUE) {
                    return(NULL)
                  } else if(nrow(outpSmiles) < 1) {
                    return(NULL)
                  }
                  if(nrow(outpSmiles) > 0){
                    return(outpSmiles)
                  }
                }
            })
        }
        
        if (enrichmentType$enrichType == "similarity" | enrichmentType$enrichType == "substructure") {
          # Error if no good sets
          if(length(unlist(reenrichResults, recursive=FALSE)) < 1){
            # Hide original form when done with enrichment
            shinyjs::show(id="enrichmentForm")
            
            # Disable changing input type when button is clicked
            shinyjs::enable(id="enrich_from")
            
            # Show 'Restart' button, disable by default so user can't interfere with enrichment process
            shinyjs::hide(id="refresh")
            shinyjs::enable(id="refresh")
            
            # Hide loading spinner
            shinyjs::hide(id="resultsContainer")
            
            # Show error message on main page
            shinyjs::show(id="errorBox")
            
            if(badConnectionFlag == TRUE){
              output$error_box <- renderUI({
                HTML(paste0("<div class=\"text-danger\">Error: Cannot connect to the Tox21 Enricher server.</div>"))
              })
            } else {
              output$error_box <- renderUI({
                HTML(paste0("<div class=\"text-danger\">Error: No valid input sets.</div>"))
              })  
            }
            return(FALSE)
          }
          
          names(reenrichResults) <- unlist(lapply(1:length(casrnBoxSplit), function(i){
            return(paste0("Set", i))
          }))
          
          enrichmentSets <- lapply(1:length(reenrichResults), function(i){
            if(is.null(reenrichResults[[i]]) == FALSE) {
              enrichmentSetsInside <- unlist(lapply(1:nrow(reenrichResults[[i]]), function(j){
                return(reenrichResults[[i]][j, "casrn"])
              }))
            } else {
              return(NULL)
            }
          })
          names(enrichmentSets) <- names(reenrichResults)
        }
        reenrichResults <- reenrichResults[!sapply(reenrichResults, is.null)]
        enrichmentSets <- enrichmentSets[!sapply(enrichmentSets, is.null)]
        
        # Generate colors for each input sets
        colorsAllSets <- unlist(lapply(names(enrichmentSets), function(x){
          # Generate a random color
          randColor <- sample(1:277, 1, replace=FALSE)
          randColor <- col2rgb(getNetworkColors()[randColor])
          return(paste0("rgb(", randColor[1], ", ", randColor[2], ", ", randColor[3], ")"))
        }))
        names(colorsAllSets) <- names(enrichmentSets)
        setColors$color <- colorsAllSets
        reenrichResultsList$reenrichResults <- reenrichResults
        enrichmentSetsList$enrichmentSets <- enrichmentSets
        
        # Get list of original input names for InChIs/SMILES and set names to display later
        if(reenrichFlag == FALSE){
          originalNames <- unlist(lapply(1:length(casrnBoxSplit), function(i) paste0(casrnBoxSplit[i], "__Set", i)))
          originalNamesList$originalNames <- originalNames
        }

        # Generate UUID for this query
        transactionId <- UUIDgenerate()
        # Access filesystem to see if UUID already exists
        resp <- NULL
        tryGenerateUUID <- tryCatch(
          {
            resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="checkId")
          }, error=function(cond){
            return(NULL)
          }
        )
        
        if(is.null(tryGenerateUUID)){
          # Hide original form when done with enrichment
          shinyjs::show(id="enrichmentForm")
          # Disable changing input type when button is clicked
          shinyjs::enable(id="enrich_from")
          # Show 'Restart' button, disable by default so user can't interfere with enrichment process
          shinyjs::hide(id="refresh")
          shinyjs::enable(id="refresh")
          # Hide loading spinner
          shinyjs::hide(id="resultsContainer")
          # Show error message on main page
          shinyjs::show(id="errorBox")
          output$error_box <- renderUI({
            HTML(paste0("<div class=\"text-danger\">Error: Cannot connect to Tox21 Enricher server.</div>"))
          })
          return(FALSE)
        }
        
        if(resp$status_code != 200){
          # Hide original form when done with enrichment
          shinyjs::show(id="enrichmentForm")
          # Disable changing input type when button is clicked
          shinyjs::enable(id="enrich_from")
          # Show 'Restart' button, disable by default so user can't interfere with enrichment process
          shinyjs::hide(id="refresh")
          shinyjs::enable(id="refresh")
          # Hide loading spinner
          shinyjs::hide(id="resultsContainer")
          # Show error message on main page
          shinyjs::show(id="errorBox")
          output$error_box <- renderUI({
            HTML(paste0("<div class=\"text-danger\">Error: Problem generating input UUID for enrichment.</div>"))
          })
          return(FALSE)
        } else {
          fullIDs <- unlist(content(resp), recursive=FALSE)
          fullIDs <- unlist(lapply(fullIDs, function(x){
            # Get just the ID
            return(unlist(str_split(x, "Output/"))[2])
          }))
          
          while(transactionId %in% fullIDs){
            # Regenerate UUID
            transactionId <- UUIDgenerate()
          }
        }
        
        # If UUID is good, assign to reactiveValue
        reactiveTransactionId$id <- transactionId

        inDirWeb <- paste0("Input/", transactionId, "/")
        outDirWeb <- paste0("Output/", transactionId, "/")

        # Get selected annotation classes
        annoSelectStr <- ""
        for (i in input$checkboxPubChem) {
            annoSelectStr <- paste0(annoSelectStr, i, "=checked,")
        }
        for (i in input$checkboxDrugMatrix) {
            annoSelectStr <- paste0(annoSelectStr, i, "=checked,")
        }
        for (i in input$checkboxDrugBank) {
            annoSelectStr <- paste0(annoSelectStr, i, "=checked,")
        }
        for (i in input$checkboxCTD) {
            annoSelectStr <- paste0(annoSelectStr, i, "=checked,")
        }
        for (i in input$checkboxOther) {
            annoSelectStr <- paste0(annoSelectStr, i, "=checked,")
        }

        # Convert enrichmentSets into a form that is API-friendly
        enrichmentSetsSanitized <- lapply(names(enrichmentSets), function(enrichmentSet){
          paste0(paste0(enrichmentSets[[enrichmentSet]], collapse=paste0("__", enrichmentSet, "\n")), "__", enrichmentSet)
        })
        enrichmentSetsSanitized <- paste0(enrichmentSetsSanitized, collapse="\n")
        
        # Convert enrichmentSets into a form that is API-friendly 2
        enrichmentSetsSanitizedLocal <- lapply(names(enrichmentSets), function(enrichmentSet){
          paste0(paste0(enrichmentSets[[enrichmentSet]], collapse=paste0("__", enrichmentSet, "|")), "__", enrichmentSet)
        })
        enrichmentSetsSanitizedLocal <- paste0(enrichmentSetsSanitizedLocal, collapse="|")

        # Convert reenrichResultsList$reenrichResults into a form that is API-friendly
        reenrichResultsSanitized <- ""
        if(length(reenrichResultsList$reenrichResults) > 0){
          reenrichResultsSanitized <- unlist(lapply(1:length(reenrichResultsList$reenrichResults), function(reenrichResult){
            rr_setname    <- names(reenrichResultsList$reenrichResults)[[reenrichResult]]
            rr_casrn      <- paste0(reenrichResultsList$reenrichResults[[reenrichResult]][,"casrn"], collapse=";")
            rr_m          <- paste0(reenrichResultsList$reenrichResults[[reenrichResult]][,"m"], collapse=";")
            rr_sim <- NULL
            if("similarity" %in% names(reenrichResultsList$reenrichResults[[reenrichResult]])){
              rr_sim      <- paste0(reenrichResultsList$reenrichResults[[reenrichResult]][,"similarity"], collapse=";")
            }
            rr_cyanide    <- paste0(reenrichResultsList$reenrichResults[[reenrichResult]][,"cyanide"], collapse=";")
            rr_isocyanate <- paste0(reenrichResultsList$reenrichResults[[reenrichResult]][,"isocyanate"], collapse=";")
            rr_aldehyde   <- paste0(reenrichResultsList$reenrichResults[[reenrichResult]][,"aldehyde"], collapse=";")
            rr_epoxide    <- paste0(reenrichResultsList$reenrichResults[[reenrichResult]][,"epoxide"], collapse=";")
            
            if(is.null(rr_sim)){ # no similarity column
              return(paste0(rr_setname, "__", rr_casrn, "__", rr_m, "__", rr_cyanide, "__", rr_isocyanate, "__", rr_aldehyde, "__", rr_epoxide))
            }
            return(paste0(rr_setname, "__", rr_casrn, "__", rr_m, "__", rr_sim, "__", rr_cyanide, "__", rr_isocyanate, "__", rr_aldehyde, "__", rr_epoxide))
          }))
        }

        # Create local file so we can reference later
        tmpDir <- paste0("./www/tmp/transaction/")
        
        
        # Get submission timestamp to put in file
        beginTime <- Sys.time()
        
        # Clean up colors to put into local file
        colorsToPrint <- lapply(1:length(colorsAllSets), function(i){
          paste0(names(colorsAllSets)[i], "__", colorsAllSets[i])
        })
        colorsToPrint <- paste0(colorsToPrint, collapse="|")

        
        # Write local info file so we can reference this request later
        file.create(paste0(tmpDir, transactionId))
        tmpLocalFile <- file(paste0(tmpDir, transactionId), open="wb")
        fileContents <- paste0(originalEnrichModeList$originalEnrichMode, "\t", enrichmentType$enrichType, "\t", transactionId, "\t", annoSelectStr, "\t", input$nodeCutoff, "\t", enrichmentSetsSanitizedLocal, "\t", paste0(originalNamesList$originalNames, collapse="|"), "\t", paste0(reenrichResultsSanitized, collapse="|"), "\t", colorsToPrint, "\t", beginTime)
        cat(fileContents, file=tmpLocalFile)
        
        #close(tmpLocalFile)
        
        # Send query to create input file
        resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="createInput", query=list(transactionId=transactionId, enrichmentSets=enrichmentSetsSanitized, setNames=paste0(names(enrichmentSets), collapse="\n"), mode=enrichmentType$enrichType, nodeCutoff=cutoff, annoSelectStr=annoSelectStr))
        
        if(resp$status_code != 200){
          # Hide original form when done with enrichment
          shinyjs::show(id="enrichmentForm")
          
          # Disable changing input type when button is clicked
          shinyjs::enable(id="enrich_from")
          
          # Show 'Restart' button, disable by default so user can't interfere with enrichment process
          shinyjs::hide(id="refresh")
          shinyjs::enable(id="refresh")
          
          # Hide loading spinner
          shinyjs::hide(id="resultsContainer")
          
          # Show error message on main page
          shinyjs::show(id="errorBox")
          
          output$error_box <- renderUI({
            HTML(paste0("<div class=\"text-danger\">Error: Problem creating input files for enrichment.</div>"))
          })
          
          return(FALSE)
        }
        
        resp <- NULL
        
        # Show waiting page
        shinyjs::show(id="waitingPage")
        splitAnnotations <- unlist(str_split(annoSelectStr, "=checked,"))
        splitAnnotationsString <- paste0(splitAnnotations, collapse=", ")
        
        # Cleaned enrichment type strings
        enrichmentDisplayType <- ""
        if(reenrichFlag == TRUE){
          if(originalEnrichModeList$originalEnrichMode == "similarity") {
            enrichmentDisplayType <- "Re-enrich from chemicals with structural similarity" 
          } else if (originalEnrichModeList$originalEnrichMode == "substructure") { # substructure
            enrichmentDisplayType <- "Re-enrich from chemicals with shared substructures"
          } else { #casrn network node update
            enrichmentDisplayType <- "Enrich from user-provided CASRN list"
          }
        } else {
          if(enrichmentType$enrichType == "casrn") {
            enrichmentDisplayType <- "Enrich from user-provided CASRN list"
          } else if(enrichmentType$enrichType == "annotation") {
            enrichmentDisplayType <- "View annotations for Tox21 chemicals"
          } else if(enrichmentType$enrichType == "similarity") {
            enrichmentDisplayType <- "Enrich from chemicals with structural similarity"
          } else { # substructure
            enrichmentDisplayType <- "Enrich from chemicals with shared substructures"
          }
        }

        # Query API to put request in queue
        resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="queue", query=list(mode=enrichmentType$enrichType, enrichmentUUID=transactionId, annoSelectStr=annoSelectStr, nodeCutoff=cutoff))
        
        # if enrichment runs into an error on the API side, cancel and show error on main UI
        if (resp$status_code != 200){
          # Hide original form when done with enrichment
          shinyjs::show(id="enrichmentForm")
          
          # Disable changing input type when button is clicked
          shinyjs::enable(id="enrich_from")
          
          # Show 'Restart' button, disable by default so user can't interfere with enrichment process
          shinyjs::hide(id="refresh")
          shinyjs::enable(id="refresh")
          
          # Hide loading spinner
          shinyjs::hide(id="resultsContainer")
          
          # Show error message on main page
          shinyjs::show(id="errorBox")
          
          output$error_box <- renderUI({
            HTML(paste0("<div class=\"text-danger\">Error: ", content(resp, as="text"), "</div>"))
          })
          
          return(FALSE)
        }
        
        # Get initial queue position
        initQueuePos <- -1
        resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="getQueuePos", query=list(transactionId=transactionId))
        if(resp$status_code != 200) {
          output$results_error_box <- renderUI({
            paste0("<div class=\"text-danger\">Error: Could not fetch queue position for this request.</div>")
          })
        } else {
          initQueuePos <- content(resp)  
        }
        
        waitingData <- data.frame("Position"=c(initQueuePos), "Mode"=c(enrichmentType$enrichType), "UUID"=c(transactionId), "Selected Annotations"=c(splitAnnotationsString), "Node Cutoff"=c(cutoff), "Input"=c(casrnBox), stringsAsFactors=FALSE)
        # Save input data to use later
        inputSetList$inputSet <- waitingData
        # update waitingData with cleaned output
        waitingData <- data.frame("Position"=c(initQueuePos), "Mode"=c(enrichmentDisplayType), "UUID"=c(transactionId), "Selected Annotations"=c(splitAnnotationsString), "Node Cutoff"=c(cutoff), "Input"=c(casrnBox), stringsAsFactors=FALSE)
        
        # Clean up column name formatting
        colnames(waitingData) <- list("Queue Position", "Request Mode", "Request UUID", "Selected Annotations", "Node Cutoff", "User Input")
        output[["waitingTable"]] <- renderUI(
          column(12,
                 DT::datatable({waitingData}, 
                               escape = FALSE,
                               rownames = FALSE,
                               class = "row-border stripe compact",
                               style = "bootstrap",
                               select = "none"
                 )
          )
        )
        
        #TODO: I don't think we need this chunk vvv
        # Query the API to see if we ran into an error
        resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="hasError", query=list(transactionId=transactionId))
        
        # if enrichment runs into an error while performing the enrichment in the queue, cancel and show error on main UI
        if(unlist(content(resp)) != FALSE | resp$status_code != 200){
          
          # Hide original form when done with enrichment
          shinyjs::show(id="enrichmentForm")
          
          # Disable changing input type when button is clicked
          shinyjs::enable(id="enrich_from")
          
          # Show 'Restart' button, disable by default so user can't interfere with enrichment process
          shinyjs::hide(id="refresh")
          shinyjs::enable(id="refresh")
          
          # Hide loading spinner
          shinyjs::hide(id="resultsContainer")
          
          # Show error message on main page
          shinyjs::show(id="errorBox")
          
          output$error_box <- renderUI({
            HTML(paste0("<div class=\"text-danger\">Error: ", errorFile, "</div>"))
          })
          return(FALSE)
        }
        return(TRUE)
    }
    
    # Check if "Results" button was pressed
    observeEvent(input$fetchResults, {
      
      # First, disable other buttons until request finishes
      shinyjs::disable(id="fetchResults")
      shinyjs::disable(id="refreshWaitingPageButton")
      shinyjs::disable(id="clipboard")
      shinyjs::disable(id="cancelEnrichment")
      
      mode <- inputSetList$inputSet[1, "Mode"]
      transactionId <- inputSetList$inputSet[1, "UUID"]
      annoSelectStr <- gsub(", ", "=checked,", inputSetList$inputSet[1, "Selected.Annotations"])
      nodeCutoff <- inputSetList$inputSet[1, "Node.Cutoff"]
      
      # Query the API to see if we ran into an error
      resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="hasError", query=list(transactionId=transactionId))
      
      # if enrichment runs into an error while performing the enrichment in the queue, cancel and show error on main UI
      if(unlist(content(resp)) != FALSE | resp$status_code != 200){
        
        # Hide waiting page
        shinyjs::hide(id="waitingPage")
        
        # Hide original form when done with enrichment
        shinyjs::show(id="enrichmentForm")
        
        # Disable changing input type when button is clicked
        shinyjs::enable(id="enrich_from")
        
        # Re-enable search button
        shinyjs::enable(id="searchButton")
        
        # Show 'Restart' button, disable by default so user can't interfere with enrichment process
        shinyjs::hide(id="refresh")
        shinyjs::enable(id="refresh")
        
        # Hide loading spinner
        shinyjs::hide(id="resultsContainer")
        
        # Show error message on main page
        shinyjs::show(id="errorBox")
        
        # Re-enable buttons
        shinyjs::enable(id="fetchResults")
        shinyjs::enable(id="refreshWaitingPageButton")
        shinyjs::enable(id="clipboard")
        shinyjs::enable(id="cancelEnrichment")
        
        output$error_box <- renderUI({
          HTML(paste0("<div class=\"text-danger\">Error: ", unlist(content(resp)), "</div>"))
        })
        
        return(FALSE)
      }

      # If no errors
      
      # First, check if process is done
      # Query the API to see if process is done
      resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="finishedRequest", query=list(transactionId=transactionId))
      
      # Show warning message if request fails
      if(resp$status_code != 200){
        output$results_error_box <- renderText(paste0("There was an error determining the status of your request. Please try again later."))
      } else {
        checkIfQueueFile <- unlist(content(resp))
        
        if(checkIfQueueFile == FALSE){
          # If it is not, print warning to UI
          output$results_error_box <- renderUI(HTML(paste0("<div class=\"text-danger\">Your request has not completed yet. Please wait for it to complete before accessing results.</div>")))
        } else {
          # Else if done, get results
          enrichmentResults(mode, transactionId, annoSelectStr, nodeCutoff)
        }
      }
      # Re-enable buttons
      shinyjs::enable(id="fetchResults")
      shinyjs::enable(id="refreshWaitingPageButton")
      shinyjs::enable(id="clipboard")
      shinyjs::enable(id="cancelEnrichment")
    })
    
    # Check if "Cancel Enrichment" button was pressed
    observeEvent(input$cancelEnrichment, {
      # Show confirmation box
      showModal(modalDialog(
        HTML(paste0("<p><b>Warning</b>: You are about to cancel this request. This action cannot be undone. Continue?</p>")),
        title="Are you sure you want to cancel this request?",
        footer=tagList(actionButton("cancelConfirm", "Yes, cancel this request."),
                       modalButton("Close"))
      ))
    })
    
    # Check if the confirmation button was pressed for enrichment cancellation
    observeEvent(input$cancelConfirm, {
      # Go back to main page
      
      # Delete from local cache
      transactionId <- reactiveTransactionId$id
      tmpPath <- paste0("./www/tmp/transaction/")
      unlink(paste0(tmpPath, transactionId))
      
      # Remove modal
      removeModal()
      
      # Hide waiting page
      shinyjs::hide(id="waitingPage")
      
      # Hide original form when done with enrichment
      shinyjs::show(id="enrichmentForm")
      
      # Disable changing input type when button is clicked
      shinyjs::enable(id="enrich_from")
      
      # Re-enable search button
      shinyjs::enable(id="searchButton")
      
      # Show 'Restart' button, disable by default so user can't interfere with enrichment process
      shinyjs::hide(id="refresh")
      shinyjs::enable(id="refresh")
      
      # Hide loading spinner
      shinyjs::hide(id="resultsContainer")
      
      # Show error message on main page
      shinyjs::show(id="errorBox")
      
      # Reset results page
      #shinyjs::reset(id="resultsContainer")
      
      output$error_box <- renderUI({
        HTML(paste0("<div class=\"text-danger\">Request cancelled.</div>"))
      })
      
      # Query the API to cancel enrichment
      resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="cancelEnrichment", query=list(transactionId=reactiveTransactionId$id))
      return(FALSE)
    })
    
    
    
    enrichmentResults <- function(mode, transactionId, annoSelectStr, cutoff, enrichmentSets, originalNames, reenrichResults, originalEnrichMode, colorsList){
        # Reset checkboxList$checkboxes
        checkboxList$checkboxes <- NULL

        # Update cache file for completed enrichment
        # Create local file so we can reference later
        tmpDir <- paste0("./www/tmp/transaction/")
        
        # Get ending timestamp to put in file
        resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="getTimestamp", query=list(transactionId=transactionId))
        # TODO: error check
        beginTime <- "not started"
        endTime <- "incomplete"
        if(resp$status_code != 200){
          return(NULL)
        }
        tmpContent <- unlist(content(resp))
        
        if(is.null(tmpContent)) {
          beginTime <- "not started"
          endTime <- "incomplete"
        } else {
          if("timestamp_start" %in% names(tmpContent["timestamp_start"])){
            beginTime <- tmpContent[["timestamp_start"]]
          } else {
            beginTime <- "not started"
          }
          if("timestamp_finish" %in% names(tmpContent["timestamp_finish"])){
            endTime <- tmpContent[["timestamp_finish"]]
          } else {
            endTime <- "incomplete"
          }
        }

        # Write local info file so we can reference this request later
        tmpLocalFile <- file(paste0(tmpDir, transactionId), open="a")
        cat(paste0("\t", beginTime, "\t", endTime, "\r"), file=tmpLocalFile)
        close(tmpLocalFile)
      
        # Show results container
        shinyjs::show(id="resultsContainer")
        # Hide waiting page
        shinyjs::hide(id="waitingPage")
        # Disable View previous results button
        shinyjs::disable(id="searchButton")
        #Show & enable refresh button
        shinyjs::show(id="refresh")
        shinyjs::disable(id="refresh")
      
        # Get enrichmentSets
        if(is.null(enrichmentSetsList$enrichmentSets) == FALSE){
          enrichmentSets <- enrichmentSetsList$enrichmentSets  
        }
        
        # Check which enrichment sets are good
        resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="checkSets", query=list(transactionId=transactionId))
        #TODO: error check
        if(resp$status_code != 200){
          return(NULL)
        }
        
        validatedSets <- unlist(content(resp))
        enrichmentSets <- unlist(lapply(names(enrichmentSets), function(x){
          if(x %in% validatedSets){
            return(enrichmentSets[x])
          } 
          return(NULL)
        }), recursive=FALSE)
        enrichmentSets <- enrichmentSets[!sapply(enrichmentSets, is.null)]
        
        #Get original names
        if(is.null(originalNamesList$originalNames) == FALSE){
          originalNames <- originalNamesList$originalNames  
        }
        
        # Get reenrichResults
        if(is.null(reenrichResultsList$reenrichResults) == FALSE){
          reenrichResults <- reenrichResultsList$reenrichResults 
        }
        
        # Get original enrich mode
        if(is.null(originalEnrichModeList$originalEnrichMode) == FALSE){
          originalEnrichMode <- originalEnrichModeList$originalEnrichMode
        }
        
        # Get colors
        if(is.null(setColors$color) == FALSE){
          colorsList <- setColors$color
        } else {
          setColors$color <- colorsList
        }

        # Get directories
        inDirWeb <- paste0("Input/", transactionId, "/")
        outDirWeb <- paste0("Output/", transactionId, "/")

        # If success at this point, hide waiting page
        shinyjs::hide(id="waitingPage")
        
        # Initialize if we have warnings
        haveWarnings <- reactiveValues(warnings = FALSE)
        
        # Render results page
        if(mode == "annotation") {
          output$resultsTabset <- renderUI({
            fluidRow(
              column(12,
                     h1(id="resultsHeader", "Fetched Annotations"),
                     do.call(tabsetPanel, c(id='tab', lapply(names(enrichmentSets), function(i) {
                       outputOptions(output, paste0("outTab_", i), suspendWhenHidden=FALSE)
                       tabPanel(
                         title=paste0(i),
                         uiOutput(paste0("outTab_", i)) %>% withSpinner()
                       )
                     })))
              ),
              column(12,
                     actionButton(inputId="downloadButton", label="Download results", icon=icon("download"))       
              )
            )
          })
          
          # Fetch setFiles from server via API
          resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="getResults", query=list(transactionId=transactionId))
          # TODO: error check
          if(resp$status_code != 200){
            return(NULL)
          }
          setFiles <- unlist(content(resp))
          setFilesSplit <- lapply(setFiles, function(setFile){
            tmpSplit <- unlist(str_split(setFile, "/"))
            return(tmpSplit[length(tmpSplit)])
          })
          setFilesSplit <- unlist(setFilesSplit, recursive=FALSE)
        
          # Event listener for per set results onclick
          lapply(1:length(setFiles), function(setFile){
            # If not the FullMatrix file or the zipped results archive, create observer for individual CASRN file links
            if( !grepl(paste0("__FullMatrix.txt"), setFilesSplit[setFile], fixed=TRUE) & !grepl(paste0(".zip"), setFilesSplit[setFile], fixed=TRUE)){
              if(is.null(setFilesObservers$observers[[paste0("casrn", setFilesSplit[setFile], "Observer__", setFile)]])){
                setFilesObservers$observers[[paste0("casrn", setFilesSplit[setFile], "Observer__", setFile)]] <- observeEvent(input[[paste0(setFilesSplit[setFile], "__link")]], {
                  # Create directory for request
                  dir.create(paste0("./www/tmp/output/", transactionId, "/"))
                  if(file.exists(paste0("./www/tmp/output/", transactionId, "/", setFilesSplit[setFile])) == FALSE){
                    # TODO: error check if download fails
                    download.file(paste0("http://", API_HOST, ":", API_PORT, "/serveFileText?transactionId=", transactionId, "&filename=", setFilesSplit[setFile], "&subDir=Output"), destfile=paste0("./www/tmp/output/", transactionId, "/", setFilesSplit[setFile]))
                  }
                  js$browseURL(paste0("tmp/output/", transactionId, "/", setFilesSplit[setFile]))
                }, ignoreInit = TRUE)
              }
            }
          })
          
          lapply(names(enrichmentSets), function(i) {
            # Full matrix
            if(is.null(setFilesObservers$observers[[paste0("fullMatrixObserver__", i)]])){
              setFilesObservers$observers[[paste0("fullMatrixObserver__", i)]] <- observeEvent(input[[paste0(i, "__FullMatrix.txt__link")]], {
                # Create directory for request
                dir.create(paste0("./www/tmp/output/", transactionId, "/"))
                if(file.exists(paste0("./www/tmp/output/", transactionId, "/", i, "__FullMatrix.txt")) == FALSE){
                  # TODO: error check if download fails
                  download.file(paste0("http://", API_HOST, ":", API_PORT, "/serveFileText?transactionId=", transactionId, "&filename=", i, "__FullMatrix.txt&subDir=Output"), destfile=paste0("./www/tmp/output/", transactionId, "/", i, "__FullMatrix.txt"))
                }
                js$browseURL(paste0("tmp/output/", transactionId, "/", i, "__FullMatrix.txt"))
              }, ignoreInit = TRUE)
            }
            
            output[[paste0("outTab_", i)]] <- renderUI(
              column(12,
                tabPanel(paste0("tab_", i), 
                  lapply(1:length(setFiles), function(setFile){
                    
                    # Create item in tab for matching sets
                    if (grepl(paste0(i, "[.]*"), setFilesSplit[setFile])) {
                      fluidRow(
                        column(id=paste0(setFilesSplit[setFile]), 3, 
                          if(unlist(str_split(setFilesSplit[setFile], "__"))[2] == "FullMatrix.txt"){
                            tipify( div(actionLink(inputId=paste0(i, "__FullMatrix.txt__link"), label=setFilesSplit[setFile])), "A matrix displaying all of the annotations and their corresponding chemicals for this set (.txt format).", placement="bottom")
                          }
                          else { # CASRN file
                            tipify( div(actionLink(inputId=paste0(setFilesSplit[setFile], "__link"), label=setFilesSplit[setFile])), "A list of annotations in the Tox21 Enricher database for this chemical with respect to the given annotation classes (.txt format).", placement="bottom")
                          }
                        )
                      )
                    }
                  })
                )
              )
            )
          })

          # Event handler for download full results
          if(is.null(setFilesObservers$observers[[paste0("downloadObserver__")]])){
            setFilesObservers$observers[[paste0("downloadObserver__")]] <- observeEvent(input[["downloadButton"]], {
              # Create directory for request
              dir.create(paste0("./www/tmp/output/", transactionId, "/"))
              if(file.exists(paste0("./www/tmp/output/", transactionId, "/tox21enricher_", transactionId, ".zip")) == FALSE){
                # TODO: error check if download fails
                download.file(paste0("http://", API_HOST, ":", API_PORT, "/serveFileText?transactionId=", transactionId, "&filename=tox21enricher_", transactionId, ".zip&subDir=Output"), destfile=paste0("./www/tmp/output/", transactionId, "/tox21enricher_", transactionId, ".zip"))
              }
              # Next, use custom JavaScript to open manual from disk in new tab
              js$browseURL(paste0("tmp/output/", transactionId, "/tox21enricher_", transactionId, ".zip"))
            }, ignoreInit = TRUE)
          }
        
        } else { # Render enrichment results
          
          # Download button
          # Event handler for download full results
          
          if(is.null(setFilesObservers$observers[[paste0("downloadObserver__")]])){
            setFilesObservers$observers[[paste0("downloadObserver__")]] <- observeEvent(input[["downloadButton"]], {
              # Create directory for request
              dir.create(paste0("./www/tmp/output/", transactionId, "/"))
              if(file.exists(paste0("./www/tmp/output/", transactionId, "/tox21enricher_", transactionId, ".zip")) == FALSE){
                # TODO: error check if download fails
                download.file(paste0("http://", API_HOST, ":", API_PORT, "/serveFileText?transactionId=", transactionId, "&filename=tox21enricher_", transactionId, ".zip&subDir=Output"), destfile=paste0("./www/tmp/output/", transactionId, "/tox21enricher_", transactionId, ".zip"))
              }
              # Next, use custom JavaScript to open manual from disk in new tab
              js$browseURL(paste0("tmp/output/", transactionId, "/tox21enricher_", transactionId, ".zip"))
            }, ignoreInit = TRUE)
          }
          
          # Choice names for radio buttons
          radioNames <- lapply(names(enrichmentSets), function(setName){
            return( HTML(paste0("<div style='width:100%;height:20px;background-color:", colorsList[setName], "'>",  setName, "</div>")) )
          })
          
          output$resultsTabset <- renderUI({
              fluidRow(
                  fluidRow(
                      column(12,
                          h1(id="resultsHeader", "Enrichment Results"),
                          do.call(tabsetPanel, c(id='tab', lapply(names(enrichmentSets), function(i) {
                            outputOptions(output, paste0("outTab_", i), suspendWhenHidden=FALSE)
                            tabPanel(
                              title=paste0(i),
                              value=paste0(i),
                              uiOutput(paste0("outTab_", i))
                            )
                          })))
                      )
                  ),
                  fluidRow(
                    column(12,
                      # Download results button
                      actionButton(inputId="downloadButton", label="Download results", icon=icon("download"))
                    )
                  ),
                  fluidRow(
                    column(12, 
                      uiOutput("reenrichCutoff")
                    )
                  ),
                  fluidRow(
                      column(12, 
                        uiOutput("reenrichButtonOut")
                      )
                  ),
                  fluidRow(
                    column(12,
                      h3("Chart Full Heatmap"),
                      uiOutput("chartHeatmap")
                    )
                  ),
                  fluidRow(
                    column(12,
                      h3("Cluster Heatmap"),
                      uiOutput("clusterHeatmap")
                    )
                  ),
                  fluidRow(
                    column(12,
                      h3("Significant P-value per Annotation Category"),
                      column(2,
                        radioButtons(inputId="radioBargraph", label="Order P-values with respect to input set", choiceNames=radioNames, choiceValues=names(enrichmentSets), selected=names(enrichmentSets)[1]),
                        actionButton(inputId="updateBargraphButton", label="Update plot")
                      ),
                      column(10,
                        uiOutput("bargraph") %>% withSpinner()
                      )
                    )
                  ),
                  
              )
          })

          lapply(names(enrichmentSets), function(i) {
                  # Fetch setFiles from server via API
                  resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="readGct", query=list(transactionId=transactionId, cutoff=cutoff, mode="set", set=i))
                  #TODO: error check
                  if(resp$status_code != 200){
                    return(NULL)
                  }
                  resp2 <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="getResults", query=list(transactionId=transactionId, setName=i))

                  # TODO: Clean this up!
                  gctFile <- sapply(content(resp), function(x){
                    gctFileInner <- unlist(sapply(names(x), function(y) {
                      if(y != "_row"){
                        return(as.double(x[y]))  
                      } else {
                        return(NULL)
                      }
                    }))
                    gctFileInner <- gctFileInner[!sapply(gctFileInner, is.null)]
                    
                    dfToReturn <- t(data.frame(gctFileInner, stringsAsFactors=FALSE))
                    
                    row.names(dfToReturn) <- x["_row"]
                    return(dfToReturn)
                  })
                  gctFileColNames <- unique(unlist(lapply(content(resp), function(x){
                    gctFileInner <- unlist(lapply(names(x), function(y) {
                      if(y != "_row"){
                        return(y)
                      } else {
                        return(NULL)
                      }
                    }))
                    gctFileInner <- gctFileInner[!sapply(gctFileInner, is.null)]
                    return(gctFileInner)
                  })))
                  gctFileRowNames <- unique(unlist(lapply(content(resp), function(x){
                    gctFileInner <- unlist(lapply(names(x), function(y) {
                      if(y != "_row"){
                        return(NULL)
                      } else {
                        return(x[y])
                      }
                    }))
                    gctFileInner <- gctFileInner[!sapply(gctFileInner, is.null)]
                    return(gctFileInner)
                  })))
                  # coerce to data frame so we can name cols and rows
                  gctFile <- as.data.frame(gctFile, stringsAsFactors = FALSE)
                  
                  # only translate if more than 1 row
                  if(ncol(gctFile) > 1){
                    gctFile <- t(gctFile)
                  }
                  colnames(gctFile) <- gctFileColNames
                  row.names(gctFile) <- gctFileRowNames
                  
                  if(is.null(gctFile) == FALSE){
                    gctFileMatrix <- data.matrix(gctFile)
                    gctCASRNNames <- rownames(gctFile)
                    gctAnnoNames <- colnames(gctFile)
                    
                    # Get list of paths to result files for each set
                    #TODO: Error check this
                    # Fetch setFiles from server via API
                    resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="getResults", query=list(transactionId=transactionId, setName=i))
                    #TODO: error check
                    if(resp$status_code != 200){
                      return(NULL)
                    }
                    
                    getAllResultFiles <- unlist(content(resp))
                    
                    # Add input file from input directory
                    getAllResultFiles <- append(getAllResultFiles, paste0(inDirWeb, i, ".txt"))
                    
                    originalInputToDisplay <- ""
                    for(j in originalNames){
                      originalSetName <- unlist(str_split(j, "__"))
                      if(originalSetName[2] == i){
                        originalInputToDisplay <- paste0("Input chemical: ", originalSetName[1])
                      }
                    }
                    
                    # Dynamically create observers for result file links
                    # Input
                    if(is.null(setFilesObservers$observers[[paste0("inputObserver__", i)]])){
                      setFilesObservers$observers[[paste0("inputObserver__", i)]] <- observeEvent(input[[paste0(i, "__", i, ".txt__link")]], {
                        # Create directory for request
                        dir.create(paste0("./www/tmp/input/", transactionId, "/"))
                        if(file.exists(paste0("./www/tmp/input/", transactionId, "/", i, ".txt")) == FALSE){
                          # TODO: error check if download fails
                          download.file(paste0("http://", API_HOST, ":", API_PORT, "/serveFileText?transactionId=", transactionId, "&filename=", i, ".txt&subDir=Input"), destfile=paste0("./www/tmp/input/", transactionId, "/", i, ".txt"))
                        }
                        # Next, use custom JavaScript to open manual from disk in new tab
                        js$browseURL(paste0("tmp/input/", transactionId, "/", i, ".txt"))
                      }, ignoreInit = TRUE)
                    }
                    
                    # Chart.txt
                    if(is.null(setFilesObservers$observers[[paste0("chartTxtObserver__", i)]])){
                      setFilesObservers$observers[[paste0("chartTxtObserver__", i)]] <- observeEvent(input[[paste0(i, "__Chart.txt__link")]], {
                        # Create directory for request
                        dir.create(paste0("./www/tmp/output/", transactionId, "/"))
                        if(file.exists(paste0("./www/tmp/output/", transactionId, "/", i, "__Chart.txt")) == FALSE){
                          # TODO: error check if download fails
                          download.file(paste0("http://", API_HOST, ":", API_PORT, "/serveFileText?transactionId=", transactionId, "&filename=", i, "__Chart.txt&subDir=Output"), destfile=paste0("./www/tmp/output/", transactionId, "/", i, "__Chart.txt"))
                        }
                        # Next, use custom JavaScript to open manual from disk in new tab
                        js$browseURL(paste0("tmp/output/", transactionId, "/", i, "__Chart.txt"))
                      }, ignoreInit = TRUE)
                    }

                    # Chart.xlsx
                    if(is.null(setFilesObservers$observers[[paste0("chartXlsxObserver__", i)]])){
                      setFilesObservers$observers[[paste0("chartXlsxObserver__", i)]] <- observeEvent(input[[paste0(i, "__Chart.xlsx__link")]], {
                        # Create directory for request
                        dir.create(paste0("./www/tmp/output/", transactionId, "/"))
                        if(file.exists(paste0("./www/tmp/output/", transactionId, "/", i, "__Chart.xlsx")) == FALSE){
                          # TODO: error check if download fails
                          download.file(paste0("http://", API_HOST, ":", API_PORT, "/serveFileText?transactionId=", transactionId, "&filename=", i, "__Chart.xlsx&subDir=Output"), destfile=paste0("./www/tmp/output/", transactionId, "/", i, "__Chart.xlsx"))
                        }
                        # Next, use custom JavaScript to open manual from disk in new tab
                        js$browseURL(paste0("tmp/output/", transactionId, "/", i, "__Chart.xlsx"))
                      }, ignoreInit = TRUE)
                    }
                    
                    # ChartSimple.txt
                    if(is.null(setFilesObservers$observers[[paste0("chartSimpleTxtObserver__", i)]])){
                      setFilesObservers$observers[[paste0("chartSimpleTxtObserver__", i)]] <- observeEvent(input[[paste0(i, "__ChartSimple.txt__link")]], {
                        # Create directory for request
                        dir.create(paste0("./www/tmp/output/", transactionId, "/"))
                        if(file.exists(paste0("./www/tmp/output/", transactionId, "/", i, "__ChartSimple.txt")) == FALSE){
                          # TODO: error check if download fails
                          download.file(paste0("http://", API_HOST, ":", API_PORT, "/serveFileText?transactionId=", transactionId, "&filename=", i, "__ChartSimple.txt&subDir=Output"), destfile=paste0("./www/tmp/output/", transactionId, "/", i, "__ChartSimple.txt"))
                        }
                        # Next, use custom JavaScript to open manual from disk in new tab
                        js$browseURL(paste0("tmp/output/", transactionId, "/", i, "__ChartSimple.txt"))
                      }, ignoreInit = TRUE)
                    }
                    
                    # ChartSimple.xlsx
                    if(is.null(setFilesObservers$observers[[paste0("chartSimpleXlsxObserver__", i)]])){
                      setFilesObservers$observers[[paste0("chartSimpleXlsxObserver__", i)]] <- observeEvent(input[[paste0(i, "__ChartSimple.xlsx__link")]], {
                        # Create directory for request
                        dir.create(paste0("./www/tmp/output/", transactionId, "/"))
                        if(file.exists(paste0("./www/tmp/output/", transactionId, "/", i, "__ChartSimple.xlsx")) == FALSE){
                          # TODO: error check if download fails
                          download.file(paste0("http://", API_HOST, ":", API_PORT, "/serveFileText?transactionId=", transactionId, "&filename=", i, "__ChartSimple.xlsx&subDir=Output"), destfile=paste0("./www/tmp/output/", transactionId, "/", i, "__ChartSimple.xlsx"))
                        }
                        # Next, use custom JavaScript to open manual from disk in new tab
                        js$browseURL(paste0("tmp/output/", transactionId, "/", i, "__ChartSimple.xlsx"))
                      }, ignoreInit = TRUE)
                    }
                    
                    # Cluster.txt
                    if(is.null(setFilesObservers$observers[[paste0("clusterTxtObserver__", i)]])){
                      setFilesObservers$observers[[paste0("clusterTxtObserver__", i)]] <- observeEvent(input[[paste0(i, "__Cluster.txt__link")]], {
                        # Create directory for request
                        dir.create(paste0("./www/tmp/output/", transactionId, "/"))
                        if(file.exists(paste0("./www/tmp/output/", transactionId, "/", i, "__Cluster.txt")) == FALSE){
                          # TODO: error check if download fails
                          download.file(paste0("http://", API_HOST, ":", API_PORT, "/serveFileText?transactionId=", transactionId, "&filename=", i, "__Cluster.txt&subDir=Output"), destfile=paste0("./www/tmp/output/", transactionId, "/", i, "__Cluster.txt"))
                        }
                        # Next, use custom JavaScript to open manual from disk in new tab
                        js$browseURL(paste0("tmp/output/", transactionId, "/", i, "__Cluster.txt"))
                      }, ignoreInit = TRUE)
                    }
                    
                    # Cluster.xlsx
                    if(is.null(setFilesObservers$observers[[paste0("clusterXlsxObserver__", i)]])){
                      setFilesObservers$observers[[paste0("clusterXlsxObserver__", i)]] <- observeEvent(input[[paste0(i, "__Cluster.xlsx__link")]], {
                        # Create directory for request
                        dir.create(paste0("./www/tmp/output/", transactionId, "/"))
                        if(file.exists(paste0("./www/tmp/output/", transactionId, "/", i, "__Cluster.xlsx")) == FALSE){
                          # TODO: error check if download fails
                          download.file(paste0("http://", API_HOST, ":", API_PORT, "/serveFileText?transactionId=", transactionId, "&filename=", i, "__Cluster.xlsx&subDir=Output"), destfile=paste0("./www/tmp/output/", transactionId, "/", i, "__Cluster.xlsx"))
                        }
                        # Next, use custom JavaScript to open manual from disk in new tab
                        js$browseURL(paste0("tmp/output/", transactionId, "/", i, "__Cluster.xlsx"))
                      }, ignoreInit = TRUE)
                    }
                    
                    # Matrix.txt
                    if(is.null(setFilesObservers$observers[[paste0("matrixObserver__", i)]])){
                      setFilesObservers$observers[[paste0("matrixObserver__", i)]] <- observeEvent(input[[paste0(i, "__Matrix.txt__link")]], {
                        # Create directory for request
                        dir.create(paste0("./www/tmp/output/", transactionId, "/"))
                        if(file.exists(paste0("./www/tmp/output/", transactionId, "/", i, "__Matrix.txt")) == FALSE){
                          # TODO: error check if download fails
                          download.file(paste0("http://", API_HOST, ":", API_PORT, "/serveFileText?transactionId=", transactionId, "&filename=", i, "__Matrix.txt&subDir=Output"), destfile=paste0("./www/tmp/output/", transactionId, "/", i, "__Matrix.txt"))
                        }
                        # Next, use custom JavaScript to open manual from disk in new tab
                        js$browseURL(paste0("tmp/output/", transactionId, "/", i, "__Matrix.txt"))
                      }, ignoreInit = TRUE)
                    }
                    
                    # Render result file links
                    output[[paste0("outTab_", i)]] <- renderUI(
                        column(12,
                            tabPanel(paste0("tab_", i), 
                              # display original input chemical if SMILES or InChI
                              if(mode == "similarity" | mode == "substructure" | originalEnrichMode == "similarity" | originalEnrichMode == "substructure"){
                                fluidRow(
                                  p(originalInputToDisplay)
                                )
                              },
                              fluidRow(
                                lapply(getAllResultFiles, function(resultFile){
                                    # If input file
                                    tooltipToUse <- ""
                                    if(resultFile == paste0(inDirWeb, i, ".txt")) {
                                      column(4, id=paste0(unlist(str_split(resultFile, inDirWeb))[2]),
                                        # Add tooltips
                                        # Input
                                        if(paste0(unlist(str_split(resultFile, inDirWeb))[2]) == paste0(i, ".txt")){
                                          tipify( div(actionLink(inputId=paste0(i, "__", i, ".txt__link"), label=paste0(unlist(str_split(unlist(str_split(resultFile, inDirWeb))[2], ".txt"))[1], " Input"))), "A list of input chemicals for this set (.txt format).", placement="bottom")
                                        }
                                      )
                                    } else {
                                      column(4, id=paste0(unlist(str_split(resultFile, outDirWeb))[2]), 
                                        # Add links to result files with corresponding tooltips
                                        # Chart.txt
                                        if(paste0(unlist(str_split(resultFile, outDirWeb))[2]) == paste0(i, "__Chart.txt")){
                                          tipify( div(actionLink(inputId=paste0(i, "__Chart.txt__link"), label=paste0(unlist(str_split(resultFile, outDirWeb))[2]))), "A list of all significant annotations (.txt format).", placement="bottom")
                                        },
                                        # Chart.xlsx
                                        if(paste0(unlist(str_split(resultFile, outDirWeb))[2]) == paste0(i, "__Chart.xlsx")){
                                          tipify( div(actionLink(inputId=paste0(i, "__Chart.xlsx__link"), label=paste0(unlist(str_split(resultFile, outDirWeb))[2]))), "A list of all significant annotations (.xlsx format).", placement="bottom")
                                        },
                                        # ChartSimple.txt
                                        if(paste0(unlist(str_split(resultFile, outDirWeb))[2]) == paste0(i, "__ChartSimple.txt")){
                                          tipify( div(actionLink(inputId=paste0(i, "__ChartSimple.txt__link"), label=paste0(unlist(str_split(resultFile, outDirWeb))[2]))), "A list of the top 10 most significant annotations for each annotation class (.txt format).", placement="bottom")
                                        },
                                        # ChartSimple.xlsx
                                        if(paste0(unlist(str_split(resultFile, outDirWeb))[2]) == paste0(i, "__ChartSimple.xlsx")){
                                          tipify( div(actionLink(inputId=paste0(i, "__ChartSimple.xlsx__link"), label=paste0(unlist(str_split(resultFile, outDirWeb))[2]))), "A list of the top 10 most significant annotations for each annotation class (.xlsx format).", placement="bottom")
                                        },
                                        # Cluster.txt
                                        if(paste0(unlist(str_split(resultFile, outDirWeb))[2]) == paste0(i, "__Cluster.txt")){
                                          tipify( div(actionLink(inputId=paste0(i, "__Cluster.txt__link"), label=paste0(unlist(str_split(resultFile, outDirWeb))[2]))), "A list of significant terms in which functionally similar annotations are grouped together to remove redundancy. This is performed with respect to the whole annotation set rather than to individual annotation classes (.txt format).", placement="bottom")
                                        },
                                        # Cluster.xlsx
                                        if(paste0(unlist(str_split(resultFile, outDirWeb))[2]) == paste0(i, "__Cluster.xlsx")){
                                          tipify( div(actionLink(inputId=paste0(i, "__Cluster.xlsx__link"), label=paste0(unlist(str_split(resultFile, outDirWeb))[2]))), "A list of significant terms in which functionally similar annotations are grouped together to remove redundancy. This is performed with respect to the whole annotation set rather than to individual annotation classes (.xlsx format).", placement="bottom")
                                        },
                                        # Matrix
                                        if(paste0(unlist(str_split(resultFile, outDirWeb))[2]) == paste0(i, "__Matrix.txt")){
                                          tipify( div(actionLink(inputId=paste0(i, "__Matrix.txt__link"), label=paste0(unlist(str_split(resultFile, outDirWeb))[2]))), "A text representation of the heatmap (.txt format).", placement="bottom")
                                        },

                                      )
                                    }
                                })  
                              )
                            ),
                            # Main heatmap per set
                            fluidRow(
                              plot_ly(x = gctAnnoNames, y = gctCASRNNames, z = gctFileMatrix, colors = colorRamp(c("white", "red")), type="heatmap", xgap=2, ygap=2 ) %>% 
                                layout(
                                  autosize=TRUE, 
                                  showlegend=FALSE, 
                                  margin = list(r=200, b=200), 
                                  xaxis=list(title="<b>Annotations</b>", tickfont=list(size=9), tickangle=15, automargin=TRUE), 
                                  yaxis=list(title="<b>Input CASRNs</b>", type="category", automargin=TRUE),
                                  plot_bgcolor="transparent",
                                  paper_bgcolor="transparent",
                                  font=list(color=theme$textcolor)
                                ) %>% hide_colorbar()
                            ),
                            hr(),
                            fluidRow(
                              if(mode != "casrn" | (originalEnrichMode == "substructure" | originalEnrichMode == "similarity")) {
                                h3("Result Chemicals")
                              },
                              if(mode != "casrn" | (originalEnrichMode == "substructure" | originalEnrichMode == "similarity")) {
                                uiOutput(paste0("table_", i))
                              }
                            )
                        )
                    )
                  } else {
                    output[[paste0("outTab_", i)]] <- renderUI(
                      column(12,
                        tabPanel(paste0("tab_", i), 
                          fluidRow(p(paste0("No result files for set: ", i)))
                        )
                      )
                    )
                  }
              }
          )

          # Render reenrichment if Substructure or Similarity
          #if (mode != "casrn") {
              reenrichResultsTotalLength <- length(unlist(lapply(names(reenrichResults), function(i){
                return(unlist(reenrichResults[[i]][,"casrn"]))
              })))
              
              for (i in names(reenrichResults)) {
                  reenrichResultsTotalLength <- reenrichResultsTotalLength + nrow(reenrichResults[[i]])
              }
              
              # Checkbox input to select chemicals for re-enrichment
              reenrichChoices <- lapply(names(reenrichResults), function(i){
                return(reenrichResults[[i]][, "casrn"])
              })
              names(reenrichChoices) <- names(reenrichResults)
              
              checkboxes <- lapply(names(reenrichChoices), function(reenrichSet){
                tmp_checkboxes <- lapply(reenrichChoices[[reenrichSet]], function(x){
                  return(checkboxInput(inputId=paste0(x, "__", reenrichSet), value=TRUE, label=NULL, width="4px"))
                })
                names(tmp_checkboxes) <- lapply(reenrichChoices[[reenrichSet]], function(x){
                  return(paste0(x, "__", reenrichSet))
                })
                return(tmp_checkboxes)
              })
              names(checkboxes) <- names(reenrichChoices)

              # Set reactive value so we can access these checkboxes later
              checkboxList$checkboxes <- checkboxes
              
              # Initialize empty list of svg images
              svgImagesList <- list()
              
              if (originalEnrichMode != "casrn") {
                lapply(names(reenrichResults),function(i) {
                  # Get chemical structure images and add to table
                  imgPath1 <- '<img src="images/structures/'
                  imgPath2 <- ' height="100" width="100"></img>'
  
                  # query API to generate images
                  resultImagesSVG <- unlist(lapply(1:nrow(reenrichResults[[i]]["casrn"]), function(x) {
                    return(paste0(reenrichResults[[i]][x, "casrn"], "__", reenrichResults[[i]][x, "m"]))
                  }))
                  resultImagesSVG <- paste0(resultImagesSVG, collapse="\n")
                  resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="generateStructures", query=list(input=resultImagesSVG))
                  structuresSvg <- content(resp)
                  resultImages <- lapply(reenrichResults[[i]][, "casrn"], function(casrnName) {
                    # Note: this is kind of a hacky way to resize the SVG images generated by rdkit, but it works...
                    resizedSvg <- str_replace(unlist(structuresSvg[casrnName][[1]]) , "width='250px'", "width='50px'")
                    resizedSvg <- str_replace(resizedSvg , "height='200px'", "height='40px'")
                    
                    # Add fetched, resized SVG to list too
                    svgImagesList[paste0(casrnName, "__", i)] <<- resizedSvg
                    
                    # Add tooltip saying CASRN on hover
                    return(HTML(paste0(
                      tags$div(  
                        tipify(
                          actionButton(
                            inputId=paste0("ab__img__", casrnName, "__", i), 
                            label=HTML(paste0(resizedSvg))
                          ),
                          title=paste0("Chemical structure for ", casrnName, ". Click to expand image with more info."),
                          placement="bottom"
                        )
                      , id=paste0("div__ab__img__", casrnName, "__", i))
                    )))
                  })
                  
                  # Get additional information for each CASRN from database and add to table
                  expandedInfo <- t(sapply(reenrichResults[[i]][,"casrn"], function(casrn){
                    resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="casrnData", query=list(input=casrn))
                    if(resp$status_code != 200){
                      return(NULL)
                    }
                    
                    # Dynamically create matching observers for zoomed in views of chemical structures
                    chemDetail <- unlist(content(resp), recursive=FALSE)
                    outputTestsubstanceChemname <- chemDetail[["testsubstance_chemname"]]
                    outputFormula <- chemDetail[["mol_formula"]]
                    outputIupac <- chemDetail[["iupac_name"]]
                    outputInchi <- chemDetail[["inchis"]]
                    outputInchikey <- chemDetail[["inchikey"]]
                    outputSmiles <- chemDetail[["smiles"]]
                    outputDtxsid <- chemDetail[["dtxsid"]]
                    outputDtxrid <- chemDetail[["dtxrid"]]
                    outputWeight <- chemDetail[["mol_weight"]]
                    outputCid <- chemDetail[["cid"]]
                    
                    # Resize structural image
                    svgExpanded <- paste0(svgImagesList[paste0(casrn, "__", i)])
                    svgExpanded <- str_replace(svgExpanded , "width='50px'", "width='600px'")
                    svgExpanded <- str_replace(svgExpanded , "height='40px'", "height='480px'")

                    # Create observer
                    if(is.null(setFilesObservers$observers[[paste0("svgObserver__", casrn, "__", i)]])){
                      setFilesObservers$observers[[paste0("svgObserver__", casrn, "__", i)]] <- observeEvent(input[[paste0("ab__img__", casrn, "__", i)]], {
                        showModal(
                          modalDialog(
                            title=casrn,
                            footer=modalButton("Close"),
                            size="l",
                            fluidRow(
                              column(12, HTML(paste0(svgExpanded)))
                            ),
                            fluidRow(
                              column(3, HTML("<b>DTXSID</b>")),
                              column(9, HTML(outputDtxsid))
                            ),
                            fluidRow(
                              column(3, HTML("<b>DTXRID</b>")),
                              column(9, HTML(outputDtxrid))
                            ),
                            fluidRow(
                              column(3, HTML("<b>Chemical Name</b>")),
                              column(9, HTML(outputTestsubstanceChemname))
                            ),
                            fluidRow(
                              column(3, HTML("<b>IUPAC Name</b>")),
                              column(9, HTML(outputIupac))
                            ),
                            fluidRow(
                              column(3, HTML("<b>CASRN</b>")),
                              column(9, HTML(casrn))
                            ),
                            fluidRow(
                              column(3, HTML("<b>SMILES</b>")),
                              column(9, HTML(outputSmiles))
                            ),
                            fluidRow(
                              column(3, HTML("<b>InChI</b>")),
                              column(9, HTML(outputInchi))
                            ),
                            fluidRow(
                              column(3, HTML("<b>InChI Key</b>")),
                              column(9, HTML(outputInchikey))
                            ),
                            fluidRow(
                              column(3, HTML("<b>Molecular Formula</b>")),
                              column(9, HTML(outputFormula))
                            ),
                            fluidRow(
                              column(3, HTML("<b>Molecular Weight</b>")),
                              column(9, HTML(outputWeight))
                            ),
                            fluidRow(
                              column(3, HTML(paste0("<a href=\"https://comptox.epa.gov/dashboard/dsstoxdb/results?search=", outputDtxsid, "&abbreviation=TOX21SL\">View at EPA</a>"))),
                              column(9, HTML(paste0("<a href=\"https://pubchem.ncbi.nlm.nih.gov/compound/", outputCid, "\">View at PubChem</a>")))
                            )
                          )
                        )
                      }, ignoreInit=TRUE)
                    }
                    infoOutp <- data.frame("iupac_name"=outputIupac, "smiles"=outputSmiles, "dtxsid"=outputDtxsid, "dtxrid"=outputDtxrid, "mol_formula"=outputFormula, "mol_weight"=outputWeight, "inchis"=outputInchi, "inchikey"=outputInchikey, stringsAsFactors=FALSE)
                    return(infoOutp)
                  }))
                  expandedInfo <- data.frame(expandedInfo)
                  row.names(expandedInfo) <- t(sapply(reenrichResults[[i]][,"casrn"], function(casrn) casrn))
                  
                  # Create final data frame
                  fullTableTmp <- t(sapply(row.names(expandedInfo), function(casrn){
                    for (row in 1:nrow(reenrichResults[[i]])){
                      if(reenrichResults[[i]][row,"casrn"] == casrn){
                        finalTable <- NULL
                        if (mode == "similarity" | originalEnrichMode == "similarity") {
                          finalTable <- data.frame(
                            "Chemical_Structure"  = resultImages[row], 
                            "DTXSID"              = expandedInfo[casrn,"dtxsid"], 
                            "CASRN"               = reenrichResults[[i]][row,"casrn"], 
                            "IUPAC_Name"          = expandedInfo[casrn,"iupac_name"], 
                            "SMILES"              = expandedInfo[casrn,"smiles"], 
                            "InChI"               = expandedInfo[casrn,"inchis"], 
                            "InChIKey"            = expandedInfo[casrn,"inchikey"],
                            "Molecular_Formula"   = expandedInfo[casrn,"mol_formula"],
                            "Molecular_Weight"    = expandedInfo[casrn,"mol_weight"],
                            "Similarity"          = round(as.numeric(reenrichResults[[i]][row,"similarity"]), digits=2), # Truncate similarity to hundredths place (easier to read)
                            "Cyanide"             = reenrichResults[[i]][row,"cyanide"],
                            "Isocyanate"          = reenrichResults[[i]][row,"isocyanate"],
                            "Aldehyde"            = reenrichResults[[i]][row,"aldehyde"],
                            "Epoxide"             = reenrichResults[[i]][row,"epoxide"],
                            stringsAsFactors=FALSE
                          )
                        } else if(mode == "substructure" | originalEnrichMode == "substructure") { # Substructure (no similarity column)
                          finalTable <- data.frame(
                            "Chemical_Structure"  = resultImages[row], 
                            "DTXSID"              = expandedInfo[casrn,"dtxsid"], 
                            "CASRN"               = reenrichResults[[i]][row,"casrn"], 
                            "IUPAC_Name"          = expandedInfo[casrn,"iupac_name"], 
                            "SMILES"              = expandedInfo[casrn,"smiles"], 
                            "InChI"               = expandedInfo[casrn,"inchis"], 
                            "InChIKey"            = expandedInfo[casrn,"inchikey"],
                            "Molecular_Formula"   = expandedInfo[casrn,"mol_formula"],
                            "Molecular_Weight"    = expandedInfo[casrn,"mol_weight"],
                            "Cyanide"             = reenrichResults[[i]][row,"cyanide"],
                            "Isocyanate"          = reenrichResults[[i]][row,"isocyanate"],
                            "Aldehyde"            = reenrichResults[[i]][row,"aldehyde"],
                            "Epoxide"             = reenrichResults[[i]][row,"epoxide"],
                            stringsAsFactors=FALSE
                          )
                        }
                        return(finalTable)
                      }
                    }
                  }))
                  fullTableTmp <- data.frame(fullTableTmp)
                  
                  imgPath1 <- '<img src="images/warnings/'
                  imgPath2 <- ' height="50" width="100"></img>'
                  
                  # Check if original string contains any of the reactive structures
                  originalInputStr <- ""
                  for(j in originalNames){
                    originalSetName <- unlist(str_split(j, "__"))
                    if(originalSetName[2] == i){
                      originalInputStr <- paste0(originalSetName[1])
                    }
                  }
                  resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="reactiveGroups", query=list(input=originalInputStr))
                  if(resp$status_code != 200){
                    return(NULL)
                  }
                  originalInputStrReactive <- unlist(str_split(content(resp), ","))
                  # 1=cyanide, 2=isocyanate, 3=aldehyde, 4=epoxide
                  
                  # Simplify reactive structure columns into one warning column
                  fullTableWarnings <- lapply(1:nrow(fullTableTmp), function(tableRow){
                    warningToDisplay <- ""
                    nitrileCol <- fullTableTmp[tableRow, (ncol(fullTableTmp)-3)]
                    isocyanateCol <- fullTableTmp[tableRow, (ncol(fullTableTmp)-2)]
                    aldehydeCol <- fullTableTmp[tableRow, (ncol(fullTableTmp)-1)]
                    epoxideCol <- fullTableTmp[tableRow, (ncol(fullTableTmp))]

                    if(toString(nitrileCol) != toString(originalInputStrReactive[1])) {
                      warningToDisplay <- paste0(tipify(div( HTML(paste0(warningToDisplay, imgPath1, 'nitrile.png"', imgPath2))   ), "Nitrile (Cyanide) group.", placement="left"))
                    }
                    
                    if(toString(isocyanateCol) != toString(originalInputStrReactive[2])) {
                      warningToDisplay <- paste0(tipify(div( HTML(paste0(warningToDisplay, imgPath1, 'isocyanate.png"', imgPath2)) ), "Isocyanate group.", placement="left"))
                    }
                  
                    if(toString(aldehydeCol) != toString(originalInputStrReactive[3])) {
                      warningToDisplay <- paste0(tipify(div( HTML(paste0(warningToDisplay, imgPath1, 'aldehyde.png"', imgPath2))   ),  "Aldehyde group.", placement="left"))
                    }
              
                    if(toString(epoxideCol) != toString(originalInputStrReactive[4])) {
                      warningToDisplay <- paste0(tipify(div( HTML(paste0(warningToDisplay, imgPath1, 'epoxide.png"', imgPath2))     ), "Epoxide group.", placement="left"))
                    }
        
                    if(warningToDisplay == "") { # if no warnings
                      warningToDisplay <- "<p>None</p>"
                    }
                    return(warningToDisplay)
                  })
                  
                  # Create final table to display
                  # Truncate to remove individual warning columns
                  if (mode == "similarity" | originalEnrichMode == "similarity") {
                    fullTable <- fullTableTmp[, 1:10]
                  } else if(mode == "substructure" | originalEnrichMode == "substructure") { # Substructure (no similarity column)
                    fullTable <- fullTableTmp[, 1:9]
                  }
                  
                  # Check if any warnings were generated
                  warningCheck <- lapply(fullTableWarnings, function(x){
                    if(x == "<p>None</p>"){
                      return(NULL)
                    }
                    return(TRUE)
                  })
                  warningCheck <- warningCheck[!sapply(warningCheck, is.null)]
                  
                  # Save warningCheck to reactive value so we can reference outside of this scope
                  warningList$warnings[[i]] <- warningCheck
                  
                  # Create checkbox column to display
                  selectList <- lapply(checkboxes[[i]], function(x) {
                    return(paste0(x))
                  })
                  
                  if(length(warningCheck) > 0){
                    # If we have warnings, then include the warning column
                    fullTable$warning=fullTableWarnings  
                    # Add checkboxes for CASRNs
                    fullTable <- data.frame(select=unlist(selectList), fullTable)
                    # Save this so we can use it outside of the method
                    finalTableToDisplay$table[[i]] <- fullTable
                    # Set appropriate column names to display in table
                    if (mode == "similarity" | originalEnrichMode == "similarity") {
                      names(fullTable) <- c(
                        "Select",
                        "Chemical Structure",
                        "DSSTox Substance ID",
                        "CASRN",
                        "IUPAC Name",
                        "SMILES",
                        "InChI",
                        "InChI Key",
                        "Molecular Formula",
                        "Molecular Weight",
                        "Similarity",
                        paste0( tipify( div("Reactive Structure Warning"), "Warning: either this chemical contains a known reactive group(s) while your original submission did not, or this chemical does not contain a known reactive group(s) that your original submission contained. It is recommended that you deselect this chemical and perform re-enrichment on your data set.", placement="bottom" ) )
                      )
                    } else if(mode == "substructure" | originalEnrichMode == "substructure") {
                      names(fullTable) <- c(
                        "Select",
                        "Chemical Structure",
                        "DSSTox Substance ID",
                        "CASRN",
                        "IUPAC Name",
                        "SMILES",
                        "InChI",
                        "InChI Key",
                        "Molecular Formula",
                        "Molecular Weight",
                        paste0( tipify( div("Reactive Structure Warning"), "Warning: either this chemical contains a known reactive group(s) while your original submission did not, or this chemical does not contain a known reactive group(s) that your original submission contained. It is recommended that you deselect this chemical and perform re-enrichment on your data set.", placement="bottom" ) )
                      )
                    }
                    
                  } else {
                    # Add checkboxes for CASRNs
                    fullTable <- data.frame(select=unlist(selectList), fullTable)
                    
                    # Set appropriate column names to display in table
                    if (mode == "similarity" | originalEnrichMode == "similarity") {
                      names(fullTable) <- c(
                        "Select",
                        "Chemical Structure",
                        "DSSTox Substance ID",
                        "CASRN",
                        "IUPAC Name",
                        "SMILES",
                        "InChI",
                        "InChI Key",
                        "Molecular Formula",
                        "Molecular Weight",
                        "Similarity"
                      )
                    } else if(mode == "substructure" | originalEnrichMode == "substructure") {
                      names(fullTable) <- c(
                        "Select",
                        "Chemical Structure",
                        "DSSTox Substance ID",
                        "CASRN",
                        "IUPAC Name",
                        "SMILES",
                        "InChI",
                        "InChI Key",
                        "Molecular Formula",
                        "Molecular Weight"
                      )
                    }
                  }
                  
                  # Remove dataframe row names so they will just be numbered
                  rownames(fullTable) <- 1:nrow(fullTable)
                  
                  # Check if chemicals with warnings exist
                  if(length(unlist(warningList$warnings[[i]], recursive=FALSE)) > 0){
                    haveWarnings$warnings <<- TRUE
                  }
                  
                  if(mode != "casrn" | (originalEnrichMode == "substructure" | originalEnrichMode == "similarity")) {
                    output[[paste0("table_", i)]] <- renderUI(
                      column(12, style="height:500px; overflow-y:scroll;",
                        DT::datatable({fullTable},
                        # Render reenrichment table (solution from https://stackoverflow.com/questions/37356625/adding-a-column-with-true-false-and-showing-that-as-a-checkbox/37356792#37356792)
                            escape = FALSE, 
                            class = "row-border stripe compact",
                            rownames = FALSE,
                            style = "bootstrap",
                            select = "none",
                            options = list( 
                              paging=TRUE,
                              preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'), 
                              drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
                              dom="Bfrtip",
                              pageLength=10,
                              buttons=list("copy", "csv", "excel", "pdf", "print", list( extend="colvis", columns=as.vector(1:(ncol(fullTable)-1)) ))  
                              #                                                           ^^ this is so we always keep the select checkboxes in the table (user can't hide them)
                            ),
                            extensions="Buttons"
                        )
                      )
                    )
                    
                    outputOptions(output, paste0("table_", i), suspendWhenHidden=FALSE)
                  } 
  
                })
              }
              
              # Render re-enrich cutoff slider
              output[["reenrichCutoff"]] <- renderUI(
                fluidRow(
                  # Re-enrichment cutoff slider
                  column(12,
                    h3("Adjust Network Node Cutoff"),
                    bsTooltip(id="nodeCutoffRe", title="This will determine the maximum number of results per data set and may affect how many nodes are generated during network generation. (default = 10). Higher values may cause the enrichment process to take longer (Not available when viewing annotations for Tox21 chemicals).", placement="bottom", trigger="hover"),
                    sliderInput(inputId = "nodeCutoffRe", label="Re-enrichment Cutoff", value=10, min=1, max=50, step=1, width="100%")
                  ),
                  hr()
                )
              )

              # Render re-enrich buttons
              output[["reenrichButtonOut"]] <- renderUI(
                fluidRow(
                  # Deselect per set button
                  column(id=paste0("selectAllSet__", i), 6, 
                    if(mode != "casrn" | (originalEnrichMode == "substructure" | originalEnrichMode == "similarity")) {
                      actionButton("selectAllSetButton", "Deselect all chemicals for this set")   
                    }
                  ),
                  # Deselect all chemicals in all sets
                  column(id=paste0("selectAllReenrich", i), 6, 
                    if(mode != "casrn" | (originalEnrichMode == "substructure" | originalEnrichMode == "similarity")) {
                      actionButton("selectAllReenrichButton", "Deselect all chemicals")   
                    }
                  ),
                  # Deselect all chemicals with warnings
                  column(id=paste0("selectAllWarningsReenrich", i), 6,
                    if(haveWarnings$warnings == TRUE) {
                      if(mode != "casrn" | (originalEnrichMode == "substructure" | originalEnrichMode == "similarity")) {
                        actionButton("selectAllWarningsReenrichButton", HTML("<div class=\"text-danger\">Deselect all chemicals with warnings</div>"))
                      }
                    }
                  ),
                  
                  # Reenrich selected chemicals
                  column(id=paste0("reenrichButtonCol", i), 12, 
                    if(mode != "casrn" | (originalEnrichMode == "substructure" | originalEnrichMode == "similarity")) {
                      actionButton("reenrichButton", "Reenrich selected chemicals", icon=icon("arrow-alt-circle-right"))    
                    } else {
                      actionButton("updateNetworkButton", "Update network", icon=icon("arrow-alt-circle-right"))    
                    },
                    hidden(
                      uiOutput("reenrich_error_box")
                    )
                  ),
                  hr()
                )
              )
          #}

          # Render Chart & Cluster heatmaps for all sets
          
          # Query API to read in gct file
          # Fetch setFiles from server via API
          resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="readGct", query=list(transactionId=transactionId, cutoff=cutoff, mode="chart"))
          #TODO: error check
          if(resp$status_code != 200){
            
            # Hide results container
            shinyjs::hide(id="resultsContainer")
            # Show waiting page
            shinyjs::show(id="waitingPage")
            
            output$results_error_box <- renderUI(HTML("<div class=\"text-danger\">There was an error completing your request.</div>"))

            return(NULL)
          }
          
          # TODO: Clean this up!
          
          gctFileChart <- sapply(content(resp), function(x){
            gctFileChartInner <- unlist(sapply(names(x), function(y) {
              if(y != "_row"){
                return(as.double(x[y]))  
              } else {
                return(NULL)
              }
            }))
            gctFileChartInner <- gctFileChartInner[!sapply(gctFileChartInner, is.null)]

            dfToReturn <- t(data.frame(gctFileChartInner, stringsAsFactors=FALSE))
            row.names(dfToReturn) <- x["_row"]
            return(dfToReturn)
          })
          
          gctFileChart <- data.frame(gctFileChart)
          
          # This is here to catch anything that's only 1 input set
          if(ncol(gctFileChart) > 1) {
            gctFileChart <- t(gctFileChart)
          } 
          
          gctFileChartColNames <- unique(unlist(lapply(content(resp), function(x){
            gctFileChartInner <- unlist(lapply(names(x), function(y) {
              if(y != "_row"){
                return(y)
              } else {
                return(NULL)
              }
            }))
            gctFileChartInner <- gctFileChartInner[!sapply(gctFileChartInner, is.null)]
            return(gctFileChartInner)
          })))
          
          
          
          gctFileChartRowNames <- unique(unlist(lapply(content(resp), function(x){
            gctFileChartInner <- unlist(lapply(names(x), function(y) {
              if(y != "_row"){
                return(NULL)
              } else {
                return(x[y])
              }
            }))
            gctFileChartInner <- gctFileChartInner[!sapply(gctFileChartInner, is.null)]
            return(gctFileChartInner)
          })))

          colnames(gctFileChart) <- gctFileChartColNames
          row.names(gctFileChart) <- gctFileChartRowNames
          gctFileChartMatrix <- data.matrix(gctFileChart)
          gctCASRNNamesChart <- rownames(gctFileChart)
          gctAnnoNamesChart <- colnames(gctFileChart)
          
          # Query API to read in gct file
          # Fetch setFiles from server via API
          resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="readGct", query=list(transactionId=transactionId, cutoff=cutoff, mode="cluster"))
          #TODO: error check
          if(resp$status_code != 200){
            #return(NULL)
          }
          # TODO: Clean this up!
          gctFileCluster <- sapply(content(resp), function(x){
            gctFileClusterInner <- unlist(sapply(names(x), function(y) {
              if(y != "_row"){
                return(as.double(x[y]))  
              } else {
                return(NULL)
              }
            }))
            gctFileClusterInner <- gctFileClusterInner[!sapply(gctFileClusterInner, is.null)]
            
            dfToReturn <- t(data.frame(gctFileClusterInner, stringsAsFactors=FALSE))
            row.names(dfToReturn) <- x["_row"]
            return(dfToReturn)
          })
          
          gctFileCluster<- data.frame(gctFileCluster)
          
          if(ncol(gctFileCluster) > 1){
            gctFileCluster <- t(gctFileCluster)
          }
          
          gctFileClusterColNames <- unique(unlist(lapply(content(resp), function(x){
            gctFileClusterInner <- unlist(lapply(names(x), function(y) {
              if(y != "_row"){
                return(y)
              } else {
                return(NULL)
              }
            }))
            gctFileClusterInner <- gctFileClusterInner[!sapply(gctFileClusterInner, is.null)]
            return(gctFileClusterInner)
          })))
          gctFileClusterRowNames <- unique(unlist(lapply(content(resp), function(x){
            gctFileClusterInner <- unlist(lapply(names(x), function(y) {
              if(y != "_row"){
                return(NULL)
              } else {
                return(x[y])
              }
            }))
            gctFileClusterInner <- gctFileClusterInner[!sapply(gctFileClusterInner, is.null)]
            return(gctFileClusterInner)
          })))
          
          colnames(gctFileCluster) <- gctFileClusterColNames
          row.names(gctFileCluster) <- gctFileClusterRowNames
          gctFileClusterMatrix <- data.matrix(gctFileCluster)
          gctCASRNNamesCluster <- rownames(gctFileCluster)
          gctAnnoNamesCluster <- colnames(gctFileCluster)
          
          # Transpose matrices for heatmap display
          gctFileChartMatrix <- t(gctFileChartMatrix)
          gctFileClusterMatrix <- t(gctFileClusterMatrix)
          
          chartFullNetwork <- generateNetwork(transactionId=transactionId, cutoff=cutoff, networkMode="chart", inputNetwork=names(enrichmentSets), qval=0.05)
          clusterFullNetwork <- generateNetwork(transactionId=transactionId, cutoff=cutoff, networkMode="cluster", inputNetwork=names(enrichmentSets), qval=0.05)
          
          
          
          output[["chartHeatmap"]] <- renderUI(
              fluidRow(
                column(12,
                  tabsetPanel(
                    tabPanel("Chart Heatmap", plot_ly(x = gctCASRNNamesChart, y = gctAnnoNamesChart, z = gctFileChartMatrix, colors = colorRamp(c("white", "red")), type="heatmap", xgap=2, ygap=2, colorbar=list(title=list(text='<b>-log<sub>10</sub> (BH P-value)</b>'))) %>% 
                               layout(
                                 margin = list(l = 300, r = 200, b = 160), 
                                 xaxis=list(title="<b>Annotations</b>", tickfont=list(size=9), tickangle=15), 
                                 yaxis=list(title="<b>Input Sets</b>", type="category"),
                                 plot_bgcolor="transparent",
                                 paper_bgcolor="transparent",
                                 font=list(color=theme$textcolor)
                                )), 
                    tabPanel("Chart Network", 
                      fluidRow(
                        column(3, 
                          h4("Edge Selection Criteria"),
                          numericInput(inputId="chartqval", label="Q-value", value=0.05, step=0.01, max=1.00, min=0.00),
                          checkboxGroupInput( label="Selected Input Sets", inputId="chartNetworkChoices", choices=names(enrichmentSets), selected=names(enrichmentSets) ),
                          HTML("<h5><b>Other Options</b></h5>"),
                          checkboxInput(inputId="physicsEnabledChart", label="Enable physics?", value=FALSE),
                          actionButton(inputId="chartNetworkUpdateButton", label="Update network"),
                        ), 
                        column(9,
                          uiOutput("chartNetwork") %>% withSpinner()
                        )
                      )
                    )
                  ),
                )
              )
          )
          output[["clusterHeatmap"]] <- renderUI(
              fluidRow(
                column(12,
                  tabsetPanel(
                    tabPanel("Cluster Heatmap", plot_ly(x = gctCASRNNamesCluster, y = gctAnnoNamesCluster, z = gctFileClusterMatrix, colors = colorRamp(c("white", "red")), type="heatmap", xgap=2, ygap=2, colorbar=list(title=list(text='<b>-log<sub>10</sub> (BH P-value)</b>'))) %>% 
                               layout(
                                 margin = list(l = 300, r = 200, b = 160), 
                                 xaxis=list(title="<b>Annotation Clusters</b>", tickfont=list(size=9), tickangle=15), 
                                 yaxis=list(title="<b>Input Sets</b>", type="category"),
                                 plot_bgcolor="transparent",
                                 paper_bgcolor="transparent",
                                 font=list(color=theme$textcolor)
                                )),
                    tabPanel("Cluster Network", 
                      fluidRow(
                        column(3, 
                          h4("Edge Selection Criteria"),
                          numericInput(inputId="clusterqval", label="Q-value", value=0.05, step=0.01, max=1.00, min=0.00),
                          checkboxGroupInput( label="Selected Input Sets", inputId="clusterNetworkChoices", choices=names(enrichmentSets), selected=names(enrichmentSets) ),
                          HTML("<h5><b>Other Options</b></h5>"),
                          checkboxInput(inputId="physicsEnabledCluster", label="Enable physics?", value=FALSE),
                          actionButton(inputId="clusterNetworkUpdateButton", label="Update network"),
                        ), 
                        column(9,
                               uiOutput("clusterNetwork") %>% withSpinner()
                        )
                      )
                    )
                  )
                )
              )
          )
          
          # Render networks
          output$chartNetwork <- renderUI(chartFullNetwork)
          output$clusterNetwork <- renderUI(clusterFullNetwork)

          # Create bar graph 
          # Query API to get chart simple
          resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="bargraph", query=list(transactionId=transactionId, inputSet=i))
          #TODO: error check
          if(resp$status_code != 200){
            #return(NULL)
          }
          
          bgChartAll <- lapply(content(resp), function(chartSimpleFile){
            bgChartCategory <- unlist(lapply(chartSimpleFile, function(x){
              return(x$Category)
            }))
            bgChartTerm <- unlist(lapply(chartSimpleFile, function(x){
              return(x$Term)
            }))
            bgChartPValue <- unlist(lapply(chartSimpleFile, function(x){
              return(as.numeric(x$PValue))
            }))

            # Create a data frame with only the columns we need (Category, Term, PValue)
            bgChart <- data.frame(Category=bgChartCategory, Term=bgChartTerm, PValue=bgChartPValue)
            
            # return NULL if nothing
            if(nrow(bgChart) == 0){
              return(NULL)
            }
            
            # Get a list of unique categories
            uniqueCategories <- unlist(lapply(1:nrow(bgChart), function(x){
              return(bgChart[x, "Category"])
            }))
            uniqueCategories <- unique(uniqueCategories)
            
            uniquePValues <- lapply(uniqueCategories, function(x){
              individualCategory <- subset(bgChart, bgChart$Category == x) 
              PValueAvg <- unlist(lapply(1:nrow(individualCategory), function(i){
                if(individualCategory[i, "PValue"] < 0.05){
                  return(individualCategory[i, "PValue"])
                } else {
                  return(0) # TODO: is this right?
                }
              }))
              names(PValueAvg) <- individualCategory[, "Term"]
              # Remove values with non-significant p-values
              # TODO: clean this up
              PValueAvg <- PValueAvg[sapply(PValueAvg, function(x){
                if(x == 0) {
                  return(FALSE)
                }
                return(TRUE)
              })]
              return(PValueAvg)
            })
            names(uniquePValues) <- uniqueCategories
            
            # Remove empty categories
            uniquePValues <- uniquePValues[sapply(uniquePValues, function(x){
              if(length(x) > 0) {
                return(TRUE)
              } else {
                return(FALSE)
              }
            })]
            return(uniquePValues)
          })
          
          # Set trace names
          names(bgChartAll) <- names(enrichmentSets)
          
          bgChartAll <- lapply(bgChartAll, function(x){
            bgChartAllInner <- lapply(x, function(y){
              if(length(y) > 0) {
                return(y)
              } else {
                return(NULL)
              }
            })
            bgChartAllInner <- bgChartAllInner[!sapply(bgChartAllInner, is.null)]
          })
          
          # Get all existing category names
          bgChartAllCategories <- unlist(lapply(bgChartAll, function(x){
            return(names(x))
          }))
          bgChartAllCategories <- unique(bgChartAllCategories)
          
          bgChartFull <- lapply(bgChartAllCategories, function(catName){
            beChartFullInner <- lapply(bgChartAll, function(innerList){
              if(catName %in% names(innerList)){
                return(innerList[[catName]])
              } else {
                return(NULL)
              }
            })
            beChartFullInner <- beChartFullInner[!sapply(beChartFullInner, is.null)]
            return(beChartFullInner)
          })
          names(bgChartFull) <- bgChartAllCategories
          bgChartFullReactive$bgChartFull <- bgChartFull
          bgChartAllCategoriesReactive$bgChartAllCategories <- bgChartAllCategories
          
          # For each unique class name, render a bar plot
          createBargraph(bgChartFull=bgChartFull, bgChartAllCategories=bgChartAllCategories, orderSet=names(enrichmentSets)[1], colorsList=colorsList)
        }
        
        # Select/Deselect all chemicals for reenrichment
        selectAllReenrichButtonStatus <- reactiveValues(option = "deselect")
        if(is.null(setFilesObservers$observers[["selectAllReenrichButtonObserver"]])){
          setFilesObservers$observers[["selectAllReenrichButtonObserver"]] <- observeEvent(input$selectAllReenrichButton, {
            
            if(selectAllReenrichButtonStatus$option == "select") { # Selecting
              selectAllReenrichButtonStatus$option = "deselect"
              updateActionButton(session, "selectAllReenrichButton", label="Deselect all chemicals")
              for (i in checkboxList$checkboxes) {
                for(j in names(i)){
                  updateCheckboxInput(session, j, value = TRUE)
                }
              }
            } else { # Deselecting
              selectAllReenrichButtonStatus$option = "select"
              updateActionButton(session, "selectAllReenrichButton", label="Select all chemicals")
              for (i in checkboxList$checkboxes) {
                for(j in names(i)){
                  updateCheckboxInput(session, j, value = FALSE)
                }
              }
            }

          }, ignoreInit=TRUE, ignoreNULL=TRUE)
        }
        
        # Select/Deselect all chemicals for a single set
        selectAllSetButtonStatus <- reactiveValues(option = "deselect")
        if(is.null(setFilesObservers$observers[["selectAllSetButtonObserver"]])){
          setFilesObservers$observers[["selectAllSetButtonObserver"]] <- observeEvent(input$selectAllSetButton, {
            if(selectAllSetButtonStatus$option == "select") { # Selecting
              selectAllSetButtonStatus$option = "deselect"
              updateActionButton(session, "selectAllSetButton", label="Deselect all chemicals for this set")
              for (i in checkboxList$checkboxes) {
                for(j in names(i)){
                  if(unlist(str_split(j, "__"))[2] == input$tab){
                    updateCheckboxInput(session, j, value = TRUE)
                  }
                }
              }
            } else { # Deselecting
              selectAllSetButtonStatus$option = "select"
              updateActionButton(session, "selectAllSetButton", label="Select all chemicals for this set")
              for (i in checkboxList$checkboxes) {
                for(j in names(i)){
                  if(unlist(str_split(j, "__"))[2] == input$tab){
                    updateCheckboxInput(session, j, value = FALSE)
                  }
                }
              }
            }
          }, ignoreInit=TRUE, ignoreNULL=TRUE)
        }
        
        # Select/Deselect all chemicals with warnings for reenrichment
        selectAllWarningsReenrichButtonStatus <- reactiveValues(option = "deselect")
        if(is.null(setFilesObservers$observers[["selectAllWarningsReenrichButtonObserver"]])){
          setFilesObservers$observers[["selectAllWarningsReenrichButtonObserver"]] <- observeEvent(input$selectAllWarningsReenrichButton, {
            # Get list of all the chemicals with warnings per set
            chemicalsWithWarnings <- lapply(finalTableToDisplay$table, function(dataset){
              chemicalsWithWarningsInner <- lapply(1:nrow(dataset), function(line){
                if(is.null(dataset[line, "warning"]) == FALSE){
                  if(dataset[line, "warning"] != "<p>None</p>") {
                    return(paste0(dataset[line, "CASRN"]))
                  }
                }
              })
              return(unlist(chemicalsWithWarningsInner))
            })
            chemicalsWithWarningsList <- unlist(lapply(names(chemicalsWithWarnings), function(dataset){
              for(casrn in chemicalsWithWarnings[dataset]) {
                return(paste0(casrn, "__", dataset))
              }
            }))
            
            if(selectAllWarningsReenrichButtonStatus$option == "select") { # Selecting
              selectAllWarningsReenrichButtonStatus$option <- "deselect"
              updateActionButton(session, "selectAllWarningsReenrichButton", label="<div class=\"text-danger\">Deselect all chemicals with warnings</div>")
              for (i in checkboxList$checkboxes) {
                for(j in names(i)){
                  if(j %in% chemicalsWithWarningsList){
                    updateCheckboxInput(session, j, value = TRUE)  
                  }
                }
              }
            } else { # Deselecting
              selectAllWarningsReenrichButtonStatus$option <- "select"
              updateActionButton(session, "selectAllWarningsReenrichButton", label="<div class=\"text-danger\">Select all chemicals with warnings</div>")
              for (i in checkboxList$checkboxes) {
                for(j in names(i)){
                  if(j %in% chemicalsWithWarningsList){
                    updateCheckboxInput(session, j, value=FALSE)  
                  }
                }
              }
            }
          }, ignoreInit=TRUE, ignoreNULL=TRUE)
        }
        
        # Observer to select/deselect checkboxes on boot (workaround for issue #19 https://github.com/hurlab/tox21enricher/issues/19)
        observeEvent(input$tab, {
          chemicalsWithWarnings <- lapply(finalTableToDisplay$table, function(dataset){
            chemicalsWithWarningsInner <- lapply(1:nrow(dataset), function(line){
              if(is.null(dataset[line, "warning"]) == FALSE){
                if(dataset[line, "warning"] != "None") {
                  return(paste0(dataset[line, "CASRN"]))
                }
              }
            })
            return(unlist(chemicalsWithWarningsInner))
          })
          chemicalsWithWarningsList <- unlist(lapply(names(chemicalsWithWarnings), function(dataset){
            for(casrn in chemicalsWithWarnings[dataset]) {
              return(paste0(casrn, "__", dataset))
            }
          }))
          
          # Check if we deselected all chems
          if(selectAllReenrichButtonStatus$option == "select") {
            for (i in checkboxList$checkboxes) {
              for(j in names(i)){
                updateCheckboxInput(session, j, value = FALSE)
              }
            }
          }
          
          # Check if we deselected all chems with warnings
          if(selectAllWarningsReenrichButtonStatus$option == "select") {
            chemicalsWithWarnings <- lapply(finalTableToDisplay$table, function(dataset){
              chemicalsWithWarningsInner <- lapply(1:nrow(dataset), function(line){
                if(is.null(dataset[line, "warning"]) == FALSE){
                  if(dataset[line, "warning"] != "<p>None</p>") {
                    return(paste0(dataset[line, "CASRN"]))
                  }
                }
              })
              return(unlist(chemicalsWithWarningsInner))
            })
            chemicalsWithWarningsList <- unlist(lapply(names(chemicalsWithWarnings), function(dataset){
              for(casrn in chemicalsWithWarnings[dataset]) {
                return(paste0(casrn, "__", dataset))
              }
            }))
            for (i in checkboxList$checkboxes) {
              for(j in names(i)){
                if(j %in% chemicalsWithWarningsList){
                  updateCheckboxInput(session, j, value=FALSE)  
                }
              }
            }
          }
        })
        
        # Re-enable refresh button
        shinyjs::enable(id="refresh")
        # Re-enable results page
        shinyjs::enable(id="enrichmentResults")

    }
    
    # Re-order bar graphs with respect to given input set
    observeEvent(input$updateBargraphButton, {
      createBargraph(bgChartFull=bgChartFullReactive$bgChartFull, bgChartAllCategories=bgChartAllCategoriesReactive$bgChartAllCategories, orderSet=input$radioBargraph, colorsList=setColors$color)
    })
    
    # Shared code to create networks
    generateNetwork <- function(transactionId, cutoff, networkMode, inputNetwork, qval, physicsEnabled=FALSE){
      inputNetwork <- paste0(inputNetwork, collapse="#")
      
      # Error handling
      resp <- GET(url=paste0("http://", API_HOST, ":", API_PORT, "/"), path="generateNetwork", query=list(transactionId=transactionId, cutoff=cutoff, mode=networkMode, input=inputNetwork, qval=qval))
      #TODO: error check
      if(resp$status_code != 200){
        return(HTML("<p class=\"text-danger\"><b>Error:</b> An error occurred while generating the network.</p>"))
      }
      
      if(length(content(resp)) == 0){
        return(HTML("<p class=\"text-danger\"><b>Error:</b> The generated network has zero nodes.</p>"))
      }
      
      outpNetwork <- t(sapply(content(resp), function(x){
        return( data.frame(
          "pairwiseid"   = x[["pairwiseid"]],
          "term1uid"     = x[["term1uid"]],
          "term2uid"     = x[["term2uid"]],
          "term1size"    = x[["term1size"]],
          "term2size"    = x[["term2size"]],
          "common"       = x[["common"]],
          "union"        = x[["union"]],
          "jaccardindex" = x[["jaccardindex"]],
          "pvalue"       = x[["pvalue"]],
          "qvalue"       = x[["qvalue"]],
          "name1"        = x[["name1"]],
          "name2"        = x[["name2"]],
          "class1"       = x[["class1"]],
          "class2"       = x[["class2"]],
          "url1"         = x[["url1"]],
          "url2"         = x[["url2"]],
          stringsAsFactors = FALSE
        ))
      }))
      outpNetwork <- as.data.frame(outpNetwork)
      rownames(outpNetwork) <- 1:nrow(outpNetwork)
      
      # define class colors
      classColors <- generateAnnoClassColors()
      
      # Create rows and classes
      # 1
      rowsSet1 <- lapply(1:nrow(outpNetwork), function(x) paste0(outpNetwork[[x, "name1"]], "@", outpNetwork[[x, "class1"]], "@", outpNetwork[[x, "url1"]], "@", classToColor(outpNetwork[[x, "class1"]], classColors)))
      # 2
      rowsSet2 <- lapply(1:nrow(outpNetwork), function(x) paste0(outpNetwork[[x, "name2"]], "@", outpNetwork[[x, "class2"]], "@", outpNetwork[[x, "url2"]], "@", classToColor(outpNetwork[[x, "class2"]], classColors)))
      rowsSet <- list(unlist(rowsSet1), unlist(rowsSet2))
      rowsSet <- unique(unlist(rowsSet, recursive = FALSE))
      
      # 1
      classes1 <- lapply(1:nrow(outpNetwork), function(x) paste0(outpNetwork[[x, "class1"]]))
      # 2
      classes2 <- lapply(1:nrow(outpNetwork), function(x) paste0(outpNetwork[[x, "class2"]]))
      classes <- list(unlist(classes1), unlist(classes2))
      classes <- unique(unlist(classes, recursive = FALSE))
      
      # Generate list of nodes for network
      networkFullNodes <- t(sapply(rowsSet, function(s){
        rowsSetSplit <- unlist(str_split(s, "@"))
        # split: 1 = Term, 2 = Class (Category), 3 = URL, 4 = Color 
        id     <- paste0(rowsSetSplit[1], "@", rowsSetSplit[2])
        url    <- paste0(rowsSetSplit[3], rowsSetSplit[1])
        rgbCss  <- rowsSetSplit[4]
        return(data.frame( id=id, label=rowsSetSplit[1], group=rowsSetSplit[2], shape="ellipse", url=url, color=rgbCss, stringsAsFactors=FALSE ))
      }))
      networkFullNodes <- as.data.frame(networkFullNodes)
      rownames(networkFullNodes) <- 1:nrow(networkFullNodes)
      
      # Generate list of edges for network
      networkFullEdges <- t(sapply(1:nrow(outpNetwork), function(p){
        rgbCss = generateJaccardColor(as.numeric(outpNetwork[[p, "jaccardindex"]]))
        edgeUUID <- UUIDgenerate()
        return(data.frame( from=paste0(outpNetwork[[p, "name1"]], "@", outpNetwork[[p, "class1"]]), to=paste0(outpNetwork[[p, "name2"]], "@", outpNetwork[[p, "class2"]]), jaccard=outpNetwork[[p, "jaccardindex"]], color=rgbCss, id=edgeUUID, stringsAsFactors=FALSE ))
      }))
      networkFullEdges <- as.data.frame(networkFullEdges)
      rownames(networkFullEdges) <- 1:nrow(networkFullEdges)
      
      fullNetwork <- visNetwork(networkFullNodes, networkFullEdges, height="500px", width="100%") %>%
        visOptions(highlightNearest=TRUE, nodesIdSelection=TRUE, selectedBy="group") %>%
        visLayout(randomSeed=runif(1)) %>%
        visPhysics(solver="forceAtlas2Based", enabled=physicsEnabled, stabilization=list(enabled=FALSE, iterations=1000, updateInterval=25)) %>%
        visEdges(smooth=TRUE) %>%
        visInteraction(navigationButtons=TRUE, keyboard=FALSE, selectable=TRUE)
      
      # Add groups for legend
      for (x in classes) {
        groupColor <- paste0( "rgb(", paste0(classColors[[x]], collapse=", "), ")" )
        fullNetwork <- fullNetwork %>% visGroups(groupname=x, color=groupColor)
      }
      fullNetwork <- fullNetwork %>% visLegend()
      
      fullNetworkExport <- visExport(
        graph=fullNetwork,
        type="png"
      )
      
      return(fluidRow(id=paste0(networkMode,"NetworkContainer"), fullNetworkExport))
    }
    
    # Re-generate chart network
    observeEvent(input$chartNetworkUpdateButton, {
      # Hide old network
      output$chartNetwork <- renderUI(div())
      #TODO: error handle no results
      if(length(input$chartNetworkChoices) == 0){
        output$chartNetwork <- renderUI(HTML("<p class=\"text-danger\"><b>Error:</b> No input sets selected.</p>"))
      } else {
        chartFullNetwork <- generateNetwork(transactionId=reactiveTransactionId$id, cutoff=input$nodeCutoffRe, networkMode="chart", inputNetwork=input$chartNetworkChoices, qval=input$chartqval, physicsEnabled=input$physicsEnabledChart)
        # Render networks
        output$chartNetwork <- renderUI(chartFullNetwork) 
      }
    })
    
    # Re-generate cluster network
    observeEvent(input$clusterNetworkUpdateButton, {
      # Hide old network
      output$clusterNetwork <- renderUI(div())
      #TODO: error handle no input sets
      if(length(input$clusterNetworkChoices) == 0){
        output$clusterNetwork <- renderUI(HTML("<p class=\"text-danger\"><b>Error:</b> No input sets selected.</p>"))
      } else {
        clusterFullNetwork <- generateNetwork(transactionId=reactiveTransactionId$id, cutoff=input$nodeCutoffRe, networkMode="cluster", inputNetwork=input$clusterNetworkChoices, qval=input$clusterqval, physicsEnabled=input$physicsEnabledCluster)
        # Render networks
        output$clusterNetwork <- renderUI(clusterFullNetwork)
      }
    })
    
    # Update network
    observeEvent(input$updateNetworkButton, {
      updateNetworkBox <- ""
      for (i in 1:length(enrichmentSetsList$enrichmentSets)){
        updateNetworkBox <- paste0(updateNetworkBox, "#", names(enrichmentSetsList$enrichmentSets)[i], "\n", paste0(enrichmentSetsList$enrichmentSets[[i]], collapse="\n"), "\n")
      }
      performEnrichment(updateNetworkBox, reenrichFlag=TRUE) 
    })
    
    # Get colors for corresponding network nodes
    classToColor <- function(annoClass, classColors) {
      if (is.null(classColors[annoClass]) == FALSE) {
        return(paste0("rgb(", paste0(classColors[[annoClass]], collapse=", "), ")"))
      } else {
        return(NULL)
      }
    }
    
    # Generate specific colors for annotation classes in network
    generateAnnoClassColors <- function() {
      return(
        list( 
          "ACTIVITY_CLASS"                  = c(255, 11, 11), 
          "ADVERSE_EFFECT"                  = c(246, 71, 36), 
          "CTD_GOFAT_BIOPROCESS"            = c(253, 174, 17),
          "CTD_GOSLIM_BIOPROCESS"           = c(171, 118, 14),
          "CTD_CHEMICALS_DISEASES"          = c(33, 209, 86),
          "CTD_CHEMICALS_GENES"             = c(107, 237, 124),
          "CTD_CHEMICALS_GOENRICH_CELLCOMP" = c(169, 232, 178),
          "CTD_CHEMICALS_GOENRICH_MOLFUNCT" = c(12, 201, 107),
          "CTD_CHEMICALS_PATHWAYS"          = c(105, 207, 58),
          "DRUGBANK_CARRIERS"               = c(254, 254, 25),
          "DRUGBANK_ENZYMES"                = c(48, 246, 246), 
          "DRUGBANK_TRANSPORTERS"           = c(7, 210, 250),
          "DRUGBANK_ATC"                    = c(217, 252, 41), 
          "DRUGBANK_ATC_CODE"               = c(50, 185, 253), 
          "DRUGBANK_TARGETS"                = c(178, 253, 29), 
          "HTS_ACTIVE"                      = c(144, 244, 43), 
          "HTS_STRONGACTIVE"                = c(80, 140, 20), 
          "INDICATION"                      = c(102, 245, 30), 
          "KNOWN_TOXICITY"                  = c(54, 254, 14), 
          "LEADSCOPE_TOXICITY"              = c(42, 246, 42), 
          "MECH_LEVEL_1"                    = c(18, 253, 58), 
          "MECH_LEVEL_2"                    = c(6, 252, 88), 
          "MECH_LEVEL_3"                    = c(15, 248, 131), 
          "MECHANISM"                       = c(31, 252, 178), 
          "MESH"                            = c(48, 249, 215), 
          "MODE_CLASS"                      = c(26, 137, 247), 
          "MULTICASE_TOX_PREDICTION"        = c(8, 88, 248), 
          "PHARMACTIONLIST"                 = c(47, 80, 247), 
          "PRODUCT_CLASS"                   = c(31, 31, 244), 
          "STRUCTURE_ACTIVITY"              = c(52, 12, 251), 
          "TA_LEVEL_1"                      = c(98, 20, 254), 
          "TA_LEVEL_2"                      = c(140, 31, 249), 
          "TA_LEVEL_3"                      = c(170, 17, 247), 
          "THERAPEUTIC_CLASS"               = c(212, 34, 248), 
          "TISSUE_TOXICITY"                 = c(254, 48, 254), 
          "TOXCAST_ACTIVE"                  = c(244, 30, 209), 
          "TOXINS_TARGETS"                  = c(252, 35, 180), 
          "TOXPRINT_STRUCTURE"              = c(249, 24, 137), 
          "TOXREFDB"                        = c(247, 55, 119), 
          "TISSUE_TOXICITY"                 = c(244, 31, 67)
        )
      )
    }
    
    # Generate specific colors for network edges based on jaccard index
    generateJaccardColor <- function(jaccard) {
      if (jaccard < 0.0) {
        # invalid
      }
      else if (jaccard >= 0.0 & jaccard < 0.1) {
        return("rgb(132, 232, 246)")
      }
      else if (jaccard >= 0.1 & jaccard < 0.2) {
        return("rgb(121, 210, 233)")
      }
      else if (jaccard >= 0.2 & jaccard < 0.3) {
        return("rgb(110, 189, 221)")
      }
      else if (jaccard >= 0.3 & jaccard < 0.4) {
        return("rgb(99, 168, 289)")
      }
      else if (jaccard >= 0.4 & jaccard < 0.5) {
        return("rgb(88, 147, 196)")
      }
      else if (jaccard >= 0.5 & jaccard < 0.6) {
        return("rgb(78, 126, 184)")
      }
      else if (jaccard >= 0.6 & jaccard < 0.7) {
        return("rgb(67, 104, 72)")
      }
      else if (jaccard >= 0.7 & jaccard < 0.8) {
        return("rgb(56, 83, 159)")
      }
      else if (jaccard >= 0.8 & jaccard < 0.9) {
        return("rgb(45, 62, 147)")
      }
      else if (jaccard >= 0.9 & jaccard <= 1.0) {
        return("rgb(34, 41, 135)")
      }
      else {
        # invalid
        return("-1")
      }
    }
    
    # Generate a list of "acceptable" colors. This is a subset of the colors returned by colors() that contrast well with a white background but also present black text well.
    getNetworkColors <- function(){
      list(
        "coral3", "coral", "chocolate3", "chocolate1", "chocolate", "chartreuse4", "chartreuse3", "chartreuse", "cadetblue4", "cadetblue3", "cadetblue2", "burlywood4", "burlywood3", "brown3", "brown1", "brown", "blueviolet", "blue1", "bisque4", "bisque3", 
        "azure4", "azure3", "aquamarine4", "aquamarine3", "antiquewhite4", "antiquewhite3", "deeppink4", "deeppink3", "deeppink", "darkviolet", "darkturquoise", "darkslategrey", "darkslategray4", "darkslategray3", "darkslateblue", "darkseagreen4", 
        "darkseagreen3", "darkseagreen", "darksalmon", "darkred", "darkorchid4", "darkorchid", "darkorange4", "darkorange", "darkolivegreen4", "darkolivegreen3", "darkolivegreen", "darkmagenta", "darkkhaki", "darkgrey", "darkgreen", "darkgoldenrod4", 
        "darkgoldenrod1", "darkgoldenrod", "darkcyan", "darkblue", "cyan4", "cyan3", "cyan", "cornsilk4", "cornsilk3", "cornflowerblue", "coral4", "goldenrod4", "goldenrod3", "goldenrod", "gold", "forestgreen", "firebrick4", "firebrick3", "firebrick1", 
        "dodgerblue4", "dodgerblue3", "dodgerblue", "dimgrey", "deepskyblue4", "deepskyblue3", "deepskyblue", "greenyellow", "green4", "green2", "green", "grey70", "grey55", "grey40", "lightpink1", "lightgrey", "lightgreen", "lightgoldenrod4", 
        "lightgoldenrod3", "lightgoldenrod1", "lightcyan4", "lightcyan3", "lightcyan", "lightcoral", "lightblue4", "lightblue3", "lightblue1", "lemonchiffon4", "lemonchiffon3", "lemonchiffon1", "lawngreen", "lavenderblush4", "lavenderblush3", "lavender", 
        "khaki4", "khaki3", "khaki1", "khaki", "ivory4", "ivory3", "indianred4", "indianred1", "indianred", "hotpink4", "hotpink3", "hotpink1", "hotpink", "honeydew4", "honeydew3", "mistyrose", "mediumvioletred", "mediumturquoise", "mediumspringgreen", 
        "mediumslateblue", "mediumseagreen", "mediumpurple4", "mediumpurple3", "mediumpurple1", "mediumorchid4", "mediumorchid3", "mediumorchid1", "mediumaquamarine", "maroon4", "maroon3", "maroon1", "maroon", "magenta4", "magenta3", "magenta", "limegreen", 
        "lightyellow4", "lightyellow3", "lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightslategrey", "lightslateblue", "lightskyblue3", "lightskyblue", "lightseagreen", "lightsalmon4", "lightsalmon3", "lightsalmon", "lightpink4", "lightpink2", 
        "pink4", "pink3", "pink2", "peru", "peachpuff4", "peachpuff3", "peachpuff", "papayawhip", "palevioletred4", "palevioletred3", "palevioletred1", "paleturquoise4", "paleturquoise3", "paleturquoise1", "palegreen4", "palegreen3", "palegreen", 
        "palegoldenrod", "orchid4", "orchid3", "orchid1", "orangered4", "orangered3", "orangered", "orange4", "orange3", "orange2", "orange", "olivedrab4", "olivedrab3", "olivedrab2", "olivedrab", "navajowhite4", "navajowhite3", "navajowhite", "moccasin", 
        "mistyrose4", "mistyrose3", "mistyrose2", "slategray1", "slategray", "slateblue4", "slateblue3", "slateblue1", "slateblue", "skyblue4", "skyblue3", "skyblue1", "skyblue", "sienna4", "sienna3", "sienna1", "seashell4", "seashell3", "seashell2", 
        "seagreen4", "seagreen3", "seagreen1", "sandybrown", "salmon4", "salmon3", "salmon1", "salmon", "saddlebrown", "royalblue4", "royalblue3", "royalblue1", "royalblue", "rosybrown4", "rosybrown3", "rosybrown1", "red2", "red", "purple4", "purple3", 
        "purple1", "powderblue", "plum4", "plum3", "plum1", "yellowgreen", "yellow4", "yellow3", "yellow2", "yellow", "wheat4", "wheat3", "wheat2", "wheat1", "violetred4", "violetred3", "violetred1", "violetred", "violet", "turquoise4", "turquoise3", 
        "turquoise1", "turquoise", "tomato4", "tomato3", "tomato", "thistle4", "thistle3", "thistle2", "thistle", "tan4", "tan3", "tan1", "tan", "steelblue4", "steelblue3", "steelblue2", "steelblue", "springgreen4", "springgreen3", "springgreen", "snow4", 
        "snow3", "slategray4", "slategray3", "slategray2"
      )
    }
    
    # Generate bargraphs
    createBargraph <- function(bgChartFull=bgChartFull, bgChartAllCategories=bgChartAllCategories, orderSet, colorsList){
      
      output[["bargraph"]] <- renderUI(
        do.call(tabsetPanel, c(id="pvaluetab", lapply(bgChartAllCategories, function(catName){
          tmpBgNames <- unique(unlist(unname(lapply(bgChartFull[[catName]], function(x){
            return(names(x))
          }))))
          
          # Get inverted p-values (-log10)
          tmpBgCleaned <- lapply(bgChartFull[[catName]], function(x){
            tmpBgCleanedInner <- unlist(lapply(tmpBgNames, function(tmpName){
              if (tmpName %in% names(x)) {
                return(-log10(x[[tmpName]]))
              } else {
                return(0)
              }
            }))
            names(tmpBgCleanedInner) <- tmpBgNames
            return(tmpBgCleanedInner)
          })
          
          if(is.null(tmpBgCleaned[[orderSet]]) == FALSE) {
            # This is necessary so that the bar graph will be ordered by value in descending order
            tmpBgNames <- factor(tmpBgNames, levels=tmpBgNames[order(tmpBgCleaned[[orderSet]], decreasing=FALSE)]) #decreasing=FALSE because we invert the p-value
          } else {
            # If set is irrelevant (i.e., we are ordering by 'Set1' but only 'Set2' has results), just order by first item in set
            tmpBgNames <- factor(tmpBgNames, levels=tmpBgNames[order(tmpBgCleaned[[1]], decreasing=FALSE)]) #decreasing=FALSE because we invert the p-value
          }

          bgDisplay <- plot_ly(
            x = tmpBgCleaned[[1]], # p-values
            y = tmpBgNames, # term names
            name = names(tmpBgCleaned)[1],
            type = "bar",
            marker = list(color = colorsList[names(tmpBgCleaned[1])])
          ) %>% layout(
            title = catName,
            margin = list(l = 300, r = 200, b = 160), 
            xaxis=list(title="<b>-log<sub>10</sub> (P-value)</b>", tickfont=list(size=12)), # TODO: need to invert p-value
            yaxis=list(title="<b>Annotation Terms</b>", type="category"),
            barmode="group",
            plot_bgcolor="transparent",
            paper_bgcolor="transparent",
            font=list(color=theme$textcolor)
          )

          # Remove first input set since we have already plotted it
          tmpBgCleaned <- tmpBgCleaned[-1]

          # Add additional plots to bar graph if more than 1 valid input set
          if(length(tmpBgCleaned) > 0){
            for(i in 1:length(tmpBgCleaned)){
              bgDisplay <- bgDisplay %>% add_trace(x = tmpBgCleaned[[i]], name = names(tmpBgCleaned)[i], marker = list(color = colorsList[names(tmpBgCleaned[i])])  )
            }
          }
          return(tabPanel(title=catName, bgDisplay))
        })))
      )
    }

    # Perform re-enrichment on selected result chemicals
    observeEvent(input$reenrichButton, {
      reenrichCASRNBox <- ""
      reenrichCurrentSet <- ""
      for (i in checkboxList$checkboxes) {
        for(j in names(i)){
          if(is.null(input[[j]]) == TRUE){
            reenrichTmpSplit <- unlist(str_split(j, "__"))
            if (reenrichCurrentSet == "") {
              reenrichCASRNBox <- paste0(reenrichCASRNBox, "#", reenrichTmpSplit[2], "\n", reenrichTmpSplit[1], "\n")
              reenrichCurrentSet = reenrichTmpSplit[2]
            }
            else if (reenrichCurrentSet != reenrichTmpSplit[2] & reenrichCurrentSet != "") {
              reenrichCASRNBox <- paste0(reenrichCASRNBox, "\n#", reenrichTmpSplit[2], "\n", reenrichTmpSplit[1], "\n")
              reenrichCurrentSet = reenrichTmpSplit[2]
            }
            else {
              reenrichCASRNBox <- paste0(reenrichCASRNBox, "\n", reenrichTmpSplit[1], "\n")
            }
          } else if((is.null(input[[j]]) == FALSE & input[[j]] == TRUE)){
            reenrichTmpSplit <- unlist(str_split(j, "__"))
            if (reenrichCurrentSet == "") {
              reenrichCASRNBox <- paste0(reenrichCASRNBox, "#", reenrichTmpSplit[2], "\n", reenrichTmpSplit[1], "\n")
              reenrichCurrentSet = reenrichTmpSplit[2]
            }
            else if (reenrichCurrentSet != reenrichTmpSplit[2] & reenrichCurrentSet != "") {
              reenrichCASRNBox <- paste0(reenrichCASRNBox, "\n#", reenrichTmpSplit[2], "\n", reenrichTmpSplit[1], "\n")
              reenrichCurrentSet = reenrichTmpSplit[2]
            }
            else {
              reenrichCASRNBox <- paste0(reenrichCASRNBox, "\n", reenrichTmpSplit[1], "\n")
            }
          }
        }
      }
      
      if(reenrichCASRNBox == ""){
        # error if nothing selected
        # Show error msg
        shinyjs::show(id="reenrich_error_box")
        output[["reenrich_error_box"]] <- renderUI(
          HTML(paste0("<div class=\"text-danger\">Error: No chemicals are selected.</div>"))
        )
        return(FALSE)
      }

      # This is to preserve original names if we want to re-enrich similarity/substructure
      originalNamesToReturn <- lapply(originalNamesList$originalNames, function(originalName){
        tmpSplit <- unlist(str_split(originalName, "__"))
        return(tmpSplit[1])
      })
      names(originalNamesToReturn) <- lapply(originalNamesList$originalNames, function(originalName){
        tmpSplit <- unlist(str_split(originalName, "__"))
        return(tmpSplit[2])
      })
      
      # Reset reenrichResultsList$reenrichResults and enrichmentSetsList$enrichmentSets
      reenrichResultsList$reenrichResults <- NULL
      enrichmentSetsList$enrichmentSets <- NULL

      checkboxList$checkboxes <- NULL

      # Perform enrichment again
      performEnrichment(reenrichCASRNBox, reenrichFlag=TRUE, originalNamesToReturn=originalNamesToReturn)
    })
    
    
    
    
})




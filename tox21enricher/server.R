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
    setFilesObservers <- reactiveValues(observers=list())
    
    # List of chems with warnings
    firstCaseWarningChems <- reactiveValues(casrns=list())
  
    # Theme info
    theme <- reactiveValues(textcolor="#000000")
    
    # Heatmap color memory
    lHeatmapColor <- reactiveValues()
    hHeatmapColor <- reactiveValues()
    
    lHeatmapColorChart <- reactiveValues(color="white")
    hHeatmapColorChart <- reactiveValues(color="red")
    
    lHeatmapColorCluster <- reactiveValues(color="white")
    hHeatmapColorCluster <- reactiveValues(color="red")

    output[["themeStatus"]] <- renderUI({
        tags$style(HTML(paste0('
            .dataTables_length label, .dataTables_filter label, .dataTables_info {
                color: ', theme$textcolor, '!important;
            }
        ')))
    })
    
    # Quick functions for changing pages
    changePage <- function(page="enrichment"){
        if(page == "enrichment"){
            # Show enrichment form
            shinyjs::show(id="enrichmentForm")
            shinyjs::enable(id="enrichmentForm")
            # Show changing input type
            shinyjs::show(id="enrich_from")
            shinyjs::enable(id="enrich_from")
            # Hide 'Restart' button
            shinyjs::hide(id="refresh")
            shinyjs::disable(id="refresh")
            # Hide results page
            shinyjs::hide(id="resultsContainer")
            shinyjs::disable(id="resultsContainer")
            # Hide search page
            shinyjs::hide(id="searchForm")
            shinyjs::disable(id="searchForm")
            shinyjs::hide(id="searchButtonsMenu")
            shinyjs::disable(id="searchButtonsMenu")
            # Hide waiting page
            shinyjs::hide(id="waitingPage")
            shinyjs::disable(id="waitingPage")
            # Enable view previous button
            shinyjs::show(id="searchButton")
            shinyjs::enable(id="searchButton")
            # Enable settings button
            shinyjs::show(id="settingsButton")
            shinyjs::enable(id="settingsButton")
            # Enable button for getting results for previous request
            shinyjs::hide(id="searchPrevButton")
            shinyjs::disable(id="searchPrevButton")
            # Hide waiting page buttons
            shinyjs::hide(id="clipboard")
            shinyjs::hide(id="cancelEnrichment")
            shinyjs::disable(id="clipboard")
            shinyjs::disable(id="cancelEnrichment")
            # Reset inputs
            shinyjs::disable(id="includeChemsWithWarnings")
            shinyjs::reset(id="submitted_chemicals")
            shinyjs::reset(id="enrich_from")
        } else if (page == "search") {
            # Hide main page and disable controls
            shinyjs::hide(id="enrichmentForm")
            shinyjs::disable(id="enrichmentForm")
            # Hide changing input type
            shinyjs::hide(id="enrich_from")
            shinyjs::disable(id="enrich_from")
            # Show search page
            shinyjs::show(id="searchForm")
            shinyjs::enable(id="searchForm")
            shinyjs::show(id="searchButtonsMenu")
            shinyjs::enable(id="searchButtonsMenu")
            # Hide waiting page
            shinyjs::hide(id="waitingPage")
            shinyjs::disable(id="waitingPage")
            # Hide 'Restart' button
            shinyjs::hide(id="refresh")
            shinyjs::disable(id="refresh")
            # Enable view previous button
            shinyjs::show(id="searchButton")
            shinyjs::enable(id="searchButton")
            # Enable settings button
            shinyjs::show(id="settingsButton")
            shinyjs::enable(id="settingsButton")
            # Enable button for getting results for previous request
            shinyjs::show(id="searchPrevButton")
            shinyjs::enable(id="searchPrevButton")
            # Hide waiting page buttons
            shinyjs::hide(id="clipboard")
            shinyjs::hide(id="cancelEnrichment")
            shinyjs::disable(id="clipboard")
            shinyjs::disable(id="cancelEnrichment")
        } else if (page == "waiting") {
            # Reset waiting page
            output[["waitingTable"]] <- renderUI(HTML(""))
            # Hide original form when done with enrichment
            shinyjs::hide(id="enrichmentForm")
            shinyjs::disable(id="enrichmentForm")
            # Hide changing input type when button is clicked
            shinyjs::hide(id="enrich_from")
            shinyjs::disable(id="enrich_from")
            # Hide search page
            shinyjs::hide(id="searchForm")
            shinyjs::disable(id="searchForm")
            shinyjs::hide(id="searchButtonsMenu")
            shinyjs::disable(id="searchButtonsMenu")
            # Disable & Hide results page
            shinyjs::hide(id="enrichmentResults")
            shinyjs::disable(id="enrichmentResults")
            shinyjs::hide(id="resultsContainer")
            shinyjs::disable(id="resultsContainer")
            # Hide 'Restart' button
            shinyjs::hide(id="refresh")
            shinyjs::disable(id="refresh")
            # Disable View previous results button
            shinyjs::hide(id="searchButton")
            shinyjs::disable(id="searchButton")
            # Enable settings button
            shinyjs::show(id="settingsButton")
            shinyjs::enable(id="settingsButton")
            # Hide/disable button for getting results for previous request
            shinyjs::hide(id="searchPrevButton")
            shinyjs::disable(id="searchPrevButton")
            # Show waitingPage
            shinyjs::reset(id="waitingPage")
            shinyjs::show(id="waitingPage")
            shinyjs::enable(id="waitingPage")
            # Show waitingTable
            shinyjs::show(id="waitingTable")
            shinyjs::enable(id="waitingTable")
            # Reenable buttons
            shinyjs::show(id="clipboard")
            shinyjs::show(id="cancelEnrichment")
            shinyjs::enable(id="clipboard")
            shinyjs::enable(id="cancelEnrichment")
        } else if(page == "results"){
            # Hide original form when done with enrichment
            shinyjs::hide(id="enrichmentForm")
            shinyjs::disable(id="enrichmentForm")
            # Hide changing input type when button is clicked
            shinyjs::hide(id="enrich_from")
            shinyjs::disable(id="enrich_from")
            # Hide search page
            shinyjs::hide(id="searchForm")
            shinyjs::disable(id="searchForm")
            shinyjs::hide(id="searchButtonsMenu")
            shinyjs::disable(id="searchButtonsMenu")
            # Show & enable results page
            shinyjs::show(id="enrichmentResults")
            shinyjs::enable(id="enrichmentResults")
            shinyjs::show(id="resultsContainer")
            shinyjs::enable(id="resultsContainer")
            # Show 'Restart' button
            shinyjs::show(id="refresh")
            shinyjs::enable(id="refresh")
            # Disable View previous results button
            shinyjs::hide(id="searchButton")
            shinyjs::disable(id="searchButton")
            # Enable settings button
            shinyjs::show(id="settingsButton")
            shinyjs::enable(id="settingsButton")
            # Hide/disable button for getting results for previous request
            shinyjs::hide(id="searchPrevButton")
            shinyjs::disable(id="searchPrevButton")
            # Hide waitingPage
            shinyjs::hide(id="waitingPage")
            shinyjs::disable(id="waitingPage")
            # Show waitingTable
            shinyjs::hide(id="waitingTable")
            shinyjs::disable(id="waitingTable")
            # Hide waiting page buttons
            shinyjs::hide(id="clipboard")
            shinyjs::hide(id="cancelEnrichment")
            shinyjs::disable(id="clipboard")
            shinyjs::disable(id="cancelEnrichment")
        }
    }
  
    # API connectivity details
    # Change host address and port in config.yml
    tox21config <- config::get("tox21enricher-client")
    API_SECURE <- tox21config$secure
    if(API_SECURE){
        API_PROTOCOL <- "https://"
    } else {
        API_PROTOCOL <- "http://"
    }
    API_HOST <- tox21config$host
    API_PORT <- tox21config$port
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
    
    # TODO: adjust this as needed for external version
    # If specified in cookies, override config.yml host/port settings
    observe({
        js$getHostInfo()
        if(!is.null(input$hostInfo)) {
            hostInfoSplit <- unlist(str_split(input$hostInfo, ":"))
            API_HOST <- hostInfoSplit[1]
            API_PORT <- hostInfoSplit[2]
            # Check if this new connection will work
            pingAPIUserConfig <- tryCatch({
                resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/ping"))
                if(resp$status_code != 200) {
                    showNotification("Error: Could not connect to Tox21 Enricher server using user-defined settings. Defaulting to use configuration file settings.", type="warning")
                    API_HOST <<- tox21config$host
                    API_PORT <<- tox21config$port
                } 
            }, error=function(cond){
                showNotification("Error: Could not connect to Tox21 Enricher server using user-defined settings. Defaulting to use configuration file settings.", type="warning")
                API_HOST <<- tox21config$host
                API_PORT <<- tox21config$port
            })
        }
    })
    
    # Display API connection status
    apiStatus <- function() {
        pingAPI <- tryCatch({
            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/ping"))
            if(resp$status_code != 200) {
                output$apiConnection <- renderUI({
                    HTML(paste0("<div class=\"text-danger\">Could not connect to Tox21 Enricher server!</div>"))
                })
                return(FALSE)
            } else {
                output$apiConnection <- renderUI({
                    HTML(paste0("<div class=\"text-success\">Connection with Tox21 Enricher server successfully established.</div>"))
                })
                return(TRUE)
            } 
        }, error=function(cond){
            output$apiConnection <- renderUI({
                HTML(paste0("<div class=\"text-danger\">Could not connect to Tox21 Enricher server!</div>"))
            })
            return(FALSE)
        })
    }
    if(!apiStatus()){
        showNotification(paste0("Could not connect to Tox21 Enricher server using defined connection settings. Please ensure your connection settings are correct and refresh the page to try again." ), id="autoretryNotification", duration=5, type="error")
    }
    
    # Get cleanup time from server to save in cookie
    resp <- NULL
    cleanupTime <- reactiveValues(hours=48) # default 48 hours
    tryCleanup <- tryCatch({
        resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getCleanupTime"))
        TRUE
    }, error=function(cond){
        return(FALSE)
    })
    if(tryCleanup){
        cleanupTime$hours <- unlist(content(resp))
    }

    # Show settings menu
    observeEvent(input$settingsButton, {
        showModal(modalDialog(
            title="Settings",
            footer=tagList(modalButton("Close")),
            size="l",
            fluidRow(
                column(12, 
                    h4("Change host"),
                    HTML(
                        "<p>By default, Tox21 Enricher will attempt to connect to the host on the port defined in the config.yml file. To connect to a different host or on a different port,<br>
                        1) Update the host and port values here.<br>
                        2) Press the \"Update\" button, then the \"Close\" button to exit the settings menu.<br>
                        3) Reload the page.<br>
                        </p>"
                    ),
                    textInput(inputId="hostUpdate", label="Host", placeholder="i.e., \"hurlab.med.und.edu\""),
                    textInput(inputId="portUpdate", label="Port", placeholder="i.e., \"80\""),
                    actionButton(input="updateHostPort", label="Update"),
                    actionButton(input="clearHostPort", label="Clear")
                )
            )
        ))
    })
    # Update address for Tox21 Enricher API with user-defined settings
    observeEvent(input$updateHostPort, {
        # display error if both fields are not specified
        if(nchar(input$hostUpdate) < 1 | nchar(input$portUpdate) < 1){
            showNotification("Error: Both a host address and port value must be specified.", type="error")
            return(FALSE)
        }
        if(is.na(strtoi(input$portUpdate))) { # if port is not a valid integer
            showNotification("Error: Invalid port.", type="error")
            return(FALSE)
        }
        # Create cookie with new host & port info 
        js$saveHostInfo(input$hostUpdate, input$portUpdate)
        showNotification("Host and port updated. These changes will take effect upon the next page refresh.", type="message")
        return(TRUE)
    })
    
    # Clear cookie for the above settings
    observeEvent(input$clearHostPort, {
        js$clearHostInfo()
        showNotification("Host and port info cleared. Using settings from configuration file. These changes will take effect upon the next page refresh.", type="message")
        return(TRUE)
    })

    # Display enrichment type on title
    titleStatus <- reactiveValues(option=character())
    observeEvent(input$enrich_from, {
        if(input$enrich_from == "View annotations for Tox21 chemicals") {
            # for grammatical reasons
            output$selected_enrich_from <- renderText({
                paste0(input$enrich_from)
            }) 
        } else {
            output$selected_enrich_from <- renderText({
                paste0("Enrich from ", input$enrich_from)
            })  
        }
    })

    # Get list of annotation classes & types from Postgres database
    get_annotations <- function(){
        # Query API to get list of annotation classes and types
        resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getAnnotations"))
        if(resp$status_code != 200){
            return(list())
        }
        outputAnnotationClass <- unlist(lapply(content(resp), function(x) x$annoclassname))
        outputAnnotationType <- unlist(lapply(content(resp), function(x) x$annotype))
        outputAnnotationDesc <- unlist(lapply(content(resp), function(x) x$annodesc))
        outputAnnotationNum <- unlist(lapply(content(resp), function(x) x$numberoftermids))
        outputAnnotations <- data.frame(annoclassname=outputAnnotationClass, annotype=outputAnnotationType, annodesc=outputAnnotationDesc, numberoftermids=outputAnnotationNum, stringsAsFactors=FALSE)
        return(outputAnnotations)
    }
    annoClasses <- reactiveValues(classes=c())
    numTerms <- reactiveValues(count=c())
    totalEnrichmentCount <- reactiveValues(count=0)
    # Get total # of requests
    getEnrichmentCount <- function(){
        tryEnrichmentCount <- tryCatch({
            # Query API to get list of annotation classes and types
            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getTotalRequests"))
            if(resp$status_code != 200){
                return(0)
            }
            return(unname(unlist((content(resp)))))
        }, error=function(cond){
            return(0)
        })
    }
    
    # Display number of total enrichment requests performed
    output$totalEnrichments <- renderUI({
        totalEnrichmentCount$count <- getEnrichmentCount()
        totalEnrichments <- HTML(paste0("<br>Total requests serviced by Tox21 Enricher this year: <b>", totalEnrichmentCount$count, "</b>"))
        return(totalEnrichments)
    })
    
    # Download user manual when link is clicked
    output$manualLink <- downloadHandler(
        filename=function(){
            return(paste0("Tox21Enricher_User_Manual.pdf"))
        },
        content=function(file){
            # First, query API to get most recent version of manual
            resp <- NULL
            tryManual <- tryCatch({
                resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getAppVersion"))
            }, error=function(cond){
                return(NULL)
            })
            if(is.null(tryManual)){
                showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                return(FALSE)
            }
            appVersion <- content(resp)
            # Next, check if we have previously downloaded the manual (Tox21 Enricher will cache previously-downloaded manuals). If yes, do nothing. If no, download the manual from the Plumber server
            # This should always get the most recent manual revision
            dir.create(paste0(tempdir(), "/docs/"))
            tryManualDL <- TRUE
            if(!file.exists(paste0(tempdir(), "/docs/Tox21Enricher_Manual_v", appVersion, ".pdf"))){
                # TODO: error check if download fails
                tryManualDL <- tryCatch({
                    download.file(paste0(API_PROTOCOL, API_ADDR, "/serveManual"), destfile=paste0(tempdir(), "/docs/Tox21Enricher_Manual_v", appVersion, ".pdf"))
                }, error=function(cond){
                    return(NULL)
                })
            }
            if(is.null(tryManualDL)) {
                showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
            } else {
                file.copy(paste0(tempdir(), "/docs/Tox21Enricher_Manual_v", appVersion, ".pdf"), file) 
            }
        }
    )
    searchStatus <- reactiveValues(option=character())
    searchStatus$option <- "search"
    resultsButtonsReactiveList <- reactiveValues(observers=NULL)
    enrichmentListDisplayReactive <- reactiveValues(enrichmentListDisplay=NULL)
    # Open search enrichment menu
    observeEvent(input$searchButton, {
        loadEnrichList()
    })
    # Load list of previous requests
    loadEnrichList <- function(){
        if(searchStatus$option == "search"){
            # destroy previous observers
            lapply(resultsButtonsReactiveList$observers, function(x) x$destroy())
            changePage(page="search")
            updateActionButton(session, "searchButton", label="Perform enrichment", icon=icon("undo"))  
            searchStatus$option <- "enrich"
            
            # Get delete time from server
            resp <- NULL
            deleteTime <- 30 # default 30 days
            cookieExpTime <- 48
            tryDelete <- tryCatch({
                resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getDeleteTime"))
                TRUE
            }, error=function(cond){
                return(FALSE)
            })
            if(!tryDelete){
                showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                return(FALSE)
            } else {
                deleteTime <- unlist(content(resp))
            }
            output[["enrichmentTable"]] <- renderUI(
                column(12,
                    h3("Input the UUID of a previous request"),
                    HTML(paste0("<div class=\"text-danger\"><b>Warning:</b> request results will be deleted from the server after ", deleteTime, " day(s) from their initial posting and may no longer be accessed.</div>")),
                    textInput(inputId="searchPreviousID", label="", placeholder="i.e., XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"),
                    fluidRow(id="searchButtonsMenu",
                        column(12, actionButton("searchPrevButton", label="View results", icon=icon("search")))
                    )
                )
            )
            # Get list of previous requests if exists
            prevSavedSessions <- input$prevSessionId
            prevSavedSessionsList <- c()
            if(!is.null(prevSavedSessions)) {
                prevSavedSessionsList <- unlist(str_split(prevSavedSessions, ";"))
                prevSavedSessionsList <- prevSavedSessionsList[vapply(prevSavedSessionsList, function(x){
                    if(nchar(x) > 0){
                        return(TRUE)
                    }
                    return(FALSE)
                }, FUN.VALUE=logical(1))] # remove empty strings
            }
            output[["prevEnrichmentRecent"]] <- renderUI(
                column(12,
                    h3("View a recently-submitted request"),
                    uiOutput("prevSessionTable")
                )
            )
            if(length(prevSavedSessionsList) < 1) {
                output[["prevSessionTable"]] <- renderUI(
                    HTML("<p><i>No recently-submitted requests found.</i></p>")
                )
                return(FALSE) 
            } else {
                # create list of action buttons for selecting previous requests
                resultsButtons <- lapply(prevSavedSessionsList, function(x) paste0(actionButton(inputId=paste0("prevSessionLink__", x), label="View results", icon=icon("search"))))
                # fetch and calculate expiration dates for cookies and fetch additional info
                prevRequestInfoList <- lapply(prevSavedSessionsList, function(x){
                    tryInfo <- tryCatch({
                        resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getAdditionalRequestInfo"), query=list(transactionId=x))
                        TRUE
                    }, error=function(cond){
                        return(FALSE)
                    })
                    if(!tryInfo){
                        showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                        return(FALSE)
                    } else {
                      if(length(content(resp)) > 0){
                          return(unlist(content(resp), recursive=FALSE))
                      } else {
                          return(NULL)
                      }
                    }
                })
                # Replace NULLs
                prevRequestInfoList <- lapply(prevRequestInfoList, function(x){
                    if(is.null(x)) { # if null (i.e., missing from database), return "dummy" list
                        return(list(
                            "original_mode"="Data missing",
                            "mode"="Data missing",
                            "cutoff"="Data missing",
                            "casrn_box"="Data missing",
                            "timestamp_posted"="Data missing",
                            "timestamp_started"="Data missing",
                            "timestamp_finished"="Data missing",
                            "expiry_date"="Data missing"
                        ))
                    }
                    return(x)
                })
                prevRequestInfoList <- do.call(rbind, prevRequestInfoList)
                if(nrow(prevRequestInfoList) < 1){
                    output[["prevSessionTable"]] <- renderUI(
                        HTML("<p><i>No recently-submitted requests found.</i></p>")
                    )
                    return(FALSE)
                }
                # Format mode column and remove other mode columns from df
                formattedModes <- unlist(lapply(seq_len(nrow(prevRequestInfoList)), function(i){
                    if(prevRequestInfoList[i, "original_mode"] == "similarity" & prevRequestInfoList[i, "mode"] == "similarity") {
                        return("Enrich from chemicals with structural similarity")
                    } else if(prevRequestInfoList[i, "original_mode"] != "similarity" & prevRequestInfoList[i, "mode"] == "casrn") {
                        return("Re-enrich from chemicals with structural similarity")
                    } else if(prevRequestInfoList[i, "original_mode"] == "substructure" & prevRequestInfoList[i, "mode"] == "substructure") {
                        return("Enrich from chemicals with shared substructures")
                    } else if(prevRequestInfoList[i, "original_mode"] != "substructure" & prevRequestInfoList[i, "mode"] == "casrn") {
                        return("Re-enrich from chemicals with shared substructures")
                    } else if(prevRequestInfoList[i, "original_mode"] == "annotation" & prevRequestInfoList[i, "mode"] == "annotation") {
                        return("View annotations for Tox21 chemicals")
                    } else if(prevRequestInfoList[i, "original_mode"] == "casrn" & prevRequestInfoList[i, "mode"] == "casrn") {
                        return("Enrich from user-provided CASRN list")
                    } else {
                        return("Unknown request type")
                    }
                }))
                
                # Include copy ID buttons for each UUID
                prevSavedSessionsList <- unlist(lapply(prevSavedSessionsList, function(x){
                    uuidWithButton <- HTML(paste0(x, "<br>", rclipButton(paste0("copyUUIDButtonPrev__", x), HTML(paste0(icon("clipboard"), " Copy UUID to clipboard")), x)))
                    # Display confirmation message when copy UUID button is pressed
                    observeEvent(input[[paste0("copyUUIDButtonPrev__", x)]], {
                        showNotification(paste0("UUID copied!"), type="message")
                    })
                    return(uuidWithButton)
                }))

                # Drop original_modes and modes columns
                prevRequestInfoList <- subset(prevRequestInfoList, select=c("cutoff", "casrn_box", "timestamp_posted", "timestamp_started", "timestamp_finished", "expiry_date"))
                prevSavedSessionsDF <- data.frame(select=unlist(resultsButtons), uuid=prevSavedSessionsList, formatted_mode=formattedModes, prevRequestInfoList, stringsAsFactors=FALSE)
                colnames(prevSavedSessionsDF) <- c("Select", "Request UUID", "Request Mode", "Node Cutoff", "User Input", "Time Posted", "Time Started", "Time Finished", "Expiration Date")
                # Display data table of prev requests
                output[["prevSessionTable"]] <- renderUI(
                    column(12, style="height:500px; overflow-y:scroll;",
                        DT::datatable({prevSavedSessionsDF},
                            # Render reenrichment table (solution from https://stackoverflow.com/questions/37356625/adding-a-column-with-true-false-and-showing-that-as-a-checkbox/37356792#37356792)
                            caption=HTML(paste0("<p>Select a recently-submitted request from the list below to view its results if it has finished.</p><div class=\"text-danger\">Requests will be cleared from this list after ", cleanupTime$hours, " hour(s) from their initial posting. Results may still be accessed after this period if you know your request's UUID.</div>")),
                            escape=FALSE, 
                            class="row-border stripe compact",
                            rownames=FALSE,
                            style="bootstrap",
                            select="none",
                            options=list( 
                                paging=FALSE,
                                preDrawCallback=JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                drawCallback=JS('function() { Shiny.bindAll(this.api().table().node()); } '),
                                dom="Bfrtip",
                                pageLength=10,
                                buttons=list("copy", "csv", "excel", "pdf", "print")
                            ),
                            extensions="Buttons"
                        )
                    )
                )

                # Load previously saved session if link is clicked
                lapply(prevSavedSessionsList, function(button){
                    transactionId <- unlist(str_split(button, "<br>"))[1] # get transaction ID from button element
                    if(is.null(resultsButtonsReactiveList$buttons[[paste0("prevSessionLink__", transactionId)]])) {
                        resultsButtonsReactiveList$buttons[[paste0("prevSessionLink__", transactionId)]] <- observeEvent(input[[paste0("prevSessionLink__", transactionId)]], {
                            # reset waiting page
                            output[["waitingPage"]] <- renderUI(HTML(""))
                            shinyjs::reset(id="waitingPage")
                            shinyjs::disable(id="waitingPage")
                            output[["waitingTable"]] <- renderUI(HTML(""))
                            shinyjs::reset(id="waitingTable")
                            shinyjs::disable(id="waitingTable")
                            shinyjs::disable(id="clipboard")
                            shinyjs::disable(id="cancelEnrichment")
                            # Disable settings button
                            shinyjs::disable(id="settingsButton")
                            
                            # Check if result (Input/Output) files exist on the server
                            resp <- NULL
                            tryPrevious <- tryCatch({
                                resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/exists"), query=list(transactionId=transactionId))
                                TRUE
                            }, error=function(cond){
                                return(FALSE)
                            })
                            if(!tryPrevious){
                                # error here
                                showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                                return(FALSE)
                            }
                            if(resp$status_code != 200 | content(resp) == -1){
                                # error here
                                showNotification("Error: No such records exist for the provided request.", type="error")
                                return(FALSE)
                            } else {
                                if(content(resp) == 0){
                                    #error here
                                    showNotification("Error: The results for this request are missing on the Tox21 Enricher server. The request may not have completed yet, or it may have been deleted.", type="error")
                                    return(FALSE)
                                }
                            }
                            # Set reactive UUID for this query
                            reactiveTransactionId$id <- transactionId
                            # Check if request has been cancelled - if so, display error message
                            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/isCancel"), query=list(transactionId=transactionId))
                            if(resp$status_code != 200) {
                                showNotification(paste0("Error: Could not fetch data for this transaction."), type="error")
                                return(FALSE)
                            }
                            else {
                                if(content(resp) == 1) {
                                    showNotification(paste0("Error: Transaction is no longer available."), type="error")
                                    return(FALSE)
                                }
                            } 
                            # Get initial queue position
                            initQueuePos <- -1
                            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getQueuePos"), query=list(transactionId=transactionId, mode="init"))
                            if(resp$status_code != 200) {
                                showNotification("Error: Could not fetch queue position for this request.", type="error")
                            } else {
                                initQueuePos <- content(resp)
                                initQueuePos <- gsub("\n", "<br><br>", initQueuePos)
                            }
                            # Get old transaction data
                            transactionData <- NULL
                            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getPrevSessionData"), query=list(transactionId=transactionId))
                            if(resp$status_code != 200) {
                                showNotification(paste0("Error: Could not fetch data for this transaction."), type="error")
                                return(FALSE)
                            } else {
                                transactionData <- content(resp)
                            }
                            # Set variables fetched from database
                            enrichmentType$enrichType <- transactionData$mode[[1]]
                            originalEnrichModeList$originalEnrichMode <- transactionData$original_mode[[1]]
                            splitAnnotationsString <- gsub("=checked,", ", ", transactionData$annotation_selection_string[[1]])
                            cutoff <- transactionData$cutoff[[1]]
                            casrnBox <- transactionData$casrn_box[[1]]
                            casrnBoxSplit <- unlist(str_split(casrnBox, "\n"))
                            casrnBoxSplit <- casrnBoxSplit[nchar(casrnBoxSplit) > 0] # remove any blank lines
                            reenrichFlag <- transactionData$reenrich_flag[[1]]
                            reenrichFlagReactive$reenrichFlagReactive <- reenrichFlag
                            enrichmentDisplayType <- ""
                            if(reenrichFlag == 1){
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
                            # Reset checkboxList$checkboxes and warningcasrns
                            checkboxList$checkboxes <- NULL
                            firstCaseWarningChems$casrns <- NULL
                            # Get colors list
                            colorsList <- unlist(str_split(transactionData$colors[[1]], "\\|"))
                            colorsListSetNames <- lapply(colorsList, function(x) unlist(str_split(x, "__"))[1])
                            colorsListSetItems <- lapply(colorsList, function(x) unlist(str_split(x, "__"))[2])
                            colorsList <- colorsListSetItems
                            names(colorsList) <- colorsListSetNames
                            colorsList <- unlist(colorsList)
                            # Set setColors$color - necessary for preserving color for bargraph generation
                            setColors$color <- colorsList
                            # Set originalNamesList$originalNames
                            originalNamesList$originalNames <- unlist(str_split(transactionData$original_names[[1]], "\\|"))
                            # This is to preserve original names if we want to re-enrich similarity/substructure
                            originalNamesToReturn <- lapply(originalNamesList$originalNames, function(originalName) unlist(str_split(originalName, "__"))[1])
                            names(originalNamesToReturn) <- lapply(originalNamesList$originalNames, function(originalName) unlist(str_split(originalName, "__"))[2])
                            # Set originalEnrichModeList$originalEnrichMode
                            originalEnrichModeList$originalEnrichMode <- transactionData$original_mode[[1]]
                            # reconstruct enrichment sets and set reactive enrichmentSetsList$enrichmentSets variable - required for viewing previous results
                            setNames <- unlist(str_split(transactionData$input[[1]], "\\|"))
                            setNames <- unique(unlist(lapply(setNames, function(x) unlist(str_split(x, "__"))[2])))
                            enrichmentSets <- lapply(setNames, function(x){
                                innerSet <- unlist(lapply(unlist(str_split(transactionData$input[[1]], "\\|")), function(y){
                                    tmpSplit <- unlist(str_split(y, "__"))
                                    if(tmpSplit[2] == x){
                                        return(tmpSplit[1])
                                    }
                                    return(NULL)
                                }))
                                innerSet <- innerSet[!vapply(innerSet, is.null, FUN.VALUE=logical(1))]
                            })
                            names(enrichmentSets) <- setNames
                            enrichmentSetsList$enrichmentSets <- enrichmentSets
                            # Set reenrichresults
                            # If re-enriching, create reenrichResults list
                            reenrichResults <- list()
                            badConnectionFlag <- NULL
                            if(originalEnrichModeList$originalEnrichMode == "substructure" | originalEnrichModeList$originalEnrichMode == "similarity"){
                                reenrichResults <- lapply(seq_len(length(enrichmentSets)), function(casrnSet){
                                    if(originalEnrichModeList$originalEnrichMode == "substructure"){
                                        # Query API to get list of annotation classes and types
                                        resp <- NULL
                                        trySubstructure <- tryCatch({
                                            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/searchBySubstructure"), query=list(input=originalNamesToReturn[[names(enrichmentSets)[casrnSet]]]))
                                            if(resp$status_code != 200){
                                                return(FALSE)
                                            }
                                            TRUE
                                        }, error=function(cond){
                                            return(FALSE)
                                        })
                                        if(!trySubstructure){
                                            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                                            return(FALSE)
                                        }
                                        outputSubCasrns <- unlist(lapply(content(resp), function(x) x$casrn))
                                        outputSubM <- unlist(lapply(content(resp), function(x) x$m))
                                        outputSubCyanide <- unlist(lapply(content(resp), function(x) x$cyanide))
                                        outputSubIsocyanate <- unlist(lapply(content(resp), function(x) x$isocyanate))
                                        outputSubAldehyde <- unlist(lapply(content(resp), function(x) x$aldehyde))
                                        outputSubEpoxide <- unlist(lapply(content(resp), function(x) x$epoxide))
                                        reenrichSubstructureResults <- data.frame(casrn=outputSubCasrns, m=outputSubM, similarity=NA, cyanide=outputSubCyanide, isocyanate=outputSubIsocyanate, aldehyde=outputSubAldehyde, epoxide=outputSubEpoxide, stringsAsFactors=FALSE)
                                        reenrichSubstructureResults <- reenrichSubstructureResults[reenrichSubstructureResults$casrn %in% enrichmentSets[[casrnSet]], ]
                                        return(reenrichSubstructureResults)
                                    } else if(originalEnrichModeList$originalEnrichMode == "similarity"){
                                        # Set Tanimoto threshold for similarity search
                                        threshold <- as.numeric(input$tanimotoThreshold) / 100
                                        resp <- NULL
                                        trySimilarity <- tryCatch({
                                            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/searchBySimilarity"), query=list(input=originalNamesToReturn[[names(enrichmentSets)[casrnSet]]], threshold=threshold))
                                            TRUE
                                        }, error=function(cond){
                                            return(FALSE)
                                        })
                                        if(!trySimilarity){
                                            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                                            return(FALSE)
                                        }
                                        if(resp$status_code != 200){
                                            return(NULL)
                                        }
                                        outputSimCasrns <- unlist(lapply(content(resp), function(x) x$casrn))
                                        outputSimM <- unlist(lapply(content(resp), function(x) x$m))
                                        outputSimSimilarity <- unlist(lapply(content(resp), function(x) x$similarity))
                                        outputSimCyanide <- unlist(lapply(content(resp), function(x) x$cyanide))
                                        outputSimIsocyanate <- unlist(lapply(content(resp), function(x) x$isocyanate))
                                        outputSimAldehyde <- unlist(lapply(content(resp), function(x) x$aldehyde))
                                        outputSimEpoxide <- unlist(lapply(content(resp), function(x) x$epoxide))
                                        reenrichSimilarityResults <- data.frame(casrn=outputSimCasrns, m=outputSimM, similarity=outputSimSimilarity, cyanide=outputSimCyanide, isocyanate=outputSimIsocyanate, aldehyde=outputSimAldehyde, epoxide=outputSimEpoxide, stringsAsFactors=FALSE)
                                        reenrichSimilarityResults <- reenrichSimilarityResults[reenrichSimilarityResults$casrn %in% enrichmentSets[[casrnSet]], ]
                                        return(reenrichSimilarityResults)
                                    }
                                })
                                names(reenrichResults) <- names(enrichmentSets)
                            } else {
                                enrichmentSets <- list()
                                setName <- ""
                                reenrichResults <- lapply(casrnBoxSplit, function(i){
                                    setName <- i
                                    if(setName != ""){
                                        outpSmiles <- NULL
                                        if (enrichmentType$enrichType == "substructure") {
                                            resp <- NULL
                                            trySubstructure <- tryCatch({
                                                resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/searchBySubstructure"), query=list(input=setName))
                                                if(resp$status_code != 200){
                                                    return(FALSE)
                                                }
                                                TRUE
                                            }, error=function(cond){
                                                return(FALSE)
                                            })
                                            if(!is.null(trySubstructure)) {
                                                if(trySubstructure == FALSE){
                                                    badConnectionFlag <<- TRUE
                                                    return(NULL)
                                                }
                                            }
                                            if(length(content(resp)) < 1){
                                                return(NULL)
                                            }    
                                            outputSubCasrns <- unlist(lapply(content(resp), function(x) x$casrn))
                                            outputSubM <- unlist(lapply(content(resp), function(x) x$m))
                                            outputSubCyanide <- unlist(lapply(content(resp), function(x) x$cyanide))
                                            outputSubIsocyanate <- unlist(lapply(content(resp), function(x) x$isocyanate))
                                            outputSubAldehyde <- unlist(lapply(content(resp), function(x) x$aldehyde))
                                            outputSubEpoxide <- unlist(lapply(content(resp), function(x) x$epoxide))
                                            outpSmiles <- data.frame(casrn=outputSubCasrns, m=outputSubM, cyanide=outputSubCyanide, isocyanate=outputSubIsocyanate, aldehyde=outputSubAldehyde, epoxide=outputSubEpoxide, stringsAsFactors=FALSE)
                                        } else if (enrichmentType$enrichType == "similarity") {
                                            # Set Tanimoto threshold for similarity search
                                            threshold <- as.numeric(input$tanimotoThreshold)/100
                                            resp <- NULL
                                            trySimilarity <- tryCatch({
                                                resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/searchBySimilarity"), query=list(input=setName, threshold=threshold))
                                                if(resp$status_code != 200){
                                                    return(FALSE)
                                                }  
                                                TRUE
                                            }, error=function(cond){
                                                return(FALSE)
                                            })
                                            if(!is.null(trySimilarity)){
                                                if(trySimilarity == FALSE){
                                                    badConnectionFlag <<- TRUE
                                                    return(NULL)
                                                }
                                            }
                                            outputSimCasrns <- unlist(lapply(content(resp), function(x) x$casrn))
                                            outputSimM <- unlist(lapply(content(resp), function(x) x$m))
                                            outputSimSimilarity <- unlist(lapply(content(resp), function(x) x$similarity))
                                            outputSimCyanide <- unlist(lapply(content(resp), function(x) x$cyanide))
                                            outputSimIsocyanate <- unlist(lapply(content(resp), function(x) x$isocyanate))
                                            outputSimAldehyde <- unlist(lapply(content(resp), function(x) x$aldehyde))
                                            outputSimEpoxide <- unlist(lapply(content(resp), function(x) x$epoxide))
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
                                    changePage(page="enrichment")
                                    if(badConnectionFlag == TRUE){
                                        showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                                    } else {
                                        showNotification("Error: No valid input sets.", type="error")
                                    }
                                    return(FALSE)
                                }
                                names(reenrichResults) <- unlist(lapply(seq_len(length(casrnBoxSplit)), function(i) paste0("Set", i)))
                                enrichmentSets <- lapply(seq_len(length(reenrichResults)), function(i){
                                    if(!is.null(reenrichResults[[i]])) {
                                        enrichmentSetsInside <- unlist(lapply(seq_len(nrow(reenrichResults[[i]])), function(j) reenrichResults[[i]][j, "casrn"]))
                                    } else {
                                        return(NULL)
                                    }
                                })
                                names(enrichmentSets) <- names(reenrichResults)
                            }
                            reenrichResults <- reenrichResults[!vapply(reenrichResults, is.null, FUN.VALUE=logical(1))]
                            reenrichResultsList$reenrichResults <- reenrichResults
                            # Save input data to use later
                            waitingData <- data.frame("Position"=c(initQueuePos), "Mode"=c(enrichmentType$enrichType), "UUID"=c(transactionId), "Selected Annotations"=c(splitAnnotationsString), "Node Cutoff"=c(cutoff), "Input"=c(casrnBox), stringsAsFactors=FALSE)
                            inputSetList$inputSet <- waitingData
                            # Finally, load results
                            redirectToResultsPage()
                        }, ignoreInit=FALSE, ignoreNULL=TRUE)
                    }
                })
            }
        } else {
            changePage(page="enrichment")
            updateActionButton(session, "searchButton", label="View previous results", icon=icon("search"))  
            searchStatus$option <- "search"
        }
    }
    
    # Search for selected request
    observeEvent(input$searchPrevButton, {
        setToFetch <- input$searchPreviousID
        # Fetch selected transaction
        resp <- NULL
        tryPrevious <- tryCatch({
            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getTransactionDetails"), query=list(uuid=setToFetch))
            TRUE
        }, error=function(cond){
            return(FALSE)
        })
        if(!tryPrevious){
            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
            return(FALSE)
        } else {
            transactionTable <- data.frame(do.call(rbind, content(resp)), stringsAsFactors=FALSE)
        }
        transactionTable <- data.frame(do.call(rbind, content(resp)), stringsAsFactors=FALSE)
        originalMode <- transactionTable[1, "original_mode"]
        mode <- transactionTable[1, "mode"]
        transactionId <- transactionTable[1, "uuid"]
        annoSelectStr <- transactionTable[1, "annotation_selection_string"]
        nodeCutoff <- transactionTable[1, "cutoff"]
        colorsList <- transactionTable[1, "colors"]
      
        # Set enrichmentType$enrichType here to original mode of the request
        enrichmentType$enrichType <- mode
        originalEnrichModeList$originalEnrichMode <- originalMode
        
        # Set originalNamesList$originalNames if similarity/substructure so reenrichment will work correctly
        originalNamesList$originalNames <- unlist(str_split(transactionTable[1, "original_names"], "\\|"))
        
        # Check if result (Input/Output) files exist on the server
        resp <- NULL
        tryPrevious <- tryCatch({
            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/exists"), query=list(transactionId=transactionId))
            TRUE
        }, error=function(cond){
            return(FALSE)
        })
        if(!tryPrevious){
            # error here
            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
            return(FALSE)
        }
        if(resp$status_code != 200 | content(resp) == -1){
            # error here
            showNotification("Error: No such records exist for the provided request.", type="error")
            return(FALSE)
        } else {
            if(content(resp) == 0){
                #error here
                showNotification("Error: The results for this request are missing on the Tox21 Enricher server. The request may not have completed yet, or it may have been deleted.", type="error")
                return(FALSE)
            }
        }
        
        # reconstruct enrichment sets
        setNames <- unlist(str_split(transactionTable[1, "input"], "\\|"))
        setNames <- unlist(lapply(setNames, function(x) unlist(str_split(x, "__"))[2]))
        setNames <- unique(setNames)
        enrichmentSets <- lapply(setNames, function(x){
            innerSet <- unlist(lapply(unlist(str_split(transactionTable[1, "input"], "\\|")), function(y){
                tmpSplit <- unlist(str_split(y, "__"))
                if(tmpSplit[2] == x){
                    return(tmpSplit[1])
                }
                return(NULL)
            }))
            innerSet <- innerSet[!vapply(innerSet, is.null, FUN.VALUE=logical(1))]
        })
        names(enrichmentSets) <- setNames
        originalNames <- unlist(str_split(transactionTable[1, "original_names"], "\\|"))
        reenrichResults <- NULL
        if(!is.na(transactionTable[1, "reenrich"])){
            reenrichResultsSets <- unlist(str_split(transactionTable[1, "reenrich"], "\\|"))
            reenrichResultsCols <- lapply(reenrichResultsSets, function(x) unlist(str_split(x, "__")))
            reenrichResultsMats <- lapply(reenrichResultsCols, function(x){
                innerList <- lapply(seq(2, length(x)), function(y){ # start at 2 to skip set name
                    return(unlist(str_split(x[y], "&")))
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
            reenrichResultsNames <- lapply(reenrichResultsCols, function(x) x[1])
            reenrichResults <- reenrichResultsMats
            names(reenrichResults) <- reenrichResultsNames
        }
        
        # Get colors list
        colorsList <- unlist(str_split(colorsList, "\\|"))
        colorsListSetNames <- lapply(colorsList, function(x) unlist(str_split(x, "__"))[1])
        colorsListSetItems <- lapply(colorsList, function(x) unlist(str_split(x, "__"))[2])
        colorsList <- colorsListSetItems
        names(colorsList) <- colorsListSetNames
        colorsList <- unlist(colorsList)
        shinyjs::hide(id="searchForm")
        shinyjs::disable(id="searchForm")
        shinyjs::hide(id="searchButtonsMenu")
        shinyjs::disable(id="searchButtonsMenu")
        # Set enrichmentSetsList$enrichmentSets - necessary for being able to regenerate network
        enrichmentSetsList$enrichmentSets <- enrichmentSets
        # Set setColors$color - necessary for preserving color for bargraph generation
        setColors$color <- colorsList
        enrichmentResults(mode, transactionId, annoSelectStr, nodeCutoff, enrichmentSets, originalNames, reenrichResults, originalMode, colorsList)
    })
    
    # update theme data when checkbox is clicked
    observeEvent(input$changeThemeToggle, {
        # Reset Venn diagrams on theme change
        output[["vennChart"]] <- renderPlot({})
        output[["vennCluster"]] <- renderPlot({})
        output[["vennChartButtons"]] <- renderUI({})
        output[["vennClusterButtons"]] <- renderUI({})
        shinyjs::hide(id="vennChart")
        shinyjs::hide(id="vennCluster")
        shinyjs::hide(id="vennChartButtons")
        shinyjs::hide(id="vennClusterButtons")
        shinyjs::hide(id="vennChartMenu")
        shinyjs::hide(id="vennClusterMenu")
        shinyjs::hide(id="nodeLinkChartMenu")
        shinyjs::hide(id="nodeLinkClusterMenu")
        if(input$changeThemeToggle == "Dark"){ # dark
            theme$textcolor <- "#FFFFFF"
            js$saveSessionThemePreferred("dark")
            js$initDarkTheme("dark")
        } else if (input$changeThemeToggle == "Light") { # light
            theme$textcolor <- "#000000"
            js$saveSessionThemePreferred("light")
            js$initDarkTheme("light")
        } else if (input$changeThemeToggle == "Auto") { # auto
            js$saveSessionThemePreferred("auto")
            js$initDarkTheme("default")
            if(input$sessionTheme == "dark"){
                theme$textcolor="#FFFFFF"
            } else if(input$sessionTheme == "light"){
                theme$textcolor="#000000"
            }
        }
        # Fix for DT::DataTable labels showing up as the wrong color
        output[["themeStatus"]] <- renderUI({
            tags$style(HTML(paste0('
                .dataTables_length label, .dataTables_filter label, .dataTables_info {
                    color: ', theme$textcolor, '!important;
                }
            ')))
        })
    }, ignoreInit=TRUE, ignoreNULL=TRUE)
    
    # Display list of annotations to select
    output$annotations <- renderUI({
        annoListFull <- list()
        getAnnotationsFromServer <- tryCatch({
            annoListFull <- get_annotations()
        }, error=function(cond){
            return(HTML("<div class=\"text-danger\">Error: Could not load any annotation classes.</div>"))
        })
        if(length(annoListFull) < 1) {
            return(HTML("<div class=\"text-danger\">Error: Could not load any annotation classes.</div>"))
        }
        
        # Get list of number of terms for each class
        numOfTermsList <- annoListFull$numberoftermids
        names(numOfTermsList) <- annoListFull$annoclassname
        annoList <- annoListFull[1]
        # Lists for each class type
        # PubChem
        classPubChem <- annoListFull[annoListFull$annotype %in% c("PubChem Compound Annotation"), ]
        classPubChem <- classPubChem[, "annoclassname"]
        descPubChem <- annoListFull[annoListFull$annotype %in% c("PubChem Compound Annotation"), ]
        descPubChem <- descPubChem[, "annodesc"]
        descPubChem <- unlist(lapply(seq_len(length(descPubChem)), function(x) paste0(descPubChem[x])))
        # DrugMatrix
        classDrugMatrix <- annoListFull[annoListFull$annotype %in% c("DrugMatrix Annotation"), ]
        classDrugMatrix <- classDrugMatrix[, "annoclassname"]
        descDrugMatrix <- annoListFull[annoListFull$annotype %in% c("DrugMatrix Annotation"), ]
        descDrugMatrix <- descDrugMatrix[, "annodesc"]
        descDrugMatrix <- unlist(lapply(seq_len(length(descDrugMatrix)), function(x) paste0(descDrugMatrix[x])))
        # DrugBank
        classDrugBank <- annoListFull[annoListFull$annotype %in% c("DrugBank Annotation"), ]
        classDrugBank <- classDrugBank[, "annoclassname"]
        descDrugBank <- annoListFull[annoListFull$annotype %in% c("DrugBank Annotation"), ]
        descDrugBank <- descDrugBank[, "annodesc"]
        descDrugBank <- unlist(lapply(seq_len(length(descDrugBank)), function(x) paste0(descDrugBank[x])))
        # CTD
        classCTD <- annoListFull[annoListFull$annotype %in% c("CTD Annotation"), ]
        classCTD <- classCTD[, "annoclassname"]
        descCTD <- annoListFull[annoListFull$annotype %in% c("CTD Annotation"), ]
        descCTD <- descCTD[, "annodesc"]
        descCTD <- unlist(lapply(seq_len(length(descCTD)), function(x) paste0(descCTD[x])))
        # Other
        classOther <- annoListFull[annoListFull$annotype %in% c("Other"), ]
        classOther <- classOther[, "annoclassname"]
        descOther <- annoListFull[annoListFull$annotype %in% c("Other"), ]
        descOther <- descOther[, "annodesc"]
        descOther <- unlist(lapply(seq_len(length(descOther)), function(x) paste0(descOther[x])))
        annodescList <- annoListFull[, "annodesc"]
        names(annodescList) <- annoListFull[, "annoclassname"]
        # Create list of annotation tooltips (help from https://stackoverflow.com/questions/36670065/tooltip-in-shiny-ui-for-help-text)
        ttPubChem <- lapply(seq_len(length(descPubChem)), function(x) tipify(bsButtonRight(paste0("tt_", classPubChem[x]), icon("question-circle"), style="inverse", size="extra-small"), descPubChem[x], placement="right"))
        names(ttPubChem) <- classPubChem
        ttDrugMatrix <- lapply(seq_len(length(descDrugMatrix)), function(x) tipify(bsButtonRight(paste0("tt_", classDrugMatrix[x]), icon("question-circle"), style="inverse", size="extra-small"), descDrugMatrix[x], placement="right"))
        names(ttDrugMatrix) <- classDrugMatrix
        ttDrugBank <- lapply(seq_len(length(descDrugBank)), function(x) tipify(bsButtonRight(paste0("tt_", classDrugBank[x]), icon("question-circle"), style="inverse", size="extra-small"), descDrugBank[x], placement="right"))
        names(ttDrugBank) <- classDrugBank
        ttCTD <- lapply(seq_len(length(descCTD)), function(x) tipify(bsButtonRight(paste0("tt_", classCTD[x]), icon("question-circle"), style="inverse", size="extra-small"), descCTD[x], placement="right"))
        names(ttCTD) <- classCTD
        ttOther <- lapply(seq_len(length(descOther)), function(x) tipify(bsButtonRight(paste0("tt_", classOther[x]), icon("question-circle"), style="inverse", size="extra-small"), descOther[x], placement="right"))
        names(ttOther) <- classOther
        
        # Merge all into one list
        annoClassList <- list(classPubChem, classDrugMatrix, classDrugBank, classCTD, classOther)
        annoClasses$classes <- annoClassList
        numTerms$count <- numOfTermsList
        
        # Sort classes by number of annotations
        sortedNumOfTermsList <- sort(numOfTermsList, decreasing=TRUE)
        classPubChem <- classPubChem[order(match(classPubChem, names(sortedNumOfTermsList)))]
        classDrugMatrix <- classDrugMatrix[order(match(classDrugMatrix, names(sortedNumOfTermsList)))]
        classDrugBank <- classDrugBank[order(match(classDrugBank, names(sortedNumOfTermsList)))]
        classCTD <- classCTD[order(match(classCTD, names(sortedNumOfTermsList)))]
        classOther <- classOther[order(match(classOther, names(sortedNumOfTermsList)))]
        
        # Sort lists of tooltips to match above prder
        ttPubChem <- ttPubChem[order(match(names(ttPubChem), names(sortedNumOfTermsList)))]
        ttDrugMatrix <- ttDrugMatrix[order(match(names(ttDrugMatrix), names(sortedNumOfTermsList)))]
        ttDrugBank <- ttDrugBank[order(match(names(ttDrugBank), names(sortedNumOfTermsList)))]
        ttCTD <- ttCTD[order(match(names(ttCTD), names(sortedNumOfTermsList)))]
        ttOther <- ttOther[order(match(names(ttOther), names(sortedNumOfTermsList)))]
        
        # Add numbers of terms to names
        # PubChem
        labelPubChem <- lapply(classPubChem, function(x) HTML(paste0(x, " <sub>(", numOfTermsList[x], " annotations)</sub>")))
        # DrugMatrix
        labelDrugMatrix <- lapply(classDrugMatrix, function(x) HTML(paste0(x, " <sub>(", numOfTermsList[x], " annotations)</sub>")))
        # DrugBank
        labelDrugBank <- lapply(classDrugBank, function(x) HTML(paste0(x, " <sub>(", numOfTermsList[x], " annotations)</sub>")))
        # CTD
        labelCTD <- lapply(classCTD, function(x){
            # Show warning for CTD_GOFAT_BIOPROCESS - not included by default due to massive size
            if(x == "CTD_GOFAT_BIOPROCESS"){
                return(HTML(paste0("CTD_GOFAT_BIOPROCESS (<b class=\"text-danger\">Very slow, unchecked by default</b>) <sub>(", numOfTermsList[x], " annotations)</sub>")))
            }
            HTML(paste0(x, " <sub>(", numOfTermsList[x], " annotations)</sub>"))
        })
        classCTDSelected <- lapply(classCTD, function(x){
            if(grepl("^CTD_GOFAT_BIOPROCESS", x)){
                return(NULL)
            } else {
                return(x)
            }
        })
        names(classCTDSelected) <- names(classCTD)
        # Other
        labelOther <- lapply(classOther, function(x) HTML(paste0(x, " <sub>(", numOfTermsList[x], " annotations)</sub>")))
        
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
                            extendedCheckboxGroupInput("checkboxPubChem", "PubChem Compound Annotations", width="400px", choiceNames=labelPubChem, choiceValues=classPubChem, selected=classPubChem, extensions=ttPubChem)
                        ),
                        column(12,
                            actionButton(inputId="selectPubChem", label="Deselect all PubChem annotations")
                        )
                    )
                ),
                tabPanel("DrugMatrix Annotations",
                    fluidRow(
                        column(12,
                            extendedCheckboxGroupInput("checkboxDrugMatrix", "DrugMatrix Annotations", width="400px", choiceNames=labelDrugMatrix, choiceValues=classDrugMatrix, selected=classDrugMatrix, extensions=ttDrugMatrix)
                        ),
                        column(12,
                            actionButton(inputId="selectDrugMatrix", label="Deselect all DrugMatrix annotations")
                        )
                    )
                ),
                tabPanel("DrugBank Annotations",
                    fluidRow(
                        column(12,
                            extendedCheckboxGroupInput("checkboxDrugBank", "DrugBank Annotations", width="400px", choiceNames=labelDrugBank, choiceValues=classDrugBank, selected=classDrugBank, extensions=ttDrugBank)
                        ),
                        column(12,
                            actionButton(inputId="selectDrugBank", label="Deselect all DrugBank annotations")
                        )
                    )
                ),
                tabPanel("CTD Annotations",
                    fluidRow(
                        column(12,
                            extendedCheckboxGroupInput("checkboxCTD", "CTD Annotations", width="700px", choiceNames=labelCTD, choiceValues=classCTD, selected=classCTDSelected, extensions=ttCTD)
                        ),
                        column(12,
                            actionButton(inputId="selectCTD", label="Deselect all CTD annotations")
                        )
                    )
                ),
                tabPanel("Other Annotations",
                    fluidRow(
                        column(12,
                            extendedCheckboxGroupInput("checkboxOther", "Other Annotations", width="400px", choiceNames=labelOther, choiceValues=classOther, selected=classOther, extensions=ttOther)
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
        btn$attribs$style <- "float: right;"
        btn
    }
    extendedCheckboxGroupInput <- function(..., extensions=list()) {
        cbg <- checkboxGroupInput(...)
        nExtensions <- length(extensions)
        nChoices <- length(cbg$children[[2]]$children[[1]])
        if (nExtensions > 0 & nChoices > 0) {
            lapply(seq_len(min(nExtensions, nChoices)), function(i) {
                # For each Extension, add the element as a child (to one of the checkboxes)
                cbg$children[[2]]$children[[1]][[i]]$children[[2]] <<- extensions[[i]]
            })
        }
        return(cbg)
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
            shinyjs::show(id="casrnExamples")
            shinyjs::hide(id="smilesExamples")
            shinyjs::enable(id="nodeCutoff")
            shinyjs::hide(id="tanimotoThreshold")
            shinyjs::disable(id="includeChemsWithWarnings")
            output[["inputInstructions"]] <- renderUI(
                p("Add \"#SetName\" before each set if using multiple sets at once. Set names may only be alphanumeric characters (A-Z, a-z, and 0-9) and spaces are ignored.")
            )
        } else if (input$enrich_from == "View annotations for Tox21 chemicals") {
            enrichmentType$enrichType <- "annotation"
            shinyjs::show(id="casrnExamples")
            shinyjs::hide(id="smilesExamples")
            # Disable node cutoff slider
            shinyjs::disable(id="nodeCutoff")
            shinyjs::hide(id="tanimotoThreshold")
            shinyjs::disable(id="includeChemsWithWarnings")
            output[["inputInstructions"]] <- renderUI(
                p("Enter the CASRNs for ", tags$a(href="https://comptox.epa.gov/dashboard/chemical_lists/TOX21SL", "chemicals in the Tox21 screening library"),  " (one per line) to view each of their associated annotations in Tox21 Enricher. Add \"#SetName\" before each set if using multiple sets at once. Set names may only be alphanumeric characters (A-Z, a-z, and 0-9) and spaces are ignored.")
            )
        } else {
            if(input$enrich_from == "chemicals with shared substructures") {
                enrichmentType$enrichType <- "substructure"
                shinyjs::hide(id="tanimotoThreshold")
            } else {
                enrichmentType$enrichType <- "similarity"
                shinyjs::show(id="tanimotoThreshold")
            }
            shinyjs::hide(id="casrnExamples")
            shinyjs::show(id="smilesExamples")
            shinyjs::enable(id="nodeCutoff")
            shinyjs::enable(id="includeChemsWithWarnings")
            output[["inputInstructions"]] <- renderUI(
                HTML(paste0("Enter partial or complete SMILES or InChI strings, one per line. <div class='text-warning'>Warning: Chemicals that contain metals may not produce expected results.</div>"))
            )
        }
    })
    
    # Toggle Select/Deselect all for annotation classes
    selectStatus <- reactiveValues(option=character())
    selectStatus$option <- "deselect"
    observeEvent(input$select_all_annotations, {
        # Grab list of annotation classes
        # 1 = PubChem
        # 2 = DrugMatrix
        # 3 = DrugBank
        # 4 = CTD
        # 5 = Other
        annoClassList <- annoClasses$classes
        if(selectStatus$option == "deselect") {
            updateCheckboxGroupInput(session, "checkboxPubChem", selected="")
            updateCheckboxGroupInput(session, "checkboxDrugMatrix", selected="")
            updateCheckboxGroupInput(session, "checkboxDrugBank", selected="")
            updateCheckboxGroupInput(session, "checkboxCTD", selected="")
            updateCheckboxGroupInput(session, "checkboxOther", selected="")
            updateActionButton(session, "select_all_annotations", label="Select all")
            selectStatus$option <- "select"
        } else {
            updateCheckboxGroupInput(session, "checkboxPubChem", selected=annoClassList[[1]])
            updateCheckboxGroupInput(session, "checkboxDrugMatrix", selected=annoClassList[[2]])
            updateCheckboxGroupInput(session, "checkboxDrugBank", selected=annoClassList[[3]])
            updateCheckboxGroupInput(session, "checkboxCTD", selected=annoClassList[[4]])
            updateCheckboxGroupInput(session, "checkboxOther", selected=annoClassList[[5]])
            updateActionButton(session, "select_all_annotations", label="Deselect all")
            selectStatus$option <- "deselect"
        }
    })
    # Select/Deselect all annotation classes with a large number of terms (> 1000)
    selectLargeStatus <- reactiveValues(option=character())
    selectLargeStatus$option <- "deselect"
    observeEvent(input$selectAllLarge, {
        # Grab list of annotation classes
        annoClassList <- annoClasses$classes
        # Grab list of numbers of terms per annotation class
        numTermsPerClass <- numTerms$count
        bigClasses <- lapply(names(numTermsPerClass), function(x){
            if(numTermsPerClass[x] > 1000) {
                return(x)
            }
            return(NULL)
        })
        bigClasses <- bigClasses[!vapply(bigClasses, is.null, FUN.VALUE=logical(1))]
        bigSelectedPubChem <- annoClassList[[1]][annoClassList[[1]] %in% bigClasses]
        bigSelectedDrugMatrix <- annoClassList[[2]][annoClassList[[2]] %in% bigClasses]
        bigSelectedDrugBank <- annoClassList[[3]][annoClassList[[3]] %in% bigClasses]
        bigSelectedCTD <- annoClassList[[4]][annoClassList[[4]] %in% bigClasses]
        bigSelectedOther <- annoClassList[[5]][annoClassList[[5]] %in% bigClasses]
        if(selectLargeStatus$option == "deselect") {
            updateCheckboxGroupInput(session, "checkboxPubChem", selected=input$checkboxPubChem[!(input$checkboxPubChem %in% bigSelectedPubChem)])
            updateCheckboxGroupInput(session, "checkboxDrugMatrix", selected=input$checkboxDrugMatrix[!(input$checkboxDrugMatrix %in% bigSelectedDrugMatrix)])
            updateCheckboxGroupInput(session, "checkboxDrugBank", selected=input$checkboxDrugBank[!(input$checkboxDrugBank %in% bigSelectedDrugBank)])
            updateCheckboxGroupInput(session, "checkboxCTD", selected=input$checkboxCTD[!(input$checkboxCTD %in% bigSelectedCTD)])
            updateCheckboxGroupInput(session, "checkboxOther", selected=input$checkboxOther[!(input$checkboxOther %in% bigSelectedOther)])
            updateActionButton(session, "selectAllLarge", label="Select all large classes")
            selectLargeStatus$option <- "select"
        } else {
            updateCheckboxGroupInput(session, "checkboxPubChem", selected=append(input$checkboxPubChem, bigSelectedPubChem))
            updateCheckboxGroupInput(session, "checkboxDrugMatrix", selected=append(input$checkboxDrugMatrix, bigSelectedDrugMatrix))
            updateCheckboxGroupInput(session, "checkboxDrugBank", selected=append(input$checkboxDrugBank, bigSelectedDrugBank))
            updateCheckboxGroupInput(session, "checkboxCTD", selected=append(input$checkboxCTD, bigSelectedCTD))
            updateCheckboxGroupInput(session, "checkboxOther", selected=append(input$checkboxOther, bigSelectedOther))
            updateActionButton(session, "selectAllLarge", label="Deselect all large classes")
            selectLargeStatus$option <- "deselect"
        }
    })
    
    # Provide CASRNs example set (single) when button is clicked
    observeEvent(input$example_casrns, {
        updateTextAreaInput(session, "submitted_chemicals", value="965-90-2\n50-50-0\n979-32-8\n4245-41-4\n143-50-0\n17924-92-4\n297-76-7\n152-43-2\n313-06-4\n4956-37-0\n112400-86-9")
    })
    # Provide CASRNs example set (multiple) when button is clicked
    observeEvent(input$example_casrnsMulti, {
        updateTextAreaInput(session, "submitted_chemicals", value="#BPA analogs\n2081-08-5\n2467-02-9\n1478-61-1\n41481-66-7\n5613-46-7\n57-63-6\n620-92-8\n77-40-7\n79-94-7\n79-95-8\n79-97-0\n80-05-7\n80-09-1\n843-55-0\n94-18-8\n#Flame retardants\n115-86-6\n115-96-8\n1241-94-7\n1330-78-5\n13674-87-8\n29761-21-5\n5436-43-1\n56803-37-3\n68937-41-7\n78-30-8\n79-94-7\n#PAH\n120-12-7\n129-00-0\n191-24-2\n206-44-0\n218-01-9\n50-32-8\n53-70-3\n56-55-3\n83-32-9\n85-01-8\n")
    })
    # Provide SMILES example set when button is clicked
    observeEvent (input$example_smiles, {
        updateTextAreaInput(session, "submitted_chemicals", value="CC(=O)C1=CC=C(C=C1)[N+]([O-])\nClCC1=CC=CC=C1\nCN(C)C1=CC=C(C=C1)\n")
    })
    # Clear CASRNs input box
    observeEvent(input$clear_casrns, {
        updateTextAreaInput(session, "submitted_chemicals", value="")
    })
    # Show/hide JSME input
    jsmeState <- reactiveValues(jsmeShowState="")
    jsmeState$jsmeShowState <- "show"
    observeEvent(input$jsme_button, {
        if (jsmeState$jsmeShowState == "show") {
            shinyjs::show("jsmeInput")
            updateActionButton(session, "jsme_button", label="Hide JSME")
            jsmeState$jsmeShowState <- "hide"
        } else {
            shinyjs::hide("jsmeInput")
            updateActionButton(session, "jsme_button", label="Draw molecules with JSME")
            jsmeState$jsmeShowState <- "show"
        }
    })

    # Flag to see if request has finished
    finishedFlag <- reactiveValues(finished=FALSE)
    # Reactive value to keep track of last queue position
    queuePosOld <- reactiveValues(pos="")
    # Automatically update waiting page/queue position in the background
    updateWaitingTable <- function(){
        transactionId <- reactiveTransactionId$id
        queuePos <- -1
        waitingData <- inputSetList$inputSet
        enrichmentDisplayType <- waitingData[1, "Mode"]
        splitAnnotationsString <- waitingData[1, "Selected.Annotations"]
        cutoff <- waitingData[1, "Node.Cutoff"]
        casrnBox <- waitingData[1, "Input"]
        resp <- NULL
        tryRefresh <- tryCatch({
            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getQueuePos"), query=list(transactionId=transactionId, mode=enrichmentDisplayType))
            TRUE
        }, error=function(cond){
            return(FALSE)
        })
        if(!tryRefresh){
            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
            return(FALSE)
        }
        if(resp$status_code != 200) {
            showNotification("Error: Could not fetch queue position for this request.", type="error")
        } else {
            queuePos <- content(resp)
            queuePos <- gsub("\n", "<br><br>", queuePos)
        }
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
        
        # Check if request has finished and set flag to TRUE if it has
        if(queuePos == "Complete!"){
            finishedFlag$finished <- TRUE
        }
        # Only update if queue position is now different than before
        if(queuePos != queuePosOld$pos){
            queuePosOld$pos <- queuePos
            # update waitingData with cleaned output
            waitingData <- data.frame("Position"=c(queuePos), "Mode"=c(enrichmentDisplayType), "UUID"=c(transactionId), "Selected Annotations"=c(splitAnnotationsString), "Node Cutoff"=c(cutoff), "Input"=c(casrnBox), stringsAsFactors=FALSE)
            # Clean up column name formatting
            colnames(waitingData) <- list("Status", "Request Mode", "Request UUID", "Selected Annotations", "Node Cutoff", "User Input")
            # Update table
            output[["waitingTable"]] <- renderUI(
                column(12,
                    DT::datatable({waitingData},
                        escape=FALSE,
                        rownames=FALSE,
                        class="row-border stripe compact",
                        style="bootstrap",
                        select="none",
                        options=list(
                            autoWidth=TRUE
                        )
                    )
                )
            )
        }
    }
    
    # Copies transaction ID to user's clipboard
    output$clipboard <- renderUI({
        rclipButton("copyUUIDButton", HTML(paste0(icon("clipboard"), " Copy UUID to clipboard")), reactiveTransactionId$id)
    })
    # Display confirmation message when copy UUID button is pressed
    observeEvent(input$copyUUIDButton, {
        showNotification(paste0("UUID copied!"), type="message")
    })
    
    # Refresh enrichment form
    observeEvent(input$refresh, {
        # Initialize all annotation classes to checked and button mode to "deselect"
        annoClassList=annoClasses$classes
        updateCheckboxGroupInput(session, "checkboxPubChem", selected=annoClassList[[1]])
        updateCheckboxGroupInput(session, "checkboxDrugMatrix", selected=annoClassList[[2]])
        updateCheckboxGroupInput(session, "checkboxDrugBank", selected=annoClassList[[3]])
        updateCheckboxGroupInput(session, "checkboxCTD", selected=annoClassList[[4]])
        updateCheckboxGroupInput(session, "checkboxOther", selected=annoClassList[[5]])
        updateActionButton(session, "select_all_annotations", label="Deselect all")
        selectStatus$option <- "deselect"
        totalEnrichmentCount$count <- getEnrichmentCount() # Get new total count
        shinyjs::reset(id="enrichmentForm")
        shinyjs::reset(id="resultsContainer")
        shinyjs::reset(id="sidebar")
        shinyjs::reset(id="apiConnection")
        shinyjs::reset(id="totalEnrichments")
        shinyjs::reset(id="select_all_annotations")
        shinyjs::reset(id="enrich_from")
        shinyjs::reset(id="submitted_chemicals")
        changePage(page="enrichment")
        # Reset enrichmentType$enrichType back to casrn
        enrichmentType$enrichType <- "casrn"
        # Clear the chemical submission text area
        updateTextAreaInput(session, "submitted_chemicals", value="")
        updateActionButton(session, "searchButton", label="View previous results", icon=icon("search"))
        searchStatus$option <- "search"
        # Reset reenrichResultsList$reenrichResults
        reenrichResultsList$reenrichResults <- NULL
        # Reset enrichmentSetsList$enrichmentSets
        enrichmentSetsList$enrichmentSets <- NULL
        # Reset setColors$color
        setColors$color <- NULL
        # Reset setFilesObservers$observers
        lapply(setFilesObservers$observers, function(x) x$destroy())
        setFilesObservers$observers <- NULL
        output[["resultsTabset"]] <- renderUI(
            div(id="resultsTmp")
        )
    })
    
    # Keep track of what enrichment type is currently selected
    enrichmentType <- reactiveValues(enrichType=character())
    enrichmentType$enrichType <- "casrn"
    
    # Perform CASRN enrichment analysis when submit button is pressed
    observeEvent(input$submit, {
        # First, check if user filled out text input form  
        # Remove whitespace for checking
        validatedInput <- gsub("\\s*", "", input$submitted_chemicals)
        if(validatedInput == "") {
            showNotification("Error: No input lines.", type="error")
            return(FALSE)
        }
        casrnValidatedInput <- ""
        errorCasrns <- list()
        inputToSubmit <- input$submitted_chemicals
        
        # Validate Input
        # 1) strip horizontal whitespace and condense multiple newlines
        casrnValidatedInput <- gsub(" ", "", input$submitted_chemicals, fixed=TRUE)
        casrnValidatedInput <- gsub("\\t", "", casrnValidatedInput)
        casrnValidatedInput <- gsub("\\n+", "\n", casrnValidatedInput)
  
        # If CASRN input, do the following:
        if(enrichmentType$enrichType == "casrn" | enrichmentType$enrichType == "annotation") {
            # 2a) check if of the form ###-###-### or setname
            casrnValidatedInput <- unlist(str_split(casrnValidatedInput, "\n"))
            casrnValidatedInput <- casrnValidatedInput[vapply(casrnValidatedInput, function(x){
                if(nchar(x) > 0){
                    return(TRUE)
                }
                return(FALSE)
            }, FUN.VALUE=logical(1))]
          
            # Generate list of indices with bad CASRNs
            errorCasrns <- lapply(seq_len(length(casrnValidatedInput)), function(i) {
                if(!grepl("^#[A-Za-z0-9]+|[0-9]+-[0-9]+-[0-9]+|(NOCAS_[0-9]+)", casrnValidatedInput[i], ignore.case=TRUE)) {
                    return(i)
                }
                return(NULL)
            })
            errorCasrns <- errorCasrns[!vapply(errorCasrns, is.null, FUN.VALUE=logical(1))]
              
            # If there are errors
            if(length(errorCasrns) > 0){
                showNotification(HTML(paste0("<div class=\"text-danger\">Error: Incorrect CASRN or set name formatting on input line(s): <b>", paste0(errorCasrns, collapse=", "), "</b>. Please check your input and try again.</div>")) , type="error")
                return(FALSE) 
            }
            inputToSubmit <- paste0(casrnValidatedInput, collapse="\n")
            
            # 3a) check if missing first set name, if you are using set names
            usingSetNames <- FALSE
            checkSets <- lapply(seq_len(length(casrnValidatedInput)), function(i) {
                if(grepl("^#[A-Za-z0-9]+", casrnValidatedInput[i], ignore.case=TRUE)) { # Detect if we are using set names
                    return(TRUE)
                }
                return(NULL)
            })
            checkSets <- checkSets[!vapply(checkSets, is.null, FUN.VALUE=logical(1))]
            if(length(checkSets) > 0){
                usingSetNames = TRUE
            }
            if(!grepl("^#[A-Za-z0-9]+", casrnValidatedInput[1], ignore.case=TRUE) & usingSetNames == TRUE) {
                showNotification("Error: It appears you are using set names but have not provided a name for the first input set. Please check your input and try again.", type="error")
                return(FALSE)
            }
            
            # 4a) check if duplicate set names
            setNamesList <- unlist(lapply(seq_len(length(casrnValidatedInput)), function(i) {
                if(grepl("^#[A-Za-z0-9]+", casrnValidatedInput[i], ignore.case=TRUE)) { # Detect if we are using set names
                    return(casrnValidatedInput[i])
                }
                return(NULL)
            }))
            setNamesList <- setNamesList[!vapply(setNamesList, is.null, FUN.VALUE=logical(1))]
            setNamesDuplicate <- as.data.frame(table(setNamesList)) %>% filter(Freq > 1)
            if(nrow(setNamesDuplicate) > 0) { # if we have duplicate names, display error message
                showNotification(paste0("Error: Duplicate set names are not allowed: ", paste0(setNamesDuplicate$setNamesList, collapse=", ")), type="error")
                return(FALSE)
            }
        }
        if(!performEnrichment(inputToSubmit, reenrichFlag=FALSE)){
            changePage(page="enrichment")
        }
    })
    # Create reactive variable to store chemicals to re-enrich
    checkboxList <- reactiveValues(checkboxes=NULL)
    finalTableToDisplay <- reactiveValues(table=NULL)
    # Create reactive variable to store reenrich flag
    reenrichFlagReactive <- reactiveValues(reenrichFlag=FALSE)
    # Create reactive variable to store chemicals with reactive structure warnings
    warningList <- reactiveValues(warnings=NULL)
    # Create reactive variable to store data related to input set
    inputSetList <- reactiveValues(inputSet=NULL)
    enrichmentSetsList <- reactiveValues(enrichmentSets=NULL)
    # Create reactive variable to store original input set names
    originalNamesList <- reactiveValues(originalNames=NULL)
    # Create reactive variable to store original enrichment mode for re-enrichment
    originalEnrichModeList <- reactiveValues(originalEnrichMode=NULL)
    reenrichResultsList <- reactiveValues(reenrichResults=NULL)
    # Create reactive variable to store transaction ID
    reactiveTransactionId <- reactiveValues(id=NULL)
    # Create reactive variables to store data related to bargraphs
    bgChartFullReactive <- reactiveValues(bgChartFull=NULL)
    bgChartAllCategoriesReactive <- reactiveValues(bgChartAllCategories=NULL)
    # Create reactive variable to store color data for plotting for each input set
    setColors <- reactiveValues(color=NULL)
    # Flag to see if waiting table has loaded
    tableLoaded <- reactiveValues(loaded=FALSE)
    
    # Main function that handles the enrichment process once a request is submitted
    performEnrichment <- function(casrnBox, reenrichFlag=FALSE, originalNamesToReturn=NULL) {
        # Display waiting page
        changePage(page="waiting")
      
        # Generate UUID for this query and access API to see if UUID already exists
        transactionId <- NULL
        resp <- NULL
        tryGenerateUUID <- tryCatch({
            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/checkId"))
            if(resp$status_code != 200){
                return(FALSE)
            }
            TRUE
        }, error=function(cond){
            return(FALSE)
        })
        if(!tryGenerateUUID) {
            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
            return(FALSE)
        } else {
            transactionId <- unlist(content(resp))
        }
        # If UUID is good, assign to reactiveValue
        reactiveTransactionId$id <- transactionId
        # Save session (transaction) ID to cookie
        js$saveSession(transactionId, cleanupTime$hours)
        # If reenriching, always do CASRN
        if(reenrichFlag == FALSE){
            reenrichFlagReactive$reenrichFlagReactive <- FALSE
            originalEnrichModeList$originalEnrichMode <- enrichmentType$enrichType
        }
        else {
            reenrichFlagReactive$reenrichFlagReactive <- TRUE
            enrichmentType$enrichType <- "casrn"
        }
        # Reset checkboxList$checkboxes and warningcasrns
        checkboxList$checkboxes <- NULL
        firstCaseWarningChems$casrns <- NULL
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
                enrichmentSets <- lapply(enrichmentSets, function(i) unlist(str_split(i, "\n")))
                names(enrichmentSets) <- lapply(enrichmentSets, function(i) i[1])
                # Remove names from inside each list
                enrichmentSets <- lapply(enrichmentSets, function(i) i[-1])
                enrichmentSets <- lapply(enrichmentSets, function(i){
                    tmp <- unlist(lapply(i, function(j){
                        if(nchar(j) < 1) {
                            return(NULL)
                        }
                        return(j)
                    }))
                    tmp <- tmp[!vapply(tmp, is.null, FUN.VALUE=logical(1))]
                    if(length(tmp) < 1) {
                        return(NULL)
                    } else {
                        return(tmp)
                    }
                })
                enrichmentSets <- enrichmentSets[!vapply(enrichmentSets, is.null, FUN.VALUE=logical(1))]
            } else { # Give arbitrary name if user only submitted a single set
                enrichmentSets <- str_split(casrnBox, "\n")
                names(enrichmentSets) <- list("Set1")
            }
            # Remove empty elements
            enrichmentSets <- lapply(enrichmentSets, function(i) i[lapply(i, nchar) > 0])
            # If re-enriching, create reenrichResults list
            if(originalEnrichModeList$originalEnrichMode == "substructure" | originalEnrichModeList$originalEnrichMode == "similarity"){
                reenrichResults <- lapply(seq_len(length(enrichmentSets)), function(casrnSet){
                    if(originalEnrichModeList$originalEnrichMode == "substructure"){
                        # Query API to get list of annotation classes and types
                        resp <- NULL
                        trySubstructure <- tryCatch({
                            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/searchBySubstructure"), query=list(input=originalNamesToReturn[[names(enrichmentSets)[casrnSet]]]))
                            if(resp$status_code != 200){
                                return(FALSE)
                            }
                            TRUE
                        }, error=function(cond){
                            return(FALSE)
                        })
                        if(!trySubstructure){
                            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                            return(FALSE)
                        }
                        outputSubCasrns <- unlist(lapply(content(resp), function(x) x$casrn))
                        outputSubM <- unlist(lapply(content(resp), function(x) x$m))
                        outputSubCyanide <- unlist(lapply(content(resp), function(x) x$cyanide))
                        outputSubIsocyanate <- unlist(lapply(content(resp), function(x) x$isocyanate))
                        outputSubAldehyde <- unlist(lapply(content(resp), function(x) x$aldehyde))
                        outputSubEpoxide <- unlist(lapply(content(resp), function(x) x$epoxide))
                        reenrichSubstructureResults <- data.frame(casrn=outputSubCasrns, m=outputSubM, similarity=NA, cyanide=outputSubCyanide, isocyanate=outputSubIsocyanate, aldehyde=outputSubAldehyde, epoxide=outputSubEpoxide, stringsAsFactors=FALSE)
                        reenrichSubstructureResults <- reenrichSubstructureResults[reenrichSubstructureResults$casrn %in% enrichmentSets[[casrnSet]], ]
                        return(reenrichSubstructureResults)
                    } else if(originalEnrichModeList$originalEnrichMode == "similarity"){
                        # Set Tanimoto threshold for similarity search
                        threshold <- as.numeric(input$tanimotoThreshold) / 100
                        resp <- NULL
                        trySimilarity <- tryCatch({
                            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/searchBySimilarity"), query=list(input=originalNamesToReturn[[names(enrichmentSets)[casrnSet]]], threshold=threshold))
                            TRUE
                        }, error=function(cond){
                            return(FALSE)
                        })
                        if(!trySimilarity){
                            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                            return(FALSE)
                        }
                        if(resp$status_code != 200){
                            return(NULL)
                        }
                        outputSimCasrns <- unlist(lapply(content(resp), function(x) x$casrn))
                        outputSimM <- unlist(lapply(content(resp), function(x) x$m))
                        outputSimSimilarity <- unlist(lapply(content(resp), function(x) x$similarity))
                        outputSimCyanide <- unlist(lapply(content(resp), function(x) x$cyanide))
                        outputSimIsocyanate <- unlist(lapply(content(resp), function(x) x$isocyanate))
                        outputSimAldehyde <- unlist(lapply(content(resp), function(x) x$aldehyde))
                        outputSimEpoxide <- unlist(lapply(content(resp), function(x) x$epoxide))
                        reenrichSimilarityResults <- data.frame(casrn=outputSimCasrns, m=outputSimM, similarity=outputSimSimilarity, cyanide=outputSimCyanide, isocyanate=outputSimIsocyanate, aldehyde=outputSimAldehyde, epoxide=outputSimEpoxide, stringsAsFactors=FALSE)
                        reenrichSimilarityResults <- reenrichSimilarityResults[reenrichSimilarityResults$casrn %in% enrichmentSets[[casrnSet]], ]
                        return(reenrichSimilarityResults)
                    }
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
                        trySubstructure <- tryCatch({
                            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/searchBySubstructure"), query=list(input=setName))
                            if(resp$status_code != 200){
                                return(FALSE)
                            }
                            TRUE
                        }, error=function(cond){
                            return(FALSE)
                        })
                        if(!trySubstructure) {
                            if(trySubstructure == FALSE){
                                badConnectionFlag <<- TRUE
                                return(NULL)
                            }
                        }
                        if(length(content(resp)) < 1){
                            return(NULL)
                        }
                        outputSubCasrns <- unlist(lapply(content(resp), function(x) x$casrn))
                        outputSubM <- unlist(lapply(content(resp), function(x) x$m))
                        outputSubCyanide <- unlist(lapply(content(resp), function(x) x$cyanide))
                        outputSubIsocyanate <- unlist(lapply(content(resp), function(x) x$isocyanate))
                        outputSubAldehyde <- unlist(lapply(content(resp), function(x) x$aldehyde))
                        outputSubEpoxide <- unlist(lapply(content(resp), function(x) x$epoxide))
                        outpSmiles <- data.frame(casrn=outputSubCasrns, m=outputSubM, cyanide=outputSubCyanide, isocyanate=outputSubIsocyanate, aldehyde=outputSubAldehyde, epoxide=outputSubEpoxide, stringsAsFactors=FALSE)
                    } else {
                        # Set Tanimoto threshold for similarity search
                        threshold <- as.numeric(input$tanimotoThreshold) / 100
                        resp <- NULL
                        trySimilarity <- tryCatch({
                            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/searchBySimilarity"), query=list(input=setName, threshold=threshold))
                            if(resp$status_code != 200){
                                return(FALSE)
                            }  
                            TRUE
                        }, error=function(cond){
                            return(FALSE)
                        })
                        if(!is.null(trySimilarity)){
                            if(trySimilarity == FALSE){
                                badConnectionFlag <<- TRUE
                                return(NULL)
                            }
                        }
                        outputSimCasrns <- unlist(lapply(content(resp), function(x) x$casrn))
                        outputSimM <- unlist(lapply(content(resp), function(x) x$m))
                        outputSimSimilarity <- unlist(lapply(content(resp), function(x) x$similarity))
                        outputSimCyanide <- unlist(lapply(content(resp), function(x) x$cyanide))
                        outputSimIsocyanate <- unlist(lapply(content(resp), function(x) x$isocyanate))
                        outputSimAldehyde <- unlist(lapply(content(resp), function(x) x$aldehyde))
                        outputSimEpoxide <- unlist(lapply(content(resp), function(x) x$epoxide))
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
            if(length(reenrichResults) < 1){
                changePage(page="enrichment")
                if(badConnectionFlag == TRUE){
                    showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                } else {
                    showNotification("Error: No valid input sets.", type="error")
                }
                return(FALSE)
            }

            names(reenrichResults) <- unlist(lapply(seq_len(length(casrnBoxSplit)), function(i) paste0("Set", i)))
            reenrichResults <- reenrichResults[!vapply(reenrichResults, is.null, FUN.VALUE=logical(1))]
            if(length(reenrichResults) > 0){
                enrichmentSets <- lapply(seq_len(length(reenrichResults)), function(i){
                    if(!is.null(reenrichResults[[i]])) {
                        enrichmentSetsInside <- unlist(lapply(seq_len(nrow(reenrichResults[[i]])), function(j) reenrichResults[[i]][j, "casrn"]))
                    } else {
                        return(NULL)
                    }
                })
                names(enrichmentSets) <- names(reenrichResults)
            }
        }
        reenrichResults <- reenrichResults[!vapply(reenrichResults, is.null, FUN.VALUE=logical(1))]
        enrichmentSets <- enrichmentSets[!vapply(enrichmentSets, is.null, FUN.VALUE=logical(1))]
        
        resp <- NULL
        maxInputSize <- 1 # default value of 1 individual set(s)
        tryTransactionSize <- tryCatch({
            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getInputMax"))
            TRUE
        }, error=function(cond){
            return(FALSE)
        })
        if(!tryTransactionSize){
            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
            changePage(page="enrichment")
            return(FALSE)
        } else {
            maxInputSize <- content(resp)
            if(length(enrichmentSets) > maxInputSize) {
                showNotification(paste0("Error: You may only submit a maximum of ", maxInputSize, " input sets at a time."), type="error")
                changePage(page="enrichment")
                return(FALSE)
            }
        }

        # Generate colors for each input sets
        # 16 = number of unique colors generated by getNetworkColors(). There may only be a max of 16 input sets in this version of Tox21 Enricher
        randColor <- sample(seq_len(16), length(names(enrichmentSets)), replace=FALSE)
        # Generate a random color
        colorsAllSets <- unlist(lapply(randColor, function(color){
            tmpColor <- col2rgb(getNetworkColors()[color])
            return(paste0("rgb(", tmpColor[1], ", ", tmpColor[2], ", ", tmpColor[3], ")"))
        }))
        names(colorsAllSets) <- names(enrichmentSets)
        setColors$color <- colorsAllSets
        reenrichResultsList$reenrichResults <- reenrichResults
        enrichmentSetsList$enrichmentSets <- enrichmentSets
        # Get list of original input names for InChIs/SMILES and set names to display later
        if(reenrichFlag == FALSE){
            if (enrichmentType$enrichType == "similarity" | enrichmentType$enrichType == "substructure") {
                originalNames <- unlist(lapply(seq_len(length(casrnBoxSplit)), function(i) paste0(casrnBoxSplit[i], "__Set", i)))
            } else {
                originalNames <- unlist(lapply(casrnBoxSplit, function(x) {
                    if(grepl("^#", x)){
                        return(gsub("#", "", x, fixed=TRUE))
                    }
                    return(NULL)
                }))
                originalNames <- originalNames[!vapply(originalNames, is.null, FUN.VALUE=logical(1))]
            }
            originalNamesList$originalNames <- originalNames
        }

        # Get selected annotation classes
        annoSelectStrPubChem <- lapply (input$checkboxPubChem, function(i) paste0(i, "=checked,"))
        annoSelectStrPubChem <- paste0(annoSelectStrPubChem, collapse="")
        annoSelectStrDrugMatrix <- lapply(input$checkboxDrugMatrix, function(i) paste0(i, "=checked,"))
        annoSelectStrDrugMatrix <- paste0(annoSelectStrDrugMatrix, collapse="")
        annoSelectStrDrugBank <- lapply (input$checkboxDrugBank, function(i) paste0(i, "=checked,"))
        annoSelectStrDrugBank <- paste0(annoSelectStrDrugBank, collapse="")
        annoSelectStrCTD <- lapply (input$checkboxCTD, function(i) paste0(i, "=checked,"))
        annoSelectStrCTD <- paste0(annoSelectStrCTD, collapse="")
        annoSelectStrOther <- lapply (input$checkboxOther, function(i) paste0(i, "=checked,"))
        annoSelectStrOther <- paste0(annoSelectStrOther, collapse="")
        annoSelectStr <- paste0(annoSelectStrPubChem, annoSelectStrDrugMatrix, annoSelectStrDrugBank, annoSelectStrCTD, annoSelectStrOther)
        
        # Convert enrichmentSets into a form that is API-friendly
        enrichmentSetsSanitized <- lapply(names(enrichmentSets), function(enrichmentSet) paste0(paste0(enrichmentSets[[enrichmentSet]], collapse=paste0("__", enrichmentSet, "\n")), "__", enrichmentSet))
        enrichmentSetsSanitized <- paste0(enrichmentSetsSanitized, collapse="\n")
        
        # Convert enrichmentSets into a form that is API-friendly 2
        enrichmentSetsSanitizedLocal <- lapply(names(enrichmentSets), function(enrichmentSet) paste0(paste0(enrichmentSets[[enrichmentSet]], collapse=paste0("__", enrichmentSet, "|")), "__", enrichmentSet))
        enrichmentSetsSanitizedLocal <- paste0(enrichmentSetsSanitizedLocal, collapse="|")

        # Preemptively remove chemicals with reactive structure warnings if option is checked
        if (enrichmentType$enrichType == "similarity" | enrichmentType$enrichType == "substructure") {
            if(input$includeChemsWithWarnings){
                # Get reactive structures for input
                originalReactiveStructures <- lapply(originalNamesList$originalNames, function(x){
                    setInput <- unlist(str_split(x, "__"))[1]
                    tryReactive <- tryCatch({
                        resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getReactiveGroups"), query=list(input=setInput))
                        TRUE
                    }, error=function(cond){
                        return(FALSE)
                    })
                    if(!tryReactive){
                        showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                        return(NULL)
                    }
                    if(resp$status_code != 200){
                        return(NULL)
                    }
                    # else, extract structure info
                    tmpSplit <- unlist(str_split(content(resp), ","))
                    struc_cyanide <- strtoi(tmpSplit[1])
                    struc_isocyanate <- strtoi(tmpSplit[2])
                    struc_aldehyde <- strtoi(tmpSplit[3])
                    struc_epoxide <- strtoi(tmpSplit[4])
                    return(data.frame(cyanide=struc_cyanide, isocyanate=struc_isocyanate, aldehyde=struc_aldehyde, epoxide=struc_epoxide, stringsAsFactors=FALSE))
                })
                names(originalReactiveStructures) <- unlist(lapply(originalNamesList$originalNames, function(x) unlist(str_split(x, "__"))[2]))
                originalReactiveStructures <- originalReactiveStructures[!vapply(originalReactiveStructures, is.null, FUN.VALUE=logical(1))]
                tryWarnings <- tryCatch({
                    resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getStructureWarnings"), query=list(input=enrichmentSetsSanitizedLocal))
                    TRUE
                }, error=function(cond){
                    return(FALSE)
                })
                if(!tryWarnings){
                    showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                    changePage(page="enrichment")
                    return(FALSE)
                } else {
                    # remove chemicals with reactive structure warnings
                    warningDF <- do.call(rbind.data.frame, content(resp))
                    # Remove from enrichmentSets
                    enrichmentSets <- unlist(lapply(names(enrichmentSets), function(setName){
                        currentSet <- enrichmentSets[[setName]]
                        currentSetWarnings <- originalReactiveStructures[[setName]]
                        relevantWarnings <- warningDF %>% filter(warningDF$casrn %in% currentSet)
                        rownames(relevantWarnings) <- relevantWarnings$casrn
                        relevantWarnings$casrn <- NULL # remove casrn column and set now names to casrns
                        CASRNsToKeep <- unlist(lapply(rownames(relevantWarnings), function(x){
                            if(relevantWarnings[x, "cyanide"] == currentSetWarnings$cyanide & relevantWarnings[x, "isocyanate"] == currentSetWarnings$isocyanate & relevantWarnings[x, "aldehyde"] == currentSetWarnings$aldehyde & relevantWarnings[x, "epoxide"] == currentSetWarnings$epoxide) {
                                return(x)
                            }
                            return(NULL)
                        }))
                        CASRNsToKeep <- CASRNsToKeep[!vapply(CASRNsToKeep, is.null, FUN.VALUE=logical(1))]
                        enrichmentSetRemoved <- lapply(enrichmentSets, function(x) x[x %in% CASRNsToKeep])
                        enrichmentSetRemoved <- enrichmentSetRemoved[lapply(enrichmentSetRemoved, length) > 0][setName]
                        return(enrichmentSetRemoved)
                    }), recursive=FALSE)
                    # Remove from reernichResults
                    reenrichResults <- lapply(names(enrichmentSets), function(setName){
                        currentSet <- enrichmentSets[[setName]]
                        currentSetWarnings <- originalReactiveStructures[[setName]]
                        relevantWarnings <- warningDF %>% filter(casrn %in% currentSet)
                        rownames(relevantWarnings) <- relevantWarnings$casrn
                        relevantWarnings$casrn <- NULL # remove casrn column and set now names to casrns
                        CASRNsToKeep <- unlist(lapply(rownames(relevantWarnings), function(x){
                            if(relevantWarnings[x, "cyanide"] == currentSetWarnings$cyanide & relevantWarnings[x, "isocyanate"] == currentSetWarnings$isocyanate & relevantWarnings[x, "aldehyde"] == currentSetWarnings$aldehyde & relevantWarnings[x, "epoxide"] == currentSetWarnings$epoxide) {
                                return(x)
                            }
                            return(NULL)
                        }))
                        CASRNsToKeep <- CASRNsToKeep[!vapply(CASRNsToKeep, is.null, FUN.VALUE=logical(1))]
                        reenrichResultRemoved <- reenrichResults[[setName]] %>% filter(casrn %in% CASRNsToKeep)
                        return(reenrichResultRemoved)
                    })
                    names(reenrichResults) <- names(enrichmentSets)
                    
                    # Set reactive values
                    enrichmentSetsList$enrichmentSets <- enrichmentSets
                    reenrichResultsList$reenrichResults <- reenrichResults
                    
                    # Convert enrichmentSets into a form that is API-friendly
                    enrichmentSetsSanitized <- lapply(names(enrichmentSets), function(enrichmentSet) paste0(paste0(enrichmentSets[[enrichmentSet]], collapse=paste0("__", enrichmentSet, "\n")), "__", enrichmentSet))
                    enrichmentSetsSanitized <- paste0(enrichmentSetsSanitized, collapse="\n")
                    
                    # Convert enrichmentSets into a form that is API-friendly 2
                    enrichmentSetsSanitizedLocal <- lapply(names(enrichmentSets), function(enrichmentSet) paste0(paste0(enrichmentSets[[enrichmentSet]], collapse=paste0("__", enrichmentSet, "|")), "__", enrichmentSet))
                    enrichmentSetsSanitizedLocal <- paste0(enrichmentSetsSanitizedLocal, collapse="|")
                }
            }
        }
        # Convert reenrichResultsList$reenrichResults into a form that is API-friendly
        reenrichResultsSanitized <- ""
        
        # Show error if input chemical matches too many chemicals ( > 1500 in this version of Tox21 Enricher)
        badSets <- unlist(lapply(names(reenrichResultsList$reenrichResults), function(x) {
            # Display error is excessively long list of matches
            if(nrow(reenrichResultsList$reenrichResults[[x]]) > 1500) {
                return(x)
            }
            return(NULL)
        }))
        badSets <- badSets[!vapply(badSets, is.null, FUN.VALUE=logical(1))]
        # Map original input to set names
        badSetInputs <- lapply(originalNamesList$originalNames, function(x){
            tmpSplit <- unlist(str_split(x, "__"))
            if(tmpSplit[2] %in% badSets){
                return(tmpSplit[1])
            }
            return(NULL)
        })
        badSetInputs <- badSetInputs[!vapply(badSetInputs, is.null, FUN.VALUE=logical(1))]
        if(length(badSetInputs) > 0){ # if we have sets that are too big, show error message, clear cookie, and go back to main page
            showNotification(HTML(paste0("<div>Error: The following input sets match over 1500 chemicals in the Tox21 database: <br><b>", paste0(badSetInputs, collapse="<br>"), "</b><br> Please refine your search and try again.</div>")), type="error")
            # Clear session (transaction) ID from cookie
            js$clearSession(transactionId)
            changePage(page="enrichment")
            return(FALSE)
        }
        if(length(reenrichResultsList$reenrichResults) > 0){
            reenrichResultsSanitized <- unlist(lapply(seq_len(length(reenrichResultsList$reenrichResults)), function(reenrichResult){
                rr_setname <- names(reenrichResultsList$reenrichResults)[[reenrichResult]]
                rr_casrn <- paste0(reenrichResultsList$reenrichResults[[reenrichResult]][, "casrn"], collapse="&")
                rr_m <- paste0(reenrichResultsList$reenrichResults[[reenrichResult]][, "m"], collapse="&")
                rr_sim <- NULL
                if("similarity" %in% names(reenrichResultsList$reenrichResults[[reenrichResult]])){
                    rr_sim <- paste0(reenrichResultsList$reenrichResults[[reenrichResult]][, "similarity"], collapse="&")
                }
                rr_cyanide <- paste0(reenrichResultsList$reenrichResults[[reenrichResult]][, "cyanide"], collapse="&")
                rr_isocyanate <- paste0(reenrichResultsList$reenrichResults[[reenrichResult]][, "isocyanate"], collapse="&")
                rr_aldehyde <- paste0(reenrichResultsList$reenrichResults[[reenrichResult]][, "aldehyde"], collapse="&")
                rr_epoxide <- paste0(reenrichResultsList$reenrichResults[[reenrichResult]][, "epoxide"], collapse="&")
                if(is.null(rr_sim)){ # no similarity column
                    return(paste0(rr_setname, "__", rr_casrn, "__", rr_m, "__", rr_cyanide, "__", rr_isocyanate, "__", rr_aldehyde, "__", rr_epoxide))
                }
                return(paste0(rr_setname, "__", rr_casrn, "__", rr_m, "__", rr_sim, "__", rr_cyanide, "__", rr_isocyanate, "__", rr_aldehyde, "__", rr_epoxide))
            }))
        }
        # Get submission timestamp to put in file
        beginTime <- Sys.time()
        # Clean up colors to put into local file
        colorsToPrint <- lapply(seq_len(length(colorsAllSets)), function(i) paste0(names(colorsAllSets)[i], "__", colorsAllSets[i]))
        colorsToPrint <- paste0(colorsToPrint, collapse="|")
        if(length(enrichmentSets) > 0){ # only create input file and database entry if at least one good set
            # Send query to create transaction entry in database
            resp <- NULL
            tryTransaction <- tryCatch({
                resp <- POST(url=paste0(API_PROTOCOL, API_ADDR, "/createTransaction"), query=list(originalMode=originalEnrichModeList$originalEnrichMode, mode=enrichmentType$enrichType, uuid=transactionId, annoSelectStr=annoSelectStr, cutoff=cutoff, input=enrichmentSetsSanitizedLocal, casrnBox=casrnBox, originalNames=paste0(originalNamesList$originalNames, collapse="|"), reenrich=paste0(reenrichResultsSanitized, collapse="|"), color=colorsToPrint, timestampPosted=beginTime, reenrichFlag=reenrichFlagReactive$reenrichFlagReactive))
                TRUE
            }, error=function(cond){
                return(FALSE)
            })
            if(!tryTransaction){
                showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                return(FALSE)
            }
            
            # Send query to create input file
            resp <- NULL
            tryInput <- tryCatch({
                resp <- POST(url=paste0(API_PROTOCOL, API_ADDR, "/createInput"), query=list(transactionId=transactionId, enrichmentSets=enrichmentSetsSanitized, setNames=paste0(names(enrichmentSets), collapse="\n"), mode=enrichmentType$enrichType, nodeCutoff=cutoff, annoSelectStr=annoSelectStr))
                TRUE
            }, error=function(cond){
                return(FALSE)
            })
            if(!tryInput){
                showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                return(FALSE)
            }
            if(resp$status_code != 200){
                changePage(page="enrichment")
                showNotification("Error: Problem creating input files for enrichment.", type="error")
                return(FALSE)
            }
        } else {
            changePage(page="enrichment")
            showNotification("Error: No valid input sets.", type="error")
            return(FALSE)
        }
        # Show waiting page
        changePage(page="waiting")
        splitAnnotations <- unlist(str_split(annoSelectStr, "=checked,"))
        splitAnnotationsString <- paste0(splitAnnotations, collapse=", ")
        # Cleaned enrichment type strings for more readable display
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
        resp <- NULL
        tryQueue <- tryCatch({
            resp <- POST(url=paste0(API_PROTOCOL, API_ADDR, "/queue"), query=list(mode=enrichmentType$enrichType, enrichmentUUID=transactionId, annoSelectStr=annoSelectStr, nodeCutoff=cutoff, setNames=paste0(names(enrichmentSets), collapse="\n")))
            TRUE
        }, error=function(cond){
            return(FALSE)
        })
        if(!tryQueue){
            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
            return(FALSE)
        }
        
        # if enrichment runs into an error on the API side, cancel and show error on main UI
        if (resp$status_code != 200){
            changePage(page="enrichment")
            showNotification(HTML(paste0("Error: ", content(resp, as="text"))), type="error")
            return(FALSE)
        }
        
        # Get initial queue position and display to user
        initQueuePos <- -1
        resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getQueuePos"), query=list(transactionId=transactionId, mode="init"))
        if(resp$status_code != 200) {
            showNotification("Error: Could not fetch queue position for this request.", type="error")
        } else {
            initQueuePos <- content(resp)
            initQueuePos <- gsub("\n", "<br><br>", initQueuePos)
        }
        
        waitingData <- data.frame("Position"=c(initQueuePos), "Mode"=c(enrichmentType$enrichType), "UUID"=c(transactionId), "Selected Annotations"=c(splitAnnotationsString), "Node Cutoff"=c(cutoff), "Input"=c(casrnBox), stringsAsFactors=FALSE)
        # Save input data to use later
        inputSetList$inputSet <- waitingData
        # update waitingData with cleaned output
        waitingData <- data.frame("Position"=c(initQueuePos), "Mode"=c(enrichmentDisplayType), "UUID"=c(transactionId), "Selected Annotations"=c(splitAnnotationsString), "Node Cutoff"=c(cutoff), "Input"=c(casrnBox), stringsAsFactors=FALSE)

        # # Clean up column name formatting
        colnames(waitingData) <- list("Status", "Request Mode", "Request UUID", "Selected Annotations", "Node Cutoff", "User Input")
        output[["waitingTable"]] <- renderUI(
            column(12,
                DT::datatable({waitingData},
                    escape=FALSE,
                    rownames=FALSE,
                    class="row-border stripe compact",
                    style="bootstrap",
                    select="none",
                    options=list(
                        autoWidth=TRUE
                    )
                )
            )
        )
        
        # Query the API to see if we ran into an error at this point
        resp <- NULL
        tryError <- tryCatch({
            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/hasError"), query=list(transactionId=transactionId))
            TRUE
        }, error=function(cond){
            return(FALSE)
        })
        if(!tryError){
            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
            return(FALSE)
        }
        # if enrichment runs into an error while performing the enrichment in the queue, cancel and show error on main UI
        if(unlist(content(resp)) != FALSE | resp$status_code != 200){
            changePage(page="enrichment")
            showNotification(HTML(paste0("Error: ", errorFile)), type="error")
            return(FALSE)
        }
        # Set flag so the app knows to start updating the table
        tableLoaded$loaded <- TRUE
        return(TRUE)
    }
    
    redirectToResultsPage <- function(){
        mode <- inputSetList$inputSet[1, "Mode"]
        transactionId <- inputSetList$inputSet[1, "UUID"]
        annoSelectStr <- gsub(", ", "=checked,", inputSetList$inputSet[1, "Selected.Annotations"], fixed=TRUE)
        nodeCutoff <- inputSetList$inputSet[1, "Node.Cutoff"]
        # Query the API to see if we ran into an error
        resp <- NULL
        tryError <- tryCatch({
            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/hasError"), query=list(transactionId=transactionId))
            TRUE
        }, error=function(cond){
            return(FALSE)
        })
        if(!tryError){
            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
            return(FALSE)
        }
        # if enrichment runs into an error while performing the enrichment in the queue, cancel and show error on main UI
        if(unlist(content(resp)) != FALSE | resp$status_code != 200){
            changePage(page="enrichment")
            showNotification(HTML(paste0("Error: ", unlist(content(resp)))), type="error")
            return(FALSE)
        }
        # If no errors
        # First, check if process is done
        # Query the API to see if process is done
        resp <- NULL
        tryQueue <- tryCatch({
            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/isRequestFinished"), query=list(transactionId=transactionId))
            TRUE
        }, error=function(cond){
            return(FALSE)
        })
        if(!tryQueue){
            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
            return(FALSE)
        }
        # Show warning message if request fails
        if(resp$status_code != 200){
            showNotification("Error: There was an error determining the status of your request. Please try again later.", type="error")
        } else {
            checkIfQueueFile <- unlist(content(resp))
            if(checkIfQueueFile == 0){
                # If it is not, print warning to UI
                showNotification("Your request has not completed yet. Please wait for it to complete before accessing results.", type="warning")
            } else if(checkIfQueueFile == -1){
                # If it crashed, print error to UI
                showNotification(paste0("There was a problem processing the request ", transactionId, " on the server. Please try submitting this request again from the beginning."), type="error")
            } else {
                # Else if done, get results
                enrichmentResults(mode, transactionId, annoSelectStr, nodeCutoff, enrichmentSetsList$enrichmentSets, originalNamesList$originalNames, reenrichResultsList$reenrichResults, originalEnrichModeList$originalEnrichMode, setColors$color)
            }
        }
        # Re-enable buttons
        shinyjs::enable(id="clipboard")
        shinyjs::enable(id="cancelEnrichment")
        shinyjs::enable(id="settingsButton")
    }
    
    observe({
        invalidateLater(2000, session)
        if(tableLoaded$loaded == TRUE){
            if(finishedFlag$finished == TRUE){
                tableLoaded$loaded <- FALSE
                finishedFlag$finished <- FALSE
                redirectToResultsPage()
            } else {
                # TODO: don't update if status is the same
                updateWaitingTable()
            }
        }
    })
    
    # Check if "Cancel Enrichment" button was pressed
    observeEvent(input$cancelEnrichment, {
        # Show confirmation box
        showModal(modalDialog(
            HTML(paste0("<p><b>Warning</b>: You are about to cancel this request. This action cannot be undone. Continue?</p>")),
            title="Are you sure you want to cancel this request?",
            footer=tagList(actionButton("cancelConfirm", "Yes, cancel this request."), modalButton("Close"))
        ))
    })
    
    # Check if the confirmation button was pressed for enrichment cancellation
    observeEvent(input$cancelConfirm, {
        # Stop updating waiting page
        tableLoaded$loaded <- FALSE
        finishedFlag$finished <- FALSE
      
        # Query the API to cancel enrichment
        resp <- NULL
        tryCancel <- tryCatch({
            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/cancelEnrichment"), query=list(transactionId=reactiveTransactionId$id))
            TRUE
        }, error=function(cond){
            return(FALSE)
        })
        if(!tryCancel){
            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
            return(FALSE)
        }
        # Remove modal
        removeModal()
        # Go back to main page
        changePage(page="enrichment")
        # Clear cookie storing this session
        js$clearSession(reactiveTransactionId$id)
        showNotification("Request cancelled.", type="message")
        return(FALSE)
    })
    
    # View results page if request has successfully completed
    enrichmentResults <- function(mode, transactionId, annoSelectStr, cutoff, enrichmentSets, originalNames, reenrichResults, originalEnrichMode, colorsList){
        changePage(page="results")
        # Reset checkboxList$checkboxes
        checkboxList$checkboxes <- NULL
        # Set reactive transactionId value 
        reactiveTransactionId$id <- transactionId
        # Reset heatmap colors
        lHeatmapColor <- reactiveValues()
        hHeatmapColor <- reactiveValues()
        lHeatmapColorChart$color <- "white"
        hHeatmapColorChart$color <- "red"
        lHeatmapColorCluster$color <- "white"
        hHeatmapColorCluster$color <- "red"
        
        # Get enrichmentSets
        if(!is.null(enrichmentSetsList$enrichmentSets)){
            enrichmentSets <- enrichmentSetsList$enrichmentSets  
        }
        # Check which enrichment sets are good
        resp <- NULL
        tryCheck <- tryCatch({
            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getInputSets"), query=list(transactionId=transactionId))
            TRUE
        }, error=function(cond){
            return(FALSE)
        })
        if(!tryCheck){
            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
            return(FALSE)
        }
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
        enrichmentSets <- enrichmentSets[!vapply(enrichmentSets, is.null, FUN.VALUE=logical(1))]
        
        #Get original names
        if(!is.null(originalNamesList$originalNames)){
            originalNames <- originalNamesList$originalNames  
        }
        # Get reenrichResults
        if(!is.null(reenrichResultsList$reenrichResults)){
            reenrichResults <- reenrichResultsList$reenrichResults 
        }
        # Get original enrich mode
        if(!is.null(originalEnrichModeList$originalEnrichMode)){
            originalEnrichMode <- originalEnrichModeList$originalEnrichMode
        }
        # Get colors
        if(!is.null(setColors$color)){
            colorsList <- setColors$color
        } else {
            setColors$color <- colorsList
        }

        # If success at this point, hide waiting page
        shinyjs::hide(id="waitingPage")
        
        # Initialize if we have warnings
        haveWarnings <- reactiveValues(warnings=FALSE)

        # Render results page
        if(mode == "annotation") {
            output$resultsTabset <- renderUI({
                fluidRow(
                    column(12,
                        h1(id="resultsHeader", "Fetched Annotations"),
                        h4(id="resultsTransactionID", paste0("Request ID: ", transactionId)),
                        do.call(tabsetPanel, c(id='tab', lapply(names(enrichmentSets), function(i) {
                            tabPanel(
                                title=paste0(i),
                                uiOutput(paste0("annotationTab_", i)) %>% withSpinner()
                            )
                        })))
                    ),
                    column(12,
                        downloadButton(outputId="downloadButton", label="Download results", icon=icon("download"))
                    )
                )
            })
            # Fetch setFiles from server via API
            resp <- NULL
            tryResults <- tryCatch({
                resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getResults"), query=list(transactionId=transactionId))
                TRUE
            }, error=function(cond){
                return(FALSE)
            })
            if(!tryResults){
                showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                return(FALSE)
            }
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
            lapply(seq_len(length(setFiles)), function(setFile){
                # Create observers for individual CASRN file links
                if( !grepl(paste0("__FullMatrix.txt"), setFilesSplit[setFile], fixed=TRUE) & !grepl(paste0(".zip"), setFilesSplit[setFile], fixed=TRUE) & !grepl(paste0("__ErrorCASRNs.txt"), setFilesSplit[setFile], fixed=TRUE) & !grepl(paste0("__Input.txt"), setFilesSplit[setFile], fixed=TRUE)){
                    if(is.null(setFilesObservers$observers[[paste0("casrn", setFilesSplit[setFile], "Observer__", setFile)]])){
                        setFilesObservers$observers[[paste0("casrn", setFilesSplit[setFile], "Observer__", setFile)]] <- observeEvent(input[[paste0(setFilesSplit[setFile], "__link")]], {
                            # Create directory for request
                            dir.create(paste0(tempdir(), "/output/"))
                            dir.create(paste0(tempdir(), "/output/", transactionId, "/"))
                            tryAnnotationDL <- TRUE
                            if(!file.exists(paste0(tempdir(), "/output/", transactionId, "/", setFilesSplit[setFile]))){
                                tryAnnotationDL <- tryCatch({
                                    download.file(paste0(API_PROTOCOL, API_ADDR, "/serveFileText?transactionId=", transactionId, "&filename=", setFilesSplit[setFile], "&subDir=Output"), destfile=paste0(tempdir(), "/output/", transactionId, "/", setFilesSplit[setFile]))
                                }, error=function(cond){
                                    return(NULL)
                                })
                            }
                            if(is.null(tryAnnotationDL)) {
                                showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                            } else {
                                # Check file size
                                fSize <- file.info(paste0(tempdir(), "/output/", transactionId, "/", setFilesSplit[setFile]))$size
                                if(fSize > 1000000){ # show preview error if file is larger than 1MB
                                    showNotification(paste0("Warning: this file is too large to be previewed. Please view it after downloading the result files."), duration=5, type="warning")
                                    return(FALSE)
                                }
                                # Open preview in modal
                                # disable refreshbutton
                                shinyjs::disable(id="refresh")
                                tmpFile <- read.delim(paste0(tempdir(), "/output/", transactionId, "/", setFilesSplit[setFile]), sep="\t", header=FALSE, comment.char="", fill=TRUE, stringsAsFactors=FALSE)
                                names(tmpFile) <- c("Class", "Annotation")
                                showModal(
                                    modalDialog(
                                        title=paste0("Annotation file for ", gsub(".txt", "", gsub("__", ": ", setFilesSplit[setFile]))),
                                        footer=actionButton(inputId="modalCloseAnnotation", label="Close preview"),
                                        size="l",
                                        fluidRow(
                                            column(12, 
                                                dataTableOutput("annotationPreview")
                                            )
                                        )
                                    )
                                )
                                output[["annotationPreview"]] <- renderDataTable(
                                    DT::datatable({tmpFile},
                                        escape=FALSE,
                                        rownames=FALSE,
                                        class="row-border stripe compact",
                                        style="bootstrap",
                                        select="none",
                                        options=list( 
                                            paging=FALSE,
                                            scrollX=TRUE,
                                            dom="Bfrtip",
                                            pageLength=10,
                                            buttons=list("copy", "csv", "excel", "pdf", "print")  
                                        ),
                                        extensions="Buttons"
                                    )
                                )
                            }
                        }, ignoreInit=TRUE)
                    }
                }
            })
            observeEvent(input$modalCloseAnnotation, {
                # enable refreshbutton
                shinyjs::enable(id="refresh")
                removeModal()
            })
            
            # Create links and handlers for result files
            lapply(names(enrichmentSets), function(i) {
                # Input
                if(is.null(setFilesObservers$observers[[paste0("inputObserver__", i)]])){
                    setFilesObservers$observers[[paste0("inputObserver__", i)]] <- observeEvent(input[[paste0(i, ".txt__link")]], {
                        # Create directory for request
                        dir.create(paste0(tempdir(), "/input/"))
                        dir.create(paste0(tempdir(), "/input/", transactionId, "/"))
                        tryInputDL <- TRUE
                        if(!file.exists(paste0(tempdir(), "/input/", transactionId, "/", i, ".txt"))){
                            tryInputDL <- tryCatch({
                                download.file(paste0(API_PROTOCOL, API_ADDR, "/serveFileText?transactionId=", transactionId, "&filename=", i, ".txt&subDir=Input"), destfile=paste0(tempdir(), "/input/", transactionId, "/", i, ".txt"))
                            }, error=function(cond){
                                return(NULL)
                            })
                        }
                        if(is.null(tryInputDL)) {
                            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                        } else {
                            # Check file size
                            fSize <- file.info(paste0(tempdir(), "/input/", transactionId, "/", i, ".txt"))$size
                            if(fSize > 1000000){ # show preview error if file is larger than 1MB
                                showNotification(paste0("Warning: this file is too large to be previewed. Please view it after downloading the result files."), duration=5, type="warning")
                                return(FALSE)
                            }
                            # Open preview in modal
                            # disable refreshbutton
                            shinyjs::disable(id="refresh")
                            tmpFile <- read.delim(paste0(tempdir(), "/input/", transactionId, "/", i, ".txt"), sep="\t", header=FALSE, comment.char="", fill=TRUE, stringsAsFactors=FALSE)
                            names(tmpFile) <- c("CASRN", "Chemical Name")
                            showModal(
                                modalDialog(
                                    title=paste0("Input file for ", i),
                                    footer=actionButton(inputId="modalCloseInputAnnotation", label="Close preview"),
                                    size="l",
                                    fluidRow(
                                        column(12, 
                                            dataTableOutput("inputPreview")
                                        )
                                    )
                                )
                            )
                            output[["inputPreview"]] <- renderDataTable(
                                DT::datatable({tmpFile},
                                    escape=FALSE,
                                    rownames=FALSE,
                                    class="row-border stripe compact",
                                    style="bootstrap",
                                    select="none",
                                    options=list( 
                                        paging=FALSE,
                                        dom="Bfrtip",
                                        pageLength=10,
                                        buttons=list("copy", "csv", "excel", "pdf", "print")  
                                    ),
                                    extensions="Buttons"
                                )
                            )
                        }
                    }, ignoreInit=TRUE)
                }
                observeEvent(input$modalCloseInputAnnotation, {
                    # enable refreshbutton
                    shinyjs::enable(id="refresh")
                    removeModal()
                })
              
                # Error CASRNs
                if(is.null(setFilesObservers$observers[[paste0("errorfileObserver__", i)]])){
                    setFilesObservers$observers[[paste0("errorfileObserver__", i)]] <- observeEvent(input[[paste0(i, "__ErrorCASRNs.txt__link")]], {
                        # Create directory for request
                        dir.create(paste0(tempdir(), "/output/"))
                        dir.create(paste0(tempdir(), "/output/", transactionId, "/"))
                        tryErrorDL <- TRUE
                        if(!file.exists(paste0(tempdir(), "/output/", transactionId, "/", i, "__ErrorCASRNs.txt"))){
                            tryErrorDL <- tryCatch({
                                download.file(paste0(API_PROTOCOL, API_ADDR, "/serveFileText?transactionId=", transactionId, "&filename=", i, "__ErrorCASRNs.txt&subDir=Output"), destfile=paste0(tempdir(), "/output/", transactionId, "/", i, "__ErrorCASRNs.txt"))
                            }, error=function(cond){
                                return(NULL)
                            })
                        }
                        if(is.null(tryErrorDL)) {
                            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                        } else {
                            # Check file size
                            fSize <- file.info(paste0(tempdir(), "/output/", transactionId, "/", i, "__ErrorCASRNs.txt"))$size
                            if(fSize > 1000000){ # show preview error if file is larger than 1MB
                                showNotification(paste0("Warning: this file is too large to be previewed. Please view it after downloading the result files."), duration=5, type="warning")
                                return(FALSE)
                            }
                            # Open preview in modal
                            # disable refreshbutton
                            shinyjs::disable(id="refresh")
                            tmpFile <- read.delim(paste0(tempdir(), "/output/", transactionId, "/", i, "__ErrorCASRNs.txt"), sep="\t", header=FALSE, comment.char="", fill=TRUE, stringsAsFactors=FALSE)
                            names(tmpFile) <- c("CASRN")
                            showModal(
                                modalDialog(
                                    title=paste0("Error CASRNs for ", i),
                                    footer=actionButton(inputId="modalCloseErrorAnnotation", label="Close preview"),
                                    size="l",
                                    fluidRow(
                                        column(12, 
                                            dataTableOutput("errorPreview")
                                        )
                                    )
                                )
                            )
                            output[["errorPreview"]] <- renderDataTable(
                                DT::datatable({tmpFile},
                                    escape=FALSE,
                                    rownames=FALSE,
                                    class="row-border stripe compact",
                                    style="bootstrap",
                                    select="none",
                                    options=list( 
                                        paging=FALSE,
                                        scrollX=TRUE,
                                        dom="Bfrtip",
                                        pageLength=10,
                                        buttons=list("copy", "csv", "excel", "pdf", "print")  
                                    ),
                                    extensions="Buttons"
                                )
                            )
                        }
                    }, ignoreInit=TRUE)
                }
                observeEvent(input$modalCloseErrorAnnotation, {
                    # enable refreshbutton
                    shinyjs::enable(id="refresh")
                    removeModal()
                })
                
                # Full matrix
                if(is.null(setFilesObservers$observers[[paste0("fullMatrixObserver__", i)]])){
                    setFilesObservers$observers[[paste0("fullMatrixObserver__", i)]] <- observeEvent(input[[paste0(i, "__FullMatrix.txt__link")]], {
                        # Create directory for request
                        dir.create(paste0(tempdir(), "/output/"))
                        dir.create(paste0(tempdir(), "/output/", transactionId, "/"))
                        tryFullMatrixDL <- TRUE
                        if(!file.exists(paste0(tempdir(), "/output/", transactionId, "/", i, "__FullMatrix.txt"))){
                            tryFullMatrixDL <- tryCatch({
                                download.file(paste0(API_PROTOCOL, API_ADDR, "/serveFileText?transactionId=", transactionId, "&filename=", i, "__FullMatrix.txt&subDir=Output"), destfile=paste0(tempdir(), "/output/", transactionId, "/", i, "__FullMatrix.txt"))
                            }, error=function(cond){
                                return(NULL)
                            })
                        }
                        if(is.null(tryFullMatrixDL)) {
                            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                        } else {
                            # Check file size
                            fSize <- file.info(paste0(tempdir(), "/output/", transactionId, "/", i, "__FullMatrix.txt"))$size
                            if(fSize > 1000000){ # show preview error if file is larger than 1MB
                                showNotification(paste0("Warning: this file is too large to be previewed. Please view it after downloading the result files."), duration=5, type="warning")
                                return(FALSE)
                            }
                            # Open preview in modal
                            # disable refreshbutton
                            shinyjs::disable(id="refresh")
                            tmpFile <- read.delim(paste0(tempdir(), "/output/", transactionId, "/", i, "__FullMatrix.txt"), sep="\t", header=TRUE, comment.char="", fill=TRUE, stringsAsFactors=FALSE)
                            names(tmpFile) <- lapply(names(tmpFile), function(x){
                              return(gsub("\\.", " ", x))
                            })
                            showModal(
                                modalDialog(
                                    title=paste0("Full matrix file for ", i),
                                    footer=actionButton(inputId="modalCloseFullMatrix", label="Close preview"),
                                    size="l",
                                    fluidRow(
                                        column(12, 
                                            dataTableOutput("fullMatrixPreview")
                                        )
                                    )
                                )
                            )
                            
                            output[["fullMatrixPreview"]] <- renderDataTable(
                                DT::datatable({tmpFile},
                                    escape=FALSE,
                                    rownames=FALSE,
                                    class="row-border stripe compact",
                                    style="bootstrap",
                                    select="none",
                                    options=list( 
                                        paging=FALSE,
                                        scrollX=TRUE,
                                        dom="Bfrtip",
                                        pageLength=10,
                                        buttons=list("copy", "csv", "excel", "pdf", "print")  
                                    ),
                                    extensions="Buttons"
                                )
                            )
                        }
                    }, ignoreInit=TRUE)
                }
                observeEvent(input$modalCloseFullMatrix, {
                    # enable refreshbutton
                    shinyjs::enable(id="refresh")
                    removeModal()
                })
                
                lapply(names(enrichmentSets), function(i) {
                    # Load matrix to make heatmap
                    fullMatrixHeatmap <- NULL
                    fullMatrixHeatmapCasrns <- NULL
                    fullMatrixHeatmapAnnotations <- NULL
                    dir.create(paste0(tempdir(), "/output/")) # Create temporary directory for request
                    dir.create(paste0(tempdir(), "/output/", transactionId, "/"))
                    tryFullMatrixDL <- TRUE
                    if(!file.exists(paste0(tempdir(), "/output/", transactionId, "/", i, "__FullMatrix.txt"))){
                        tryFullMatrixDL <- tryCatch({
                            download.file(paste0(API_PROTOCOL, API_ADDR, "/serveFileText?transactionId=", transactionId, "&filename=", i, "__FullMatrix.txt&subDir=Output"), destfile=paste0(tempdir(), "/output/", transactionId, "/", i, "__FullMatrix.txt"))
                        }, error=function(cond){
                            return(NULL)
                        })
                    }
                    if(is.null(tryFullMatrixDL)) {
                        showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                    } else {
                        # Check file size
                        fSize <- file.info(paste0(tempdir(), "/output/", transactionId, "/", i, "__FullMatrix.txt"))$size
                        if(fSize > 1000000){ # show preview error if file is larger than 1MB
                            showNotification(paste0("Warning: this file is too large to be previewed. Please view it after downloading the result files."), duration=5, type="warning")
                            return(FALSE)
                        }
                        fullMatrixHeatmap <- read.delim(paste0(tempdir(), "/output/", transactionId, "/", i, "__FullMatrix.txt"), sep="\t", header=TRUE, comment.char="", fill=TRUE, stringsAsFactors=FALSE)
                        names(fullMatrixHeatmap) <- lapply(names(fullMatrixHeatmap), function(x) gsub("\\.", " ", x))
                        fullMatrixHeatmapCasrns <- fullMatrixHeatmap[, 1]
                        fullMatrixHeatmap$CASRN <- NULL
                        fullMatrixHeatmapAnnotations <- names(fullMatrixHeatmap)
                        fullMatrixHeatmap <- t(data.matrix(fullMatrixHeatmap))
                    }
                    
                    lHeatmapColor[[paste0("color__", i)]] <- "white"
                    hHeatmapColor[[paste0("color__", i)]] <- "red"
                    
                    output[[paste0("annotationTab_", i)]] <- renderUI(
                        div(
                            column(12,
                                lapply(seq_len(length(setFiles)), function(setFile){
                                    # Create item in tab for matching sets
                                    if (grepl(paste0(i, "[.]*"), setFilesSplit[setFile])) {
                                        fluidRow(
                                            column(id=paste0(setFilesSplit[setFile]), 3, 
                                                if(endsWith(setFilesSplit[setFile], "__FullMatrix.txt")){
                                                    tipify( div(actionLink(inputId=paste0(i, "__FullMatrix.txt__link"), label=gsub("__FullMatrix.txt", " Full Matrix", setFilesSplit[setFile]))), "A matrix displaying all of the annotations and their corresponding chemicals for this set (.txt format).", placement="bottom")
                                                } else if (endsWith(setFilesSplit[setFile], "ErrorCASRNs.txt")){ 
                                                    tipify( div(actionLink(inputId=paste0(setFilesSplit[setFile], "__link"), label=paste0(i, " Error CASRNs"))), "A list of submitted CASRNs that were not found in the Tox21 Enricher database (.txt format).", placement="bottom")
                                                } else if (endsWith(setFilesSplit[setFile], "__Input.txt")){ # Input file
                                                    tipify( div(actionLink(inputId=paste0(gsub("__Input", "", setFilesSplit[setFile]), "__link"), label=paste0(i, " Input"))), "A list of input chemicals for this set (.txt format).", placement="bottom")
                                                } else { # CASRN file
                                                    tipify( div(actionLink(inputId=paste0(setFilesSplit[setFile], "__link"), label=gsub(".txt", "", gsub(paste0(i, "__"), "", setFilesSplit[setFile])))), "A list of annotations in the Tox21 Enricher database for this chemical with respect to the given annotation classes (.txt format).", placement="bottom")
                                                }
                                            )
                                        )
                                    }
                                })
                            ),
                            if(!is.null(fullMatrixHeatmap)) {
                                fluidRow(
                                    column(1, 
                                        radioButtons(inputId=paste0("lHeatmapColorControl__", i), label="Select 0 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected="white"),
                                    ),
                                    column(1,
                                        radioButtons(inputId=paste0("hHeatmapColorControl__", i), label="Select 1 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected="red")
                                    ),
                                    column(10,
                                        uiOutput(outputId=paste0("fullMatrixHeatmap__", i))
                                    )
                                )
                            }
                        )
                    )
                    
                    output[[paste0("fullMatrixHeatmap__", i)]] <- renderUI(
                        div(
                            plot_ly(x=fullMatrixHeatmapCasrns, y=fullMatrixHeatmapAnnotations, z=fullMatrixHeatmap, colors=colorRamp(c(lHeatmapColor[[paste0("color__", i)]], hHeatmapColor[[paste0("color__", i)]])), type="heatmap", xgap=2, ygap=2 ) %>%
                            layout(
                                autosize=TRUE,
                                showlegend=FALSE,
                                xaxis=list(title="<b>Input CASRNs</b>", automargin=TRUE),
                                yaxis=list(title="<b>Annotations</b>", tickfont=list(size=9), tickangle=15, type="category", automargin=TRUE),
                                plot_bgcolor="transparent",
                                paper_bgcolor="transparent",
                                font=list(color=theme$textcolor)
                            ) %>% hide_colorbar()
                        )
                    )
                    
                    # add observers for heatmap colors
                    observeEvent(input[[paste0("lHeatmapColorControl__", i)]], {
                        lHeatmapColor[[paste0("color__", i)]] <- input[[paste0("lHeatmapColorControl__", i)]]
                    })
                    observeEvent(input[[paste0("hHeatmapColorControl__", i)]], {
                        hHeatmapColor[[paste0("color__", i)]] <- input[[paste0("hHeatmapColorControl__", i)]]
                    })
                    
                })
            })
  
            # Event handler for download full results
            output$downloadButton <- downloadHandler(
                filename=function(){
                    return(paste0("tox21enricher_", transactionId, ".zip"))
                },
                content=function(file){
                    # Create directory for request
                    dir.create(paste0(tempdir(), "/output/"))
                    dir.create(paste0(tempdir(), "/output/", transactionId, "/"))
                    tryZipDL <- TRUE
                    if(!file.exists(paste0(tempdir(), "/output/", transactionId, "/tox21enricher_", transactionId, ".zip"))){
                        tryZipDL <- tryCatch({
                            download.file(paste0(API_PROTOCOL, API_ADDR, "/serveFileText?transactionId=", transactionId, "&filename=tox21enricher_", transactionId, ".zip&subDir=Output"), destfile=paste0(tempdir(), "/output/", transactionId, "/tox21enricher_", transactionId, ".zip"))
                        }, error=function(cond){
                            return(NULL)
                        })
                    }
                    if(is.null(tryZipDL)) {
                        showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                    } else {
                        file.copy(paste0(tempdir(), "/output/", transactionId, "/tox21enricher_", transactionId, ".zip"), file)
                    }
                }
            )
        } else { # Render enrichment results
            # Download button for zipped archive of all files
            # Event handler for download full results
            output$downloadButton <- downloadHandler(
                filename=function(){
                    return(paste0("tox21enricher_", transactionId, ".zip"))
                },
                content=function(file){
                    # Create directory for request
                    dir.create(paste0(tempdir(), "/output/"))
                    dir.create(paste0(tempdir(), "/output/", transactionId, "/"))
                    tryZipDL <- TRUE
                    if(!file.exists(paste0(tempdir(), "/output/", transactionId, "/tox21enricher_", transactionId, ".zip"))){
                        tryZipDL <- tryCatch({
                            download.file(paste0(API_PROTOCOL, API_ADDR, "/serveFileText?transactionId=", transactionId, "&filename=tox21enricher_", transactionId, ".zip&subDir=Output"), destfile=paste0(tempdir(), "/output/", transactionId, "/tox21enricher_", transactionId, ".zip"))
                        }, error=function(cond){
                            return(NULL)
                        })
                    }
                    if(is.null(tryZipDL)) {
                        showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                    } else {
                        file.copy(paste0(tempdir(), "/output/", transactionId, "/tox21enricher_", transactionId, ".zip"), file)
                    }
                }
            )
          
            # Choice names for radio buttons
            radioNames <- lapply(names(enrichmentSets), function(setName){
                # Calculate if text color needs to be black or white
                colorString <- gsub("rgb(", "", colorsList[setName], fixed=TRUE)
                colorString <- gsub(")", "", colorString, fixed=TRUE)
                tmpSplit <- unlist(str_split(colorString, ", "))
                bgRed <- strtoi(tmpSplit[1])
                bgGreen <- strtoi(tmpSplit[2])
                bgBlue <- strtoi(tmpSplit[3])
                textDisplayColor <- "#000000" # default black
                # formula from https://stackoverflow.com/questions/3942878/how-to-decide-font-color-in-white-or-black-depending-on-background-color
                if((bgRed * 0.299 + bgGreen * 0.587 + bgBlue * 0.114) <= 186) {
                    textDisplayColor <- "#FFFFFF"
                }
                return(HTML(paste0("<div style='width:100%;height:20px;color:", textDisplayColor, ";background-color:", colorsList[setName], "'>",  setName, "</div>")))
            })
            output$resultsTabset <- renderUI({
                fluidRow(
                    fluidRow(
                        column(12,
                            h1(id="resultsHeader", "Enrichment Results"),
                            h4(id="resultsTransactionID", paste0("Request ID: ", transactionId)),
                            do.call(tabsetPanel, c(id='tab', lapply(names(enrichmentSets), function(i) {
                                outputOptions(output, paste0("outTab_", i), suspendWhenHidden=FALSE)
                                tabPanel(
                                    title=paste0(i),
                                    value=paste0(i),
                                    uiOutput(paste0("outTab_", i)),
                                )
                                
                            }))),
                        )
                    ),
                    fluidRow(
                        column(12,
                            # Download results button
                            downloadButton(outputId="downloadButton", label="Download results", icon=icon("download"))
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
                resp <- NULL
                tryGCT <- tryCatch({
                    resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/readGct"), query=list(transactionId=transactionId, cutoff=cutoff, mode="set", set=i))
                    TRUE
                }, error=function(cond){
                    return(FALSE)
                })
                if(!tryGCT){
                    showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                    return(FALSE)
                }
                if(resp$status_code != 200){
                    return(NULL)
                }
                gctFile <- NULL
                if(length(content(resp)) > 0){
                    # TODO: Clean this up!
                    colLength <- length(content(resp)[[1]]) - 1 # number of columns -1 to remove column name
                    gctFile <- vapply(content(resp), function(x){
                        gctFileInner <- unlist(vapply(names(x), function(y) {
                            if(y != "_row"){
                                return(as.double(x[y]))  
                            } else {
                                return(0)
                            }
                        }, FUN.VALUE=numeric(1)))
                        gctFileInner <- gctFileInner[!grepl("_row", names(gctFileInner))]
                        gctFileInner <- gctFileInner[!vapply(gctFileInner, is.null, FUN.VALUE=logical(1))]
                        dfToReturn <- t(data.frame(gctFileInner, stringsAsFactors=FALSE))
                        row.names(dfToReturn) <- x["_row"]
                        return(dfToReturn)
                    }, FUN.VALUE=double(colLength))
                    gctFileColNames <- unique(unlist(lapply(content(resp), function(x){
                        gctFileInner <- unlist(lapply(names(x), function(y) {
                            if(y != "_row"){
                                return(y)
                            } else {
                                return(NULL)
                            }
                        }))
                        gctFileInner <- gctFileInner[!vapply(gctFileInner, is.null, FUN.VALUE=logical(1))]
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
                        gctFileInner <- gctFileInner[!vapply(gctFileInner, is.null, FUN.VALUE=logical(1))]
                        return(gctFileInner)
                    })))
                    # Coerce to data frame so we can name cols and rows and then transpose
                    gctFile <- as.data.frame(gctFile, stringsAsFactors=FALSE, check.names=FALSE)
                    # Only do this if more than 1 col
                    if(ncol(gctFile) > 1){
                        gctFile <- t(gctFile)
                    }
                    # Reformat columns (annotations) to put pvalues in scientific notation
                    colnames(gctFile) <- unlist(lapply(gctFileColNames, function(x){
                        colNameSplit <- unlist(str_split(x, " \\| ")) # split on vertical bar to separate term [1], annotation class [2], and pvalue [3]
                        return(paste0(colNameSplit[1], " | ", colNameSplit[2], " | ", formatC(as.numeric(colNameSplit[3]), format="e", digits=2)))
                    }))
                    rownames(gctFile) <- gctFileRowNames
                }
                if(!is.null(gctFile)){
                    gctFileMatrix <- data.matrix(gctFile)
                    gctCASRNNames <- rownames(gctFile)
                    gctAnnoNames <- colnames(gctFile)
                    # Get list of paths to result files for each set
                    # Fetch setFiles from server via API
                    resp <- NULL
                    tryResults <- tryCatch({
                        resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getResults"), query=list(transactionId=transactionId, setName=i))
                        TRUE
                    }, error=function(cond){
                        return(FALSE)
                    })
                    if(!tryResults){
                        showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                        return(FALSE)
                    }
                    if(resp$status_code != 200){
                        return(NULL)
                    }
                    getAllResultFiles <- unlist(content(resp))
                  
                    # Get original input chemical if similarity or substructure
                    originalInputToDisplay <- ""
                    if(mode == "substructure" | mode == "similarity"){
                        lapply(originalNames, function(j){
                            originalSetName <- unlist(str_split(j, "__"))
                            if(originalSetName[2] == i){
                                originalInputToDisplay <<- paste0("Input chemical: ", originalSetName[1])
                            }
                        }) 
                    }

                    # Dynamically create observers for result file links
                    # Input
                    if(is.null(setFilesObservers$observers[[paste0("inputObserver__", i)]])){
                        setFilesObservers$observers[[paste0("inputObserver__", i)]] <- observeEvent(input[[paste0(i, "__", i, ".txt__link")]], {
                            # Create directory for request
                            dir.create(paste0(tempdir(), "/input/"))
                            dir.create(paste0(tempdir(), "/input/", transactionId, "/"))
                            tryInputDL <- TRUE
                            if(!file.exists(paste0(tempdir(), "/input/", transactionId, "/", i, ".txt"))){
                                tryInputDL <- tryCatch({
                                    download.file(paste0(API_PROTOCOL, API_ADDR, "/serveFileText?transactionId=", transactionId, "&filename=", i, ".txt&subDir=Input"), destfile=paste0(tempdir(), "/input/", transactionId, "/", i, ".txt"))
                                }, error=function(cond){
                                    return(NULL)
                                })
                            }
                            if(is.null(tryInputDL)){
                                showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                            } else {
                                # Check file size
                                fSize <- file.info(paste0(tempdir(), "/input/", transactionId, "/", i, ".txt"))$size
                                if(fSize > 1000000){ # show preview error if file is larger than 1MB
                                    showNotification(paste0("Warning: this file is too large to be previewed. Please view it after downloading the result files."), duration=5, type="warning")
                                    return(FALSE)
                                }
                                # Open preview in modal
                                # disable refreshbutton
                                shinyjs::disable(id="refresh")
                                tmpFile <- read.delim(paste0(tempdir(), "/input/", transactionId, "/", i, ".txt"), sep="\t", header=FALSE, comment.char="", fill=TRUE, stringsAsFactors=FALSE)
                                names(tmpFile) <- c("CASRN", "Chemical Name")
                                showModal(
                                    modalDialog(
                                        title=paste0("Input file for ", i),
                                        footer=actionButton(inputId="modalCloseInputEnrichment", label="Close preview"),
                                        size="l",
                                        fluidRow(
                                            column(12, 
                                                dataTableOutput("inputPreview")
                                            )
                                        )
                                    )
                                )
                                output[["inputPreview"]] <- renderDataTable(
                                    DT::datatable({tmpFile},
                                        escape=FALSE,
                                        rownames=FALSE,
                                        class="row-border stripe compact",
                                        style="bootstrap",
                                        select="none",
                                        options=list( 
                                            paging=FALSE,
                                            dom="Bfrtip",
                                            pageLength=10,
                                            buttons=list("copy", "csv", "excel", "pdf", "print")  
                                        ),
                                        extensions="Buttons"
                                    )
                                )
                            }
                        }, ignoreInit=TRUE)
                    }
                    observeEvent(input$modalCloseInputEnrichment, {
                        # enable refreshbutton
                        shinyjs::enable(id="refresh")
                        removeModal()
                    })
                  
                    # Chart.txt
                    if(is.null(setFilesObservers$observers[[paste0("chartTxtObserver__", i)]])){
                        setFilesObservers$observers[[paste0("chartTxtObserver__", i)]] <- observeEvent(input[[paste0(i, "__Chart.txt__link")]], {
                            # Create directory for request
                            dir.create(paste0(tempdir(), "/output/"))
                            dir.create(paste0(tempdir(), "/output/", transactionId, "/"))
                            tryChartDL <- TRUE
                            if(!file.exists(paste0(tempdir(), "/output/", transactionId, "/", i, "__Chart.txt"))){
                                tryChartDL <- tryCatch({
                                    download.file(paste0(API_PROTOCOL, API_ADDR, "/serveFileText?transactionId=", transactionId, "&filename=", i, "__Chart.txt&subDir=Output"), destfile=paste0(tempdir(), "/output/", transactionId, "/", i, "__Chart.txt"))
                                }, error=function(cond){
                                    return(NULL)
                                })
                            }
                            if(is.null(tryChartDL)){
                                showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                            } else {
                                # Check file size
                                fSize <- file.info(paste0(tempdir(), "/output/", transactionId, "/", i, "__Chart.txt"))$size
                                if(fSize > 1000000){ # show preview error if file is larger than 1MB
                                    showNotification(paste0("Warning: this file is too large to be previewed. Please view it after downloading the result files."), duration=5, type="warning")
                                    return(FALSE)
                                }
                                # Open preview in modal
                                # disable refreshbutton
                                shinyjs::disable(id="refresh")
                                tmpFile <- read.delim(paste0(tempdir(), "/output/", transactionId, "/", i, "__Chart.txt"), sep="\t", header=TRUE, comment.char="", fill=TRUE, stringsAsFactors=FALSE)
                                names(tmpFile) <- c("Category", "Term", "Count", "%", "PValue", "CASRNs", "List Total", "Pop Hits", "Pop Total", "Fold Enrichment", "Bonferroni", "Benjamini", "FDR")
                                showModal(
                                    modalDialog(
                                        title=paste0("Chart file for ", i),
                                        footer=actionButton(inputId="modalCloseChart", label="Close preview"),
                                        size="l",
                                        fluidRow(
                                            column(12, 
                                                dataTableOutput("chartPreview")
                                            )
                                        )
                                    )
                                )
                                output[["chartPreview"]] <- renderDataTable(
                                    DT::datatable({tmpFile},
                                        escape=FALSE,
                                        rownames=FALSE,
                                        class="row-border stripe compact",
                                        style="bootstrap",
                                        select="none",
                                        options=list( 
                                            paging=FALSE,
                                            scrollX=TRUE,
                                            dom="Bfrtip",
                                            pageLength=10,
                                            buttons=list("copy", "csv", "excel", "pdf", "print"),
                                            rowCallback=JS(
                                                "function(row, data) {
                                                    for (i=1; i < data.length; i++) {
                                                        if (data[i] < 1){
                                                            $('td:eq('+i+')', row).html(data[i].toExponential(1));
                                                        }
                                                    }
                                                }"
                                            )
                                        ),
                                        extensions="Buttons"
                                    ) %>% formatRound(columns=c('PValue', 'Bonferroni', 'Benjamini', 'FDR'), digits=4) %>% formatRound(columns=c('%', 'Fold Enrichment'), digits=2)
                                )
                            }
                        }, ignoreInit=TRUE)
                    }
                    observeEvent(input$modalCloseChart, {
                        # enable refreshbutton
                        shinyjs::enable(id="refresh")
                        removeModal()
                    })
                    
                    # ChartSimple.txt
                    if(is.null(setFilesObservers$observers[[paste0("chartSimpleTxtObserver__", i)]])){
                        setFilesObservers$observers[[paste0("chartSimpleTxtObserver__", i)]] <- observeEvent(input[[paste0(i, "__ChartSimple.txt__link")]], {
                            # Create directory for request
                            dir.create(paste0(tempdir(), "/output/"))
                            dir.create(paste0(tempdir(), "/output/", transactionId, "/"))
                            tryChartSimpleDL <- TRUE
                            if(!file.exists(paste0(tempdir(), "/output/", transactionId, "/", i, "__ChartSimple.txt"))){
                                tryChartSimpleDL <- tryCatch({
                                    download.file(paste0(API_PROTOCOL, API_ADDR, "/serveFileText?transactionId=", transactionId, "&filename=", i, "__ChartSimple.txt&subDir=Output"), destfile=paste0(tempdir(), "/output/", transactionId, "/", i, "__ChartSimple.txt"))
                                }, error=function(cond){
                                    return(NULL)
                                })
                            }
                            if(is.null(tryChartSimpleDL)){
                                showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                            } else {
                                # Check file size
                                fSize <- file.info(paste0(tempdir(), "/output/", transactionId, "/", i, "__ChartSimple.txt"))$size
                                if(fSize > 1000000){ # show preview error if file is larger than 1MB
                                    showNotification(paste0("Warning: this file is too large to be previewed. Please view it after downloading the result files."), duration=5, type="warning")
                                    return(FALSE)
                                }
                                # Open preview in modal
                                # disable refreshbutton
                                shinyjs::disable(id="refresh")
                                tmpFile <- read.delim(paste0(tempdir(), "/output/", transactionId, "/", i, "__ChartSimple.txt"), sep="\t", header=TRUE, comment.char="", fill=TRUE, stringsAsFactors=FALSE)
                                names(tmpFile) <- c("Category", "Term", "Count", "%", "PValue", "Fold Enrichment", "Benjamini")
                                showModal(
                                    modalDialog(
                                        title=paste0("Chart simple file for ", i),
                                        footer=actionButton(inputId="modalCloseChartSimple", label="Close preview"),
                                        size="l",
                                        fluidRow(
                                            column(12, 
                                                dataTableOutput("chartSimplePreview")
                                            )
                                        )
                                    )
                                )
                                output[["chartSimplePreview"]] <- renderDataTable(
                                    DT::datatable({tmpFile},
                                        escape=FALSE,
                                        rownames=FALSE,
                                        class="row-border stripe compact",
                                        style="bootstrap",
                                        select="none",
                                        options=list( 
                                            paging=FALSE,
                                            scrollX=TRUE,
                                            dom="Bfrtip",
                                            pageLength=10,
                                            buttons=list("copy", "csv", "excel", "pdf", "print"),
                                            rowCallback=JS(
                                                "function(row, data) {
                                                    for (i=1; i < data.length; i++) {
                                                        if (data[i] < 1){
                                                            $('td:eq('+i+')', row).html(data[i].toExponential(1));
                                                        }
                                                    }
                                                }"
                                            )
                                        ),
                                        extensions="Buttons"
                                    ) %>% formatRound(columns=c('PValue', 'Benjamini'), digits=4) %>% formatRound(columns=c('%', 'Fold Enrichment'), digits=2)
                                )
                            }
                        }, ignoreInit=TRUE)
                    }
                    observeEvent(input$modalCloseChartSimple, {
                        # enable refreshbutton
                        shinyjs::enable(id="refresh")
                        removeModal()
                    })
                    
                    # Cluster.txt
                    if(is.null(setFilesObservers$observers[[paste0("clusterTxtObserver__", i)]])){
                        setFilesObservers$observers[[paste0("clusterTxtObserver__", i)]] <- observeEvent(input[[paste0(i, "__Cluster.txt__link")]], {
                            # Create directory for request
                            dir.create(paste0(tempdir(), "/output/"))
                            dir.create(paste0(tempdir(), "/output/", transactionId, "/"))
                            tryClusterDL <- TRUE
                            if(!file.exists(paste0(tempdir(), "/output/", transactionId, "/", i, "__Cluster.txt"))){
                                tryClusterDL <- tryCatch({
                                    download.file(paste0(API_PROTOCOL, API_ADDR, "/serveFileText?transactionId=", transactionId, "&filename=", i, "__Cluster.txt&subDir=Output"), destfile=paste0(tempdir(), "/output/", transactionId, "/", i, "__Cluster.txt"))
                                }, error=function(cond){
                                    return(NULL)
                                })
                            }
                            if(is.null(tryClusterDL)){
                                showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                            } else {
                                # Check file size
                                fSize <- file.info(paste0(tempdir(), "/output/", transactionId, "/", i, "__Cluster.txt"))$size
                                if(fSize > 1000000){ # show preview error if file is larger than 1MB
                                    showNotification(paste0("Warning: this file is too large to be previewed. Please view it after downloading the result files."), duration=5, type="warning")
                                    return(FALSE)
                                }
                                # Open preview in modal
                                tmpFile <- readLines(paste0(tempdir(), "/output/", transactionId, "/", i, "__Cluster.txt"))
                                # Create a separate data frame for each cluster
                                clusterNames <- unlist(lapply(tmpFile, function(x){
                                    if(grepl("^Annotation Cluster", x)) {
                                        return(x) # extract cluster names & scores
                                    }
                                    return(NULL)
                                }))
                                clusterNames <- clusterNames[!vapply(clusterNames, is.null, FUN.VALUE=logical(1))]
                                clusterList <- lapply(seq_len(length(clusterNames)), function(x) list())
                                names(clusterList) <- lapply(clusterNames, function(x) unlist(str_split(x, "\t"))[1]) # Remove enrichment scores
                                enrichmentScores <- lapply(clusterNames, function(x) unlist(str_split(x, "\t"))[2]) # Remove enrichment scores
                                names(enrichmentScores) <- names(clusterList)
                                currentCluster <- ""
                                tmpFileDFs <- unlist(lapply(tmpFile, function(x){
                                    if(grepl("^Annotation Cluster", x)) {
                                        currentCluster <<- x
                                        return(NULL)
                                    } else if(grepl("^Category", x)){
                                        #Do nothing, skip header
                                        return(NULL)
                                    } else {
                                        if(nchar(x) > 0){
                                            clusterName <- unlist(str_split(currentCluster, "\t"))[1]
                                            return(paste0(clusterName, "__", x))
                                        }
                                        return(NULL)
                                    }
                                }))
                                tmpFileDFs <- tmpFileDFs[!vapply(tmpFileDFs, is.null, FUN.VALUE=logical(1))]
                                filteredTmpFileDFs <- lapply(names(clusterList), function(x){
                                    innerTmpFileDFs <- lapply(tmpFileDFs, function(y){
                                        tmpSplit <- unlist(str_split(y, "__"))
                                        clusterName <- tmpSplit[1]
                                        clusterValues <- tmpSplit[2]
                                        if(clusterName == x) {
                                            return(unlist(str_split(clusterValues, "\t")))
                                        }
                                        return(NULL)
                                    })
                                    innerTmpFileDFs <- innerTmpFileDFs[!vapply(innerTmpFileDFs, is.null, FUN.VALUE=logical(1))]
                                    return(innerTmpFileDFs)
                                })
                                names(filteredTmpFileDFs) <- names(clusterList)
                                filteredTmpFileDFs <- lapply(filteredTmpFileDFs, function(x){
                                    tmpDF <- do.call(rbind.data.frame, x)
                                    colnames(tmpDF) <- c("Category", "Term", "Count", "%", "PValue", "CASRNs", "List Total", "Pop Hits", "Pop Total", "Fold Enrichment", "Bonferroni", "Benjamini", "FDR")
                                    rownames(tmpDF) <- seq_len(nrow(tmpDF))
                                    return(tmpDF)
                                })
                                # disable refreshbutton
                                shinyjs::disable(id="refresh")
                                showModal(
                                    modalDialog(
                                        title=paste0("Cluster file for ", i),
                                        footer=actionButton(inputId="modalCloseCluster", label="Close preview"),
                                        size="l",
                                        fluidRow(
                                            lapply(names(filteredTmpFileDFs), function(x){
                                                column(12, 
                                                    DT::dataTableOutput(paste0("clusterPreview__", x))
                                                )
                                            })
                                        )
                                    )
                                )
                                lapply(names(filteredTmpFileDFs), function(x){
                                    output[[paste0("clusterPreview__", x)]] <- DT::renderDataTable(
                                        DT::datatable({filteredTmpFileDFs[[x]]},
                                            escape=FALSE,
                                            rownames=FALSE,
                                            class="row-border stripe compact",
                                            style="bootstrap",
                                            select="none",
                                            caption=HTML(paste0("<span style='color:", theme$textcolor, ";'>", x, " - ", enrichmentScores[x], "</span>")),
                                            options=list( 
                                                paging=FALSE,
                                                scrollX=TRUE,
                                                dom="Bfrtip",
                                                pageLength=10,
                                                buttons=list("copy", "csv", "excel", "pdf", "print"),
                                                rowCallback=JS( # formatting for float values in table (rounding)
                                                    "function(row, data) {
                                                        if (parseFloat(data[3]) < 1){ // %
                                                            $('td:eq('+3+')', row).html(parseFloat(data[3]).toExponential(1));
                                                        }
                                                        if (parseFloat(data[4]) < 1){ // Pvalue
                                                            $('td:eq('+4+')', row).html(parseFloat(data[4]).toExponential(1));
                                                        }
                                                        if (parseFloat(data[9]) < 1){ // Fold Enrichment
                                                            $('td:eq('+9+')', row).html(parseFloat(data[9]).toExponential(1));
                                                        }
                                                        if (parseFloat(data[10]) < 1){ // Bonferroni
                                                            $('td:eq('+10+')', row).html(parseFloat(data[10]).toExponential(1));
                                                        }
                                                        if (parseFloat(data[11]) < 1){ // Benjamini
                                                            $('td:eq('+11+')', row).html(parseFloat(data[11]).toExponential(1));
                                                        }
                                                        if (parseFloat(data[12]) < 1){ // FDR
                                                            $('td:eq('+12+')', row).html(parseFloat(data[12]).toExponential(1));
                                                        }
                                                    }"
                                                )
                                            ),
                                            extensions="Buttons"
                                        ) %>% formatRound(columns=c('PValue', 'Bonferroni', 'Benjamini', 'FDR'), digits=4) %>% formatRound(columns=c('%', 'Fold Enrichment'), digits=2)
                                    )
                                })
                            }
                        }, ignoreInit=TRUE)
                    }
                    observeEvent(input$modalCloseCluster, {
                        # enable refreshbutton
                        shinyjs::enable(id="refresh")
                        removeModal()
                    })

                    # Matrix.txt
                    if(is.null(setFilesObservers$observers[[paste0("matrixObserver__", i)]])){
                        setFilesObservers$observers[[paste0("matrixObserver__", i)]] <- observeEvent(input[[paste0(i, "__Matrix.txt__link")]], {
                            # Create directory for request
                            dir.create(paste0(tempdir(), "/output/"))
                            dir.create(paste0(tempdir(), "/output/", transactionId, "/"))
                            tryMatrixDL <- TRUE
                            if(!file.exists(paste0(tempdir(), "/output/", transactionId, "/", i, "__Matrix.txt"))){
                                tryMatrixDL <- tryCatch({
                                    download.file(paste0(API_PROTOCOL, API_ADDR, "/serveFileText?transactionId=", transactionId, "&filename=", i, "__Matrix.txt&subDir=Output"), destfile=paste0(tempdir(), "/output/", transactionId, "/", i, "__Matrix.txt"))
                                }, error=function(cond){
                                    return(NULL)
                                })
                            }
                            if(is.null(tryMatrixDL)){
                                showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                            } else {
                                # Check file size
                                fSize <- file.info(paste0(tempdir(), "/output/", transactionId, "/", i, "__Matrix.txt"))$size
                                if(fSize > 1000000){ # show preview error if file is larger than 1MB
                                    showNotification(paste0("Warning: this file is too large to be previewed. Please view it after downloading the result files."), duration=5, type="warning")
                                    return(FALSE)
                                }
                                # Open preview in modal
                                # disable refreshbutton
                                shinyjs::disable(id="refresh")
                                tmpFile <- read.csv(paste0(tempdir(), "/output/", transactionId, "/", i, "__Matrix.txt"), sep="\t", header=TRUE, comment.char="", fill=TRUE, stringsAsFactors=FALSE)
                                names(tmpFile) <- lapply(names(tmpFile), function(x){
                                    return(gsub("\\.", " ", x))
                                })
                                showModal(
                                    modalDialog(
                                        title=paste0("Matrix file for ", i),
                                        footer=actionButton(inputId="modalCloseMatrix", label="Close preview"),
                                        size="l",
                                        fluidRow(
                                            column(12, 
                                                dataTableOutput("matrixPreview")
                                            )
                                        )
                                    )
                                )
                                output[["matrixPreview"]] <- renderDataTable(
                                    DT::datatable({tmpFile},
                                        escape=FALSE,
                                        rownames=FALSE,
                                        class="row-border stripe compact",
                                        style="bootstrap",
                                        select="none",
                                        options=list( 
                                            paging=FALSE,
                                            scrollX=TRUE,
                                            dom="Bfrtip",
                                            pageLength=10,
                                            buttons=list("copy", "csv", "excel", "pdf", "print")  
                                        ),
                                        extensions="Buttons"
                                    )
                                )
                            }
                        }, ignoreInit=TRUE)
                    }
                    observeEvent(input$modalCloseMatrix, {
                        # enable refreshbutton
                        shinyjs::enable(id="refresh")
                        removeModal()
                    })
                    
                    # ErrorCASRNs.txt
                    if(is.null(setFilesObservers$observers[[paste0("errorfileObserver__", i)]])){
                        setFilesObservers$observers[[paste0("errorfileObserver__", i)]] <- observeEvent(input[[paste0(i, "__ErrorCASRNs.txt__link")]], {
                            # Create directory for request
                            dir.create(paste0(tempdir(), "/output/"))
                            dir.create(paste0(tempdir(), "/output/", transactionId, "/"))
                            tryErrorDL <- TRUE
                            if(!file.exists(paste0(tempdir(), "/output/", transactionId, "/", i, "__ErrorCASRNs.txt"))){
                                tryErrorDL <- tryCatch({
                                    download.file(paste0(API_PROTOCOL, API_ADDR, "/serveFileText?transactionId=", transactionId, "&filename=", i, "__ErrorCASRNs.txt&subDir=Output"), destfile=paste0(tempdir(), "/output/", transactionId, "/", i, "__ErrorCASRNs.txt"))
                                }, error=function(cond){
                                    return(NULL)
                                })
                            }
                            if(is.null(tryErrorDL)){
                                showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                            } else {
                                # Check file size
                                fSize <- file.info(paste0(tempdir(), "/output/", transactionId, "/", i, "__ErrorCASRNs.txt"))$size
                                if(fSize > 1000000){ # show preview error if file is larger than 1MB
                                    showNotification(paste0("Warning: this file is too large to be previewed. Please view it after downloading the result files."), duration=5, type="warning")
                                    return(FALSE)
                                }
                                # Open preview in modal
                                # disable refreshbutton
                                shinyjs::disable(id="refresh")
                                tmpFile <- read.delim(paste0(tempdir(), "/output/", transactionId, "/", i, "__ErrorCASRNs.txt"), sep="\t", header=FALSE, comment.char="", fill=TRUE, stringsAsFactors=FALSE)
                                names(tmpFile) <- c("CASRN")
                                showModal(
                                    modalDialog(
                                        title=paste0("Error CASRNs for ", i),
                                        footer=actionButton(inputId="modalCloseErrorEnrichment", label= "Close preview"),
                                        size="l",
                                        fluidRow(
                                            column(12, 
                                                dataTableOutput("errorPreview")
                                            )
                                        )
                                    )
                                )
                                output[["errorPreview"]] <- renderDataTable(
                                    DT::datatable({tmpFile},
                                        escape=FALSE,
                                        rownames=FALSE,
                                        class="row-border stripe compact",
                                        style="bootstrap",
                                        select="none",
                                        options=list( 
                                            paging=FALSE,
                                            scrollX=TRUE,
                                            dom="Bfrtip",
                                            pageLength=10,
                                            buttons=list("copy", "csv", "excel", "pdf", "print")  
                                        ),
                                        extensions="Buttons"
                                    )
                                )
                            }
                        }, ignoreInit=TRUE)
                    }
                    observeEvent(input$modalCloseErrorEnrichment, {
                        # enable refreshbutton
                        shinyjs::enable(id="refresh")
                        removeModal()
                    })
                    
                    # Generate default reactive values for colors
                    lHeatmapColor[[paste0("color__", i)]] <- "white"
                    hHeatmapColor[[paste0("color__", i)]] <- "red"
                    
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
                                        # Strip path from file
                                        tmpName <- unlist(str_split(resultFile, "/"))
                                        # If input file
                                        tooltipToUse <- ""
                                        if(endsWith(resultFile, paste0(i, ".txt"))) {
                                            fluidRow(
                                                # Add tooltips
                                                # Input
                                                column(3, id=paste0(tmpName[length(tmpName)]), tipify( div(actionLink(inputId=paste0(i, "__", i, ".txt__link"), label=paste0(unlist(str_split(tmpName[length(tmpName)], ".txt"))[1], " Input"))), "A list of input chemicals for this set (.txt format).", placement="bottom") )
                                            )
                                        } else {
                                            fluidRow(
                                                # Chart.txt
                                                if(endsWith(resultFile, paste0(i, "__Chart.txt"))){
                                                    column(3, id=paste0(tmpName[length(tmpName)]), tipify( div(actionLink(inputId=paste0(i, "__Chart.txt__link"), label=paste0(gsub(".txt", "", gsub("__", " ", tmpName[length(tmpName)]))))), "A list of all significant annotations (.txt format).", placement="bottom") )
                                                },
                                                # ChartSimple.txt
                                                if(endsWith(resultFile, paste0(i, "__ChartSimple.txt"))){
                                                    column(3, id=paste0(tmpName[length(tmpName)]), tipify( div(actionLink(inputId=paste0(i, "__ChartSimple.txt__link"), label=paste0(gsub("ChartSimple", "Chart Simple", gsub(".txt", "", gsub("__", " ", tmpName[length(tmpName)])))))), "A list of the top N most significant annotations for each annotation class, where N is the specified cutoff value (.txt format).", placement="bottom") )
                                                },
                                                # Cluster.txt
                                                if(endsWith(resultFile, paste0(i, "__Cluster.txt"))){
                                                    column(3, id=paste0(tmpName[length(tmpName)]), tipify( div(actionLink(inputId=paste0(i, "__Cluster.txt__link"), label=paste0(gsub(".txt", "", gsub("__", " ", tmpName[length(tmpName)]))))), "A list of significant terms in which functionally similar annotations are grouped together to remove redundancy. This is performed with respect to the whole annotation set rather than to individual annotation classes (.txt format).", placement="bottom") )
                                                },
                                                # Matrix
                                                if(endsWith(resultFile, paste0(i, "__Matrix.txt"))){
                                                    column(3, id=paste0(tmpName[length(tmpName)]), tipify( div(actionLink(inputId=paste0(i, "__Matrix.txt__link"), label=paste0(gsub(".txt", "", gsub("__", " ", tmpName[length(tmpName)]))))), "A text representation of the heatmap (.txt format).", placement="bottom") )
                                                },
                                                # Error CASRNs list
                                                if(endsWith(resultFile, paste0(i, "__ErrorCASRNs.txt"))){
                                                    column(3, id=paste0(tmpName[length(tmpName)]), tipify( div(actionLink(inputId=paste0(i, "__ErrorCASRNs.txt__link"), label=paste0(gsub("ErrorCASRNs", "Error CASRNs", gsub(".txt", "", gsub("__", " ", tmpName[length(tmpName)])))))), "A list of submitted CASRNs that were not found in the Tox21 Enricher database (.txt format).", placement="bottom") )
                                                }
                                            )
                                        }
                                    })  
                                )
                            ),
                            # Main heatmap per set
                            # fluidRow(
                            #     column(1, 
                            #         radioButtons(inputId=paste0("lHeatmapColorControl__", i), label="Select 0 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected="white"),
                            #     ),
                            #     column(1,
                            #         radioButtons(inputId=paste0("hHeatmapColorControl__", i), label="Select 1 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected="red")
                            #     ),
                            #     column(10,
                            #         plot_ly(x=gctAnnoNames, y=gctCASRNNames, z=gctFileMatrix, colors=colorRamp(c(lHeatmapColor[[paste0("color__", i)]], hHeatmapColor[[paste0("color__", i)]])), type="heatmap", xgap=2, ygap=2 ) %>% 
                            #         layout(
                            #             autosize=TRUE, 
                            #             showlegend=FALSE, 
                            #             margin=list(r=200, b=200), 
                            #             xaxis=list(title="<b>Annotations</b>", tickfont=list(size=9), tickangle=15, automargin=TRUE), 
                            #             yaxis=list(title="<b>Input CASRNs</b>", type="category", automargin=TRUE),
                            #             plot_bgcolor="transparent",
                            #             paper_bgcolor="transparent",
                            #             font=list(color=theme$textcolor)
                            #         ) %>% hide_colorbar()
                            #     )
                            # ),
                            uiOutput(paste0("perSetHeatmap__", i)),
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
                    
                    output[[paste0("perSetHeatmap__", i)]] <- renderUI(
                        # Main heatmap per set
                        fluidRow(
                            column(1,
                               radioButtons(inputId=paste0("lHeatmapColorControl__", i), label="Select 0 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected=lHeatmapColor[[paste0("color__", i)]]),
                            ),
                            column(1,
                               radioButtons(inputId=paste0("hHeatmapColorControl__", i), label="Select 1 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected=hHeatmapColor[[paste0("color__", i)]])
                            ),
                            column(10,
                                plot_ly(x=gctAnnoNames, y=gctCASRNNames, z=gctFileMatrix, colors=colorRamp(c(lHeatmapColor[[paste0("color__", i)]], hHeatmapColor[[paste0("color__", i)]])), type="heatmap", xgap=2, ygap=2 ) %>%
                                layout(
                                    autosize=TRUE,
                                    showlegend=FALSE,
                                    margin=list(r=200, b=200),
                                    xaxis=list(title="<b>Annotations</b>", tickfont=list(size=9), tickangle=15, automargin=TRUE),
                                    yaxis=list(title="<b>Input CASRNs</b>", type="category", automargin=TRUE),
                                    plot_bgcolor="transparent",
                                    paper_bgcolor="transparent",
                                    font=list(color=theme$textcolor)
                                ) %>% hide_colorbar()
                            )
                        )
                    )
                    
                    # add observers for heatmap colors
                    observeEvent(input[[paste0("lHeatmapColorControl__", i)]], {
                        lHeatmapColor[[paste0("color__", i)]] <- input[[paste0("lHeatmapColorControl__", i)]]
                        output[[paste0("perSetHeatmap__", i)]] <- renderUI(
                                # Main heatmap per set
                                fluidRow(
                                    column(1,
                                        radioButtons(inputId=paste0("lHeatmapColorControl__", i), label="Select 0 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected=lHeatmapColor[[paste0("color__", i)]]),
                                    ),
                                    column(1,
                                        radioButtons(inputId=paste0("hHeatmapColorControl__", i), label="Select 1 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected=hHeatmapColor[[paste0("color__", i)]])
                                    ),
                                    column(10,
                                        plot_ly(x=gctAnnoNames, y=gctCASRNNames, z=gctFileMatrix, colors=colorRamp(c(lHeatmapColor[[paste0("color__", i)]], hHeatmapColor[[paste0("color__", i)]])), type="heatmap", xgap=2, ygap=2 ) %>%
                                        layout(
                                            autosize=TRUE,
                                            showlegend=FALSE,
                                            margin=list(r=200, b=200),
                                            xaxis=list(title="<b>Annotations</b>", tickfont=list(size=9), tickangle=15, automargin=TRUE),
                                            yaxis=list(title="<b>Input CASRNs</b>", type="category", automargin=TRUE),
                                            plot_bgcolor="transparent",
                                            paper_bgcolor="transparent",
                                            font=list(color=theme$textcolor)
                                        ) %>% hide_colorbar()
                                    )
                                )
                          )
                    })
                    observeEvent(input[[paste0("hHeatmapColorControl__", i)]], {
                        hHeatmapColor[[paste0("color__", i)]] <- input[[paste0("hHeatmapColorControl__", i)]]
                        output[[paste0("perSetHeatmap__", i)]] <- renderUI(
                            # Main heatmap per set
                            fluidRow(
                                column(1,
                                   radioButtons(inputId=paste0("lHeatmapColorControl__", i), label="Select 0 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected=lHeatmapColor[[paste0("color__", i)]]),
                                ),
                                column(1,
                                   radioButtons(inputId=paste0("hHeatmapColorControl__", i), label="Select 1 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected=hHeatmapColor[[paste0("color__", i)]])
                                ),
                                column(10,
                                    plot_ly(x=gctAnnoNames, y=gctCASRNNames, z=gctFileMatrix, colors=colorRamp(c(lHeatmapColor[[paste0("color__", i)]], hHeatmapColor[[paste0("color__", i)]])), type="heatmap", xgap=2, ygap=2 ) %>%
                                    layout(
                                        autosize=TRUE,
                                        showlegend=FALSE,
                                        margin=list(r=200, b=200),
                                        xaxis=list(title="<b>Annotations</b>", tickfont=list(size=9), tickangle=15, automargin=TRUE),
                                        yaxis=list(title="<b>Input CASRNs</b>", type="category", automargin=TRUE),
                                        plot_bgcolor="transparent",
                                        paper_bgcolor="transparent",
                                        font=list(color=theme$textcolor)
                                    ) %>% hide_colorbar()
                                )
                            )
                        )
                    })
                } else {
                    # Get original input chemical if similarity or substructure
                    originalInputToDisplay <- NULL
                    if(mode == "substructure" | mode == "similarity"){
                        originalInputToDisplay <- lapply(originalNames, function(j){
                            originalSetName <- unlist(str_split(j, "__"))
                            if(originalSetName[2] == i){
                                return(paste0("Input chemical: ", originalSetName[1]))
                            }
                            return(NULL)
                        })
                        originalInputToDisplay <- originalInputToDisplay[!vapply(originalInputToDisplay, is.null, FUN.VALUE=logical(1))][1]
                    }
                    output[[paste0("outTab_", i)]] <- renderUI(
                        column(12,
                            tabPanel(paste0("tab_", i), 
                                if(!is.null(originalInputToDisplay)){
                                    fluidRow(p(paste0(originalInputToDisplay)))
                                },
                                fluidRow(p(paste0("No result files for set: ", i)))
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
                }
            })
            # Render reenrichment if Substructure or Similarity
            reenrichResultsTotalLength <- length(unlist(lapply(names(reenrichResults), function(i) unlist(reenrichResults[[i]][, "casrn"]))))
            reenrichResultsTotalLength <- reenrichResultsTotalLength + sum(unlist(lapply(names(reenrichResults), function(i) nrow(reenrichResults[[i]]))))
            # Checkbox input to select chemicals for re-enrichment
            reenrichChoices <- lapply(names(reenrichResults), function(i) reenrichResults[[i]][, "casrn"])
            names(reenrichChoices) <- names(reenrichResults)
            checkboxes <- lapply(names(reenrichChoices), function(reenrichSet){
                tmp_checkboxes <- lapply(reenrichChoices[[reenrichSet]], function(x) checkboxInput(inputId=paste0(x, "__", reenrichSet), value=TRUE, label=NULL, width="4px"))
                names(tmp_checkboxes) <- lapply(reenrichChoices[[reenrichSet]], function(x) paste0(x, "__", reenrichSet))
                return(tmp_checkboxes)
            })
            names(checkboxes) <- names(reenrichChoices)

            # Set reactive value so we can access these checkboxes later
            checkboxList$checkboxes <- checkboxes

            # Generate list of svg images
            svgImagesList <- list()
            if (originalEnrichMode != "casrn") {
                lapply(names(reenrichResults), function(i) {
                    # Get chemical structure images and add to table
                    imgPath1 <- '<img src="images/structures/'
                    imgPath2 <- ' height="100" width="100"></img>'
                    # query API to generate images
                    resultImagesSVG <- unlist(lapply(seq_len(nrow(reenrichResults[[i]]["casrn"])), function(x) paste0(reenrichResults[[i]][x, "casrn"], "__", reenrichResults[[i]][x, "m"])))
                    resultImagesSVG <- paste0(resultImagesSVG, collapse="\n")
                    # Get SVG images from server
                    resp <- NULL
                    trySVG <- tryCatch({
                        resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getStructureImages"), query=list(input=resultImagesSVG))
                        TRUE
                    }, error=function(cond){
                        return(FALSE)
                    })
                    if(!trySVG){
                        showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                        return(FALSE)
                    }
                    
                    structuresSvg <- content(resp)
                    resultImages <- lapply(reenrichResults[[i]][, "casrn"], function(casrnName) {
                        # Note: this is kind of a hacky way to resize the SVG images generated by rdkit, but it works...
                        resizedSvg <- gsub("width='250px'", "width='50px'", unlist(structuresSvg[casrnName][[1]]))
                        resizedSvg <- gsub("height='200px'", "height='40px'", resizedSvg)
                        
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
                                ), id=paste0("div__ab__img__", casrnName, "__", i)
                            )
                        )))
                    })

                    # Get additional information for each CASRN from database and add to table
                    expandedInfo <- t(vapply(reenrichResults[[i]][, "casrn"], function(casrn){
                        dummy <- c("", "", "", "", "", "", "", "") # dummy list to return in case we have errors fetching data
                        resp <- NULL
                        tryCasrn <- tryCatch({
                            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getCasrnData"), query=list(input=casrn))
                            TRUE
                        }, error=function(cond){
                            return(dummy)
                        })
                        if(!tryCasrn){
                            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                            return(dummy)
                        }
                        if(resp$status_code != 200){
                            return(dummy)
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
                        svgExpanded <- gsub("width='50px'", "width='600px'", svgExpanded)
                        svgExpanded <- gsub("height='40px'", "height='480px'", svgExpanded)
      
                        # Create observer
                        if(is.null(setFilesObservers$observers[[paste0("svgObserver__", casrn, "__", i)]])){
                            setFilesObservers$observers[[paste0("svgObserver__", casrn, "__", i)]] <- observeEvent(input[[paste0("ab__img__", casrn, "__", i)]], {
                                # disable refreshbutton
                                shinyjs::disable(id="refresh")
                                showModal(
                                    modalDialog(
                                        title=casrn,
                                        footer=actionButton(inputId="modalCloseStructure", label="Close"),
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
                                            column(9, HTML(round(as.numeric(outputWeight), digits=2)))
                                        ),
                                        fluidRow(
                                            column(3, HTML(paste0("<a href=\"https://comptox.epa.gov/dashboard/dsstoxdb/results?search=", outputDtxsid, "&abbreviation=TOX21SL\">View at EPA</a>"))),
                                            column(9, HTML(paste0("<a href=\"https://pubchem.ncbi.nlm.nih.gov/compound/", outputCid, "\">View at PubChem</a>")))
                                        )
                                    )
                                )
                            }, ignoreInit=TRUE)
                            observeEvent(input$modalCloseStructure, {
                                # enable refreshbutton
                                shinyjs::enable(id="refresh")
                                removeModal()
                            })
                        }
                        infoOutp <- c("iupac_name"=outputIupac, "smiles"=outputSmiles, "dtxsid"=outputDtxsid, "dtxrid"=outputDtxrid, "mol_formula"=outputFormula, "mol_weight"=outputWeight, "inchis"=outputInchi, "inchikey"=outputInchikey)
                        return(infoOutp)
                    }, FUN.VALUE=character(8)))
                    expandedInfo <- data.frame(expandedInfo, stringsAsFactors=FALSE)
                    row.names(expandedInfo) <- t(vapply(reenrichResults[[i]][, "casrn"], function(casrn) casrn, FUN.VALUE=character(1)))
                    # Create final data frame
                    fullTableTmp <- t(vapply(row.names(expandedInfo), function(casrn){
                        fullTableTmpInner <- lapply(seq_len(nrow(reenrichResults[[i]])), function(row){
                            if(reenrichResults[[i]][row, "casrn"] == casrn){
                                finalTable <- NULL
                                if(mode == "similarity" | originalEnrichMode == "similarity"){
                                    finalTable <- list(
                                        "Chemical_Structure"=resultImages[row], 
                                        "DTXSID"=expandedInfo[casrn, "dtxsid"], 
                                        "CASRN"=reenrichResults[[i]][row, "casrn"], 
                                        "IUPAC_Name"=expandedInfo[casrn, "iupac_name"], 
                                        "SMILES"=expandedInfo[casrn, "smiles"], 
                                        "InChI"=expandedInfo[casrn, "inchis"], 
                                        "InChIKey"=expandedInfo[casrn, "inchikey"],
                                        "Molecular_Formula"=expandedInfo[casrn, "mol_formula"],
                                        "Molecular_Weight"=expandedInfo[casrn, "mol_weight"],
                                        "Similarity"=round(as.numeric(reenrichResults[[i]][row, "similarity"]), digits=2), # Truncate similarity to hundredths place (easier to read)
                                        "Cyanide"=reenrichResults[[i]][row, "cyanide"],
                                        "Isocyanate"=reenrichResults[[i]][row, "isocyanate"],
                                        "Aldehyde"=reenrichResults[[i]][row, "aldehyde"],
                                        "Epoxide"=reenrichResults[[i]][row, "epoxide"]
                                    )
                                } else if(mode == "substructure" | originalEnrichMode == "substructure") { # Substructure (no similarity column)
                                    finalTable <- list(
                                        "Chemical_Structure"=resultImages[row], 
                                        "DTXSID"=expandedInfo[casrn, "dtxsid"], 
                                        "CASRN"=reenrichResults[[i]][row, "casrn"], 
                                        "IUPAC_Name"=expandedInfo[casrn, "iupac_name"], 
                                        "SMILES"=expandedInfo[casrn, "smiles"], 
                                        "InChI"=expandedInfo[casrn, "inchis"], 
                                        "InChIKey"=expandedInfo[casrn, "inchikey"],
                                        "Molecular_Formula"=expandedInfo[casrn, "mol_formula"],
                                        "Molecular_Weight"=expandedInfo[casrn, "mol_weight"],
                                        "Similarity"=NA,
                                        "Cyanide"=reenrichResults[[i]][row, "cyanide"],
                                        "Isocyanate"=reenrichResults[[i]][row, "isocyanate"],
                                        "Aldehyde"=reenrichResults[[i]][row, "aldehyde"],
                                        "Epoxide"=reenrichResults[[i]][row, "epoxide"]
                                    )
                                }
                                return(finalTable)
                            }
                            return(NULL)
                        })
                        fullTableTmpInner <- fullTableTmpInner[!vapply(fullTableTmpInner, is.null, FUN.VALUE=logical(1))]
                        return(fullTableTmpInner[[1]])
                    }, FUN.VALUE=list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))) # 14 because that is the number of columns as shown above
                    fullTableTmp <- data.frame(fullTableTmp)
                    imgPath1 <- '<img src="images/warnings/'
                    imgPath2 <- ' height="50" width="100"></img>'
                    # Check if original string contains any of the reactive structures
                    originalInputStr <-unlist(lapply(originalNames, function(j){
                        originalSetName <- unlist(str_split(j, "__"))
                        if(originalSetName[2] == i){
                            return(paste0(originalSetName[1]))
                        }
                        return(NULL)
                    }))
                    originalInputStr <- originalInputStr[!vapply(originalInputStr, is.null, FUN.VALUE=logical(1))]
                    resp <- NULL
                    tryReactive <- tryCatch({
                        resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getReactiveGroups"), query=list(input=originalInputStr))
                        TRUE
                    }, error=function(cond){
                        return(FALSE)
                    })
                    if(!tryReactive){
                        showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                        return(FALSE)
                    }
                    if(resp$status_code != 200){
                        return(NULL)
                    }
                    originalInputStrReactive <- unlist(str_split(content(resp), ","))
                    # 1=cyanide, 2=isocyanate, 3=aldehyde, 4=epoxide
                    # Simplify reactive structure columns into one warning column
                    fullTableWarnings <- lapply(seq_len(nrow(fullTableTmp)), function(tableRow){
                        warningToDisplay <- ""
                        nitrileCol <- fullTableTmp[tableRow, (ncol(fullTableTmp) - 3)]
                        isocyanateCol <- fullTableTmp[tableRow, (ncol(fullTableTmp) - 2)]
                        aldehydeCol <- fullTableTmp[tableRow, (ncol(fullTableTmp) - 1)]
                        epoxideCol <- fullTableTmp[tableRow, (ncol(fullTableTmp))]
                        if(toString(nitrileCol) != toString(originalInputStrReactive[1])) {
                            warningToDisplay <- paste0(tipify(div( HTML(paste0(warningToDisplay, imgPath1, 'nitrile.png"', imgPath2))), "Nitrile (Cyanide) group.", placement="left"))
                        }
                        if(toString(isocyanateCol) != toString(originalInputStrReactive[2])) {
                            warningToDisplay <- paste0(tipify(div( HTML(paste0(warningToDisplay, imgPath1, 'isocyanate.png"', imgPath2))), "Isocyanate group.", placement="left"))
                        }
                        if(toString(aldehydeCol) != toString(originalInputStrReactive[3])) {
                            warningToDisplay <- paste0(tipify(div( HTML(paste0(warningToDisplay, imgPath1, 'aldehyde.png"', imgPath2))),  "Aldehyde group.", placement="left"))
                        }
                        if(toString(epoxideCol) != toString(originalInputStrReactive[4])) {
                            warningToDisplay <- paste0(tipify(div( HTML(paste0(warningToDisplay, imgPath1, 'epoxide.png"', imgPath2))), "Epoxide group.", placement="left"))
                        }
                        if(warningToDisplay == "") { # if no warnings
                            warningToDisplay <- "<p>None</p>"
                        }
                        return(warningToDisplay)
                    })
                    # Create final table to display
                    # Truncate to remove individual warning columns
                    if (mode == "similarity" | originalEnrichMode == "similarity") {
                        fullTable <- fullTableTmp[, seq_len(10)]
                    } else if(mode == "substructure" | originalEnrichMode == "substructure") { # Substructure (no similarity column)
                        fullTable <- fullTableTmp[, seq_len(9)]
                    }

                    # Check if any warnings were generated
                    warningCheck <- lapply(fullTableWarnings, function(x){
                        if(x == "<p>None</p>"){
                            return(NULL)
                        }
                        return(TRUE)
                    })
                    warningCheck <- unique(warningCheck[!vapply(warningCheck, is.null, FUN.VALUE=logical(1))])
                    # Save warningCheck to reactive value so we can reference outside of this scope
                    warningList$warnings[[i]] <- warningCheck
                    # Create checkbox column to display
                    selectList <- lapply(checkboxes[[i]], function(x) paste0(x))
                    if(length(warningCheck) > 0){
                        # If we have warnings, then include the warning column
                        fullTable$warning <- fullTableWarnings  
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
                    rownames(fullTable) <- seq_len(nrow(fullTable))
                    # Check if chemicals with warnings exist
                    if(length(unlist(warningList$warnings[[i]], recursive=FALSE)) > 0){
                        haveWarnings$warnings <<- TRUE
                    }
                    if(mode != "casrn" | (originalEnrichMode == "substructure" | originalEnrichMode == "similarity")) {
                        output[[paste0("table_", i)]] <- renderUI(
                            column(12, style="height:500px; overflow-y:scroll;",
                                DT::datatable({fullTable},
                                # Render reenrichment table (solution from https://stackoverflow.com/questions/37356625/adding-a-column-with-true-false-and-showing-that-as-a-checkbox/37356792#37356792)
                                    escape=FALSE, 
                                    class="row-border stripe compact",
                                    rownames=FALSE,
                                    style="bootstrap",
                                    select="none",
                                    options=list( 
                                        paging=FALSE,
                                        preDrawCallback=JS('function() { Shiny.unbindAll(this.api().table().node()); }'), 
                                        drawCallback=JS('function() { Shiny.bindAll(this.api().table().node()); } '),
                                        dom="Bfrtip",
                                        pageLength=10,
                                        buttons=list("copy", "csv", "excel", "pdf", "print", list( extend="colvis", columns=as.vector(seq_len((ncol(fullTable)-1)))))  
                                        #                                                           ^^ this is so we always keep the select checkboxes in the table (user can't hide them)
                                    ),
                                    extensions="Buttons"
                                ) %>% formatRound(columns=c("Molecular Weight"), digits=2)
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
                        tipify(sliderInput(inputId="nodeCutoffRe", label="Re-enrichment Cutoff", value=10, min=1, max=50, step=1, width="100%"), "This will determine the maximum number of results per data set and may affect how many nodes are generated during network generation. (default=10). Higher values may cause the enrichment process to take longer (Not available when viewing annotations for Tox21 chemicals).", placement="bottom")
                    ),
                    hr()
                )
            )
            
            # Render re-enrich buttons
            output[["reenrichButtonOut"]] <- renderUI(
                fluidRow(
                    # Deselect per set button
                    column(id=paste0("selectAllSet__"), 6, 
                        if(mode != "casrn" | (originalEnrichMode == "substructure" | originalEnrichMode == "similarity")) {
                            actionButton("selectAllSetButton", "Deselect all chemicals for this set")   
                        }
                    ),
                    # Deselect all chemicals in all sets
                    column(id=paste0("selectAllReenrich"), 6, 
                        if(mode != "casrn" | (originalEnrichMode == "substructure" | originalEnrichMode == "similarity")) {
                            actionButton("selectAllReenrichButton", "Deselect all chemicals")   
                        }
                    ),
                    # Deselect all chemicals with warnings
                    column(id=paste0("selectAllWarningsReenrich"), 6,
                        if(haveWarnings$warnings == TRUE) {
                            if(mode != "casrn" | (originalEnrichMode == "substructure" | originalEnrichMode == "similarity")) {
                               actionButton("selectAllWarningsReenrichButton", HTML("<div class=\"text-danger\">Deselect all chemicals with warnings</div>"))
                            }
                        }
                    ),
                    # Reenrich selected chemicals
                    column(id=paste0("reenrichButtonCol"), 12, 
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

            # Render Chart & Cluster heatmaps for all sets
            gctFileChartMatrix <- NULL
            gctFileClusterMatrix <- NULL
            chartClasses <- NULL
            clusterClasses <- NULL
            gctCASRNNamesChart <- NULL
            gctAnnoNamesChart <- NULL
            gctCASRNNamesCluster <- NULL
            gctAnnoNamesCluster <- NULL
            
            # Query API to read in gct chart file
            # Fetch setFiles from server via API
            resp <- NULL
            tryGCT <- tryCatch({
                resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/readGct"), query=list(transactionId=transactionId, cutoff=cutoff, mode="chart"))
                TRUE
            }, error=function(cond){
                return(FALSE)
            })
            if(!tryGCT){
                showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                return(FALSE)
            }
            if(resp$status_code != 200){
                changePage(page="waiting")
                showNotification("Error: There was an error completing your request.", type="error")
                return(NULL)
            }
            if(length(content(resp)) > 0){
                colLength <- length(content(resp)[[1]]) - 1 # number of columns -1 to remove column name
                gctFileChart <- vapply(content(resp), function(x){
                    gctFileChartInner <- unlist(vapply(names(x), function(y) {
                        if(y != "_row"){
                            return(as.double(x[y]))  
                        } else {
                            return(0)
                        }
                    }, FUN.VALUE=numeric(1)))
                    gctFileChartInner <- gctFileChartInner[!grepl("_row", names(gctFileChartInner))]
                    gctFileChartInner <- gctFileChartInner[!vapply(gctFileChartInner, is.null, FUN.VALUE=logical(1))]
                    dfToReturn <- t(data.frame(gctFileChartInner, stringsAsFactors=FALSE))
                    row.names(dfToReturn) <- x["_row"]
                    return(dfToReturn)
                }, FUN.VALUE=double(colLength))
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
                    gctFileChartInner <- gctFileChartInner[!vapply(gctFileChartInner, is.null, FUN.VALUE=logical(1))]
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
                    gctFileChartInner <- gctFileChartInner[!vapply(gctFileChartInner, is.null, FUN.VALUE=logical(1))]
                    return(gctFileChartInner)
                })))
                colnames(gctFileChart) <- gctFileChartColNames
                row.names(gctFileChart) <- gctFileChartRowNames
                gctFileChartMatrix <- data.matrix(gctFileChart)
                gctCASRNNamesChart <- rownames(gctFileChart)
                gctAnnoNamesChart <- colnames(gctFileChart)
            }
            # Query API to read in gct cluster file
            # Fetch setFiles from server via API
            resp <- NULL
            tryGCT <- tryCatch({
                resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/readGct"), query=list(transactionId=transactionId, cutoff=cutoff, mode="cluster"))
                TRUE
            }, error=function(cond){
                return(FALSE)
            })
            if(!tryGCT){
                showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                return(FALSE)
            }
            if(resp$status_code != 200){
                return(NULL)
            }
            if(length(content(resp)) > 0){
                colLength <- length(content(resp)[[1]])-1 # number of columns -1 to remove column name
                gctFileCluster <- vapply(content(resp), function(x){
                    gctFileClusterInner <- unlist(vapply(names(x), function(y) {
                        if(y != "_row"){
                            return(as.double(x[y]))  
                        } else {
                            return(0)
                        }
                    }, FUN.VALUE=numeric(1)))
                    gctFileClusterInner <- gctFileClusterInner[!grepl("_row", names(gctFileClusterInner))]
                    gctFileClusterInner <- gctFileClusterInner[!vapply(gctFileClusterInner, is.null, FUN.VALUE=logical(1))]
                    dfToReturn <- t(data.frame(gctFileClusterInner, stringsAsFactors=FALSE))
                    row.names(dfToReturn) <- x["_row"]
                    return(dfToReturn)
                }, FUN.VALUE=double(colLength))
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
                    gctFileClusterInner <- gctFileClusterInner[!vapply(gctFileClusterInner, is.null, FUN.VALUE=logical(1))]
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
                    gctFileClusterInner <- gctFileClusterInner[!vapply(gctFileClusterInner, is.null, FUN.VALUE=logical(1))]
                    return(gctFileClusterInner)
                })))
                colnames(gctFileCluster) <- gctFileClusterColNames
                row.names(gctFileCluster) <- gctFileClusterRowNames
                gctFileClusterMatrix <- data.matrix(gctFileCluster)
                gctCASRNNamesCluster <- rownames(gctFileCluster)
                gctAnnoNamesCluster <- colnames(gctFileCluster)
            }
            # Transpose matrices for heatmap display and get relevant annotation class names
            if(!is.null(gctFileChartMatrix)){
                gctFileChartMatrix <- t(gctFileChartMatrix) 
                chartClasses <- unique(unlist(lapply(colnames(gctFileChartMatrix), function(x) unlist(str_split(x, " \\| "))[1])))
            }
            if(!is.null(gctFileClusterMatrix)){
                gctFileClusterMatrix <- t(gctFileClusterMatrix)
                clusterClasses <- unique(unlist(lapply(colnames(gctFileClusterMatrix), function(x) unlist(str_split(x, " \\| "))[1])))
            }
            # Generate networks for chart & cluster
            chartFullNetwork <- generateNetwork(transactionId=transactionId, cutoff=cutoff, networkMode="chart", inputNetwork=names(enrichmentSets), qval=0.05, physicsEnabled=FALSE, smoothCurve=TRUE, keep=chartClasses)
            clusterFullNetwork <- generateNetwork(transactionId=transactionId, cutoff=cutoff, networkMode="cluster", inputNetwork=names(enrichmentSets), qval=0.05, physicsEnabled=FALSE, smoothCurve=TRUE, keep=clusterClasses)
            
            lHeatmapColorChart$color <- "white"
            hHeatmapColorChart$color <- "red"
            lHeatmapColorCluster$color <- "white"
            hHeatmapColorCluster$color <- "red"
            
            # Display heatmap/network panels
            output[["chartHeatmap"]] <- renderUI(
                fluidRow(
                    column(12,
                        tabsetPanel(
                            tabPanel("Chart Heatmap", 
                                div(
                                    column(1, 
                                        radioButtons(inputId=paste0("lHeatmapColorControl__Chart"), label="Select 0 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected=lHeatmapColorChart$color),
                                    ),
                                    column(1,
                                        radioButtons(inputId=paste0("hHeatmapColorControl__Chart"), label="Select 1 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected=hHeatmapColorChart$color)
                                    ),
                                    column(10,
                                        plot_ly(x=gctCASRNNamesChart, y=gctAnnoNamesChart, z=gctFileChartMatrix, colors=colorRamp(c("white", "red")), type="heatmap", xgap=2, ygap=2, colorbar=list(title=list(text='<b>-log<sub>10</sub> (BH P-value)</b>'))) %>% 
                                        layout(
                                            margin=list(l=300, r=200, b=160), 
                                            xaxis=list(title="<b>Annotations</b>", tickfont=list(size=9), tickangle=15), 
                                            yaxis=list(title="<b>Input Sets</b>", type="category"),
                                            plot_bgcolor="transparent",
                                            paper_bgcolor="transparent",
                                            font=list(color=theme$textcolor)
                                        )
                                    )
                                )
                            ), 
                            tabPanel("Chart Network", 
                                fluidRow(
                                    column(3, 
                                        h4("Edge Selection Criteria"),
                                        numericInput(inputId="chartqval", label="q-value", value=0.05, step=0.01, max=1.00, min=0.01),
                                        checkboxGroupInput(label="Selected Input Sets", inputId="chartNetworkChoices", choices=names(enrichmentSets), selected=names(enrichmentSets)),
                                        checkboxGroupInput(label="Selected Annotation Classes", inputId="chartNetworkClasses", choices=chartClasses, selected=chartClasses),
                                        HTML("<h5><b>Other Options</b></h5>"),
                                        checkboxInput(inputId="physicsEnabledChart", label="Enable physics?", value=FALSE),
                                        checkboxInput(inputId="smoothCurveChart", label="Smooth curve for edges?", value=TRUE),
                                        actionButton(inputId="chartNetworkUpdateButton", label="Update network"),
                                        h4("More Information for Selected Annotation"),
                                        HTML("<p>Click on any <b>node</b> to view additional information for the annotation in the selected node.</p>"),
                                        h4("Overlapping Chemicals"),
                                        HTML("<p>Click on any <b>edge</b> to view a Venn diagram of the chemicals associated with the annotations in its two nodes.</p>"),
                                    ), 
                                    column(9,
                                        uiOutput("chartNetwork") %>% withSpinner()
                                    )
                                ),
                                hidden(
                                    fluidRow(id="nodeLinkChartMenu",
                                        column(12, 
                                            uiOutput("nodeLinkChart")
                                        )
                                    )
                                ),
                                hidden(
                                    fluidRow(id="vennChartMenu",
                                        column(4,
                                            uiOutput("vennChartButtons")
                                        ),
                                        column(8,
                                            plotOutput(
                                                outputId="vennChart"
                                            ) %>% withSpinner()
                                        )
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
                            tabPanel("Cluster Heatmap", 
                                div(
                                    column(1, 
                                        radioButtons(inputId=paste0("lHeatmapColorControl__Cluster"), label="Select 0 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected=lHeatmapColorCluster$color),
                                    ),
                                    column(1,
                                        radioButtons(inputId=paste0("hHeatmapColorControl__Cluster"), label="Select 1 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected=hHeatmapColorCluster$color)
                                    ),
                                    column(10,
                                        plot_ly(x=gctCASRNNamesCluster, y=gctAnnoNamesCluster, z=gctFileClusterMatrix, colors=colorRamp(c(lHeatmapColorCluster$color, hHeatmapColorCluster$color)), type="heatmap", xgap=2, ygap=2, colorbar=list(title=list(text='<b>-log<sub>10</sub> (BH P-value)</b>'))) %>% 
                                        layout(
                                            margin=list(l=300, r=200, b=160), 
                                            xaxis=list(title="<b>Annotation Clusters</b>", tickfont=list(size=9), tickangle=15), 
                                            yaxis=list(title="<b>Input Sets</b>", type="category"),
                                            plot_bgcolor="transparent",
                                            paper_bgcolor="transparent",
                                            font=list(color=theme$textcolor)
                                        )
                                    )
                                )
                            ),
                            tabPanel("Cluster Network", 
                                fluidRow(
                                    column(3, 
                                        h4("Edge Selection Criteria"),
                                        numericInput(inputId="clusterqval", label="q-value", value=0.05, step=0.01, max=1.00, min=0.01),
                                        checkboxGroupInput( label="Selected Input Sets", inputId="clusterNetworkChoices", choices=names(enrichmentSets), selected=names(enrichmentSets) ),
                                        checkboxGroupInput( label="Selected Annotation Classes", inputId="clusterNetworkClasses", choices=clusterClasses, selected=clusterClasses ),
                                        HTML("<h5><b>Other Options</b></h5>"),
                                        checkboxInput(inputId="physicsEnabledCluster", label="Enable physics?", value=FALSE),
                                        checkboxInput(inputId="smoothCurveCluster", label="Smooth curve for edges?", value=TRUE),
                                        actionButton(inputId="clusterNetworkUpdateButton", label="Update network"),
                                    ), 
                                    column(9,
                                        uiOutput("clusterNetwork") %>% withSpinner()
                                    )
                                ),
                                hidden(
                                    fluidRow(id="nodeLinkClusterMenu",
                                        column(12, 
                                            uiOutput("nodeLinkCluster")
                                        )
                                    )
                                ),
                                hidden(
                                    fluidRow(id="vennClusterMenu",
                                        column(4,
                                            uiOutput("vennClusterButtons")
                                        ),
                                        column(8,
                                            plotOutput(
                                                outputId="vennCluster"
                                            ) %>% withSpinner()
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
            
            # Observers for chart/cluster heatmap color selectors
            observeEvent(input$lHeatmapColorControl__Chart, {
                lHeatmapColorChart$color <- input$lHeatmapColorControl__Chart
                # Display heatmap/network panels
                output[["chartHeatmap"]] <- renderUI(
                    fluidRow(
                        column(12,
                            tabsetPanel(
                                tabPanel("Chart Heatmap", 
                                    div(
                                        column(1, 
                                            radioButtons(inputId=paste0("lHeatmapColorControl__Chart"), label="Select 0 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected=lHeatmapColorChart$color),
                                        ),
                                        column(1,
                                            radioButtons(inputId=paste0("hHeatmapColorControl__Chart"), label="Select 1 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected=hHeatmapColorChart$color)
                                        ),
                                        column(10,
                                            plot_ly(x=gctCASRNNamesChart, y=gctAnnoNamesChart, z=gctFileChartMatrix, colors=colorRamp(c(lHeatmapColorChart$color, hHeatmapColorChart$color)), type="heatmap", xgap=2, ygap=2, colorbar=list(title=list(text='<b>-log<sub>10</sub> (BH P-value)</b>'))) %>% 
                                            layout(
                                                margin=list(l=300, r=200, b=160), 
                                                xaxis=list(title="<b>Annotations</b>", tickfont=list(size=9), tickangle=15), 
                                                yaxis=list(title="<b>Input Sets</b>", type="category"),
                                                plot_bgcolor="transparent",
                                                paper_bgcolor="transparent",
                                                font=list(color=theme$textcolor)
                                            )
                                        )
                                    )
                                ), 
                                tabPanel("Chart Network", 
                                    fluidRow(
                                        column(3, 
                                            h4("Edge Selection Criteria"),
                                            numericInput(inputId="chartqval", label="q-value", value=0.05, step=0.01, max=1.00, min=0.01),
                                            checkboxGroupInput(label="Selected Input Sets", inputId="chartNetworkChoices", choices=names(enrichmentSets), selected=names(enrichmentSets)),
                                            checkboxGroupInput(label="Selected Annotation Classes", inputId="chartNetworkClasses", choices=chartClasses, selected=chartClasses),
                                            HTML("<h5><b>Other Options</b></h5>"),
                                            checkboxInput(inputId="physicsEnabledChart", label="Enable physics?", value=FALSE),
                                            checkboxInput(inputId="smoothCurveChart", label="Smooth curve for edges?", value=TRUE),
                                            actionButton(inputId="chartNetworkUpdateButton", label="Update network"),
                                            h4("More Information for Selected Annotation"),
                                            HTML("<p>Click on any <b>node</b> to view additional information for the annotation in the selected node.</p>"),
                                            h4("Overlapping Chemicals"),
                                            HTML("<p>Click on any <b>edge</b> to view a Venn diagram of the chemicals associated with the annotations in its two nodes.</p>"),
                                        ), 
                                        column(9,
                                            uiOutput("chartNetwork") %>% withSpinner()
                                        )
                                    ),
                                    hidden(
                                        fluidRow(id="nodeLinkChartMenu",
                                            column(12, 
                                                uiOutput("nodeLinkChart")
                                            )
                                        )
                                    ),
                                    hidden(
                                        fluidRow(id="vennChartMenu",
                                            column(4,
                                                uiOutput("vennChartButtons")
                                            ),
                                            column(8,
                                                plotOutput(
                                                    outputId="vennChart"
                                                ) %>% withSpinner()
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            })
            
            # Observers for chart/cluster heatmap color selectors
            observeEvent(input$hHeatmapColorControl__Cluster, {
                hHeatmapColorCluster$color <- input$hHeatmapColorControl__Cluster
                # Display heatmap/network panels
                output[["clusterHeatmap"]] <- renderUI(
                    fluidRow(
                        column(12,
                            tabsetPanel(
                                tabPanel("Cluster Heatmap", 
                                    div(
                                        column(1, 
                                            radioButtons(inputId=paste0("lHeatmapColorControl__Cluster"), label="Select 0 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected=lHeatmapColorCluster$color),
                                        ),
                                        column(1,
                                            radioButtons(inputId=paste0("hHeatmapColorControl__Cluster"), label="Select 1 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected=hHeatmapColorCluster$color)
                                        ),
                                        column(10,
                                            plot_ly(x=gctCASRNNamesCluster, y=gctAnnoNamesCluster, z=gctFileClusterMatrix, colors=colorRamp(c(lHeatmapColorCluster$color, hHeatmapColorCluster$color)), type="heatmap", xgap=2, ygap=2, colorbar=list(title=list(text='<b>-log<sub>10</sub> (BH P-value)</b>'))) %>% 
                                            layout(
                                                margin=list(l=300, r=200, b=160), 
                                                xaxis=list(title="<b>Annotation Clusters</b>", tickfont=list(size=9), tickangle=15), 
                                                yaxis=list(title="<b>Input Sets</b>", type="category"),
                                                plot_bgcolor="transparent",
                                                paper_bgcolor="transparent",
                                                font=list(color=theme$textcolor)
                                            )
                                        )
                                    )
                                ),
                                tabPanel("Cluster Network", 
                                    fluidRow(
                                        column(3, 
                                            h4("Edge Selection Criteria"),
                                            numericInput(inputId="clusterqval", label="q-value", value=0.05, step=0.01, max=1.00, min=0.01),
                                            checkboxGroupInput( label="Selected Input Sets", inputId="clusterNetworkChoices", choices=names(enrichmentSets), selected=names(enrichmentSets) ),
                                            checkboxGroupInput( label="Selected Annotation Classes", inputId="clusterNetworkClasses", choices=clusterClasses, selected=clusterClasses ),
                                            HTML("<h5><b>Other Options</b></h5>"),
                                            checkboxInput(inputId="physicsEnabledCluster", label="Enable physics?", value=FALSE),
                                            checkboxInput(inputId="smoothCurveCluster", label="Smooth curve for edges?", value=TRUE),
                                            actionButton(inputId="clusterNetworkUpdateButton", label="Update network"),
                                        ), 
                                        column(9,
                                            uiOutput("clusterNetwork") %>% withSpinner()
                                        )
                                    ),
                                    hidden(
                                        fluidRow(id="nodeLinkClusterMenu",
                                            column(12, 
                                                uiOutput("nodeLinkCluster")
                                            )
                                        )
                                    ),
                                    hidden(
                                        fluidRow(id="vennClusterMenu",
                                            column(4,
                                                uiOutput("vennClusterButtons")
                                            ),
                                            column(8,
                                                plotOutput(
                                                    outputId="vennCluster"
                                                ) %>% withSpinner()
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            })
            
            observeEvent(input$lHeatmapColorControl__Cluster, {
                lHeatmapColorCluster$color <- input$lHeatmapColorControl__Cluster
                # Display heatmap/network panels
                output[["clusterHeatmap"]] <- renderUI(
                    fluidRow(
                        column(12,
                            tabsetPanel(
                                tabPanel("Cluster Heatmap", 
                                    div(
                                        column(1, 
                                            radioButtons(inputId=paste0("lHeatmapColorControl__Cluster"), label="Select 0 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected=lHeatmapColorCluster$color),
                                        ),
                                        column(1,
                                            radioButtons(inputId=paste0("hHeatmapColorControl__Cluster"), label="Select 1 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected=hHeatmapColorCluster$color)
                                        ),
                                        column(10,
                                            plot_ly(x=gctCASRNNamesCluster, y=gctAnnoNamesCluster, z=gctFileClusterMatrix, colors=colorRamp(c(lHeatmapColorCluster$color, hHeatmapColorCluster$color)), type="heatmap", xgap=2, ygap=2, colorbar=list(title=list(text='<b>-log<sub>10</sub> (BH P-value)</b>'))) %>% 
                                            layout(
                                                margin=list(l=300, r=200, b=160), 
                                                xaxis=list(title="<b>Annotation Clusters</b>", tickfont=list(size=9), tickangle=15), 
                                                yaxis=list(title="<b>Input Sets</b>", type="category"),
                                                plot_bgcolor="transparent",
                                                paper_bgcolor="transparent",
                                                font=list(color=theme$textcolor)
                                            )
                                        )
                                    )
                                ),
                                tabPanel("Cluster Network", 
                                    fluidRow(
                                        column(3, 
                                            h4("Edge Selection Criteria"),
                                            numericInput(inputId="clusterqval", label="q-value", value=0.05, step=0.01, max=1.00, min=0.01),
                                            checkboxGroupInput( label="Selected Input Sets", inputId="clusterNetworkChoices", choices=names(enrichmentSets), selected=names(enrichmentSets) ),
                                            checkboxGroupInput( label="Selected Annotation Classes", inputId="clusterNetworkClasses", choices=clusterClasses, selected=clusterClasses ),
                                            HTML("<h5><b>Other Options</b></h5>"),
                                            checkboxInput(inputId="physicsEnabledCluster", label="Enable physics?", value=FALSE),
                                            checkboxInput(inputId="smoothCurveCluster", label="Smooth curve for edges?", value=TRUE),
                                            actionButton(inputId="clusterNetworkUpdateButton", label="Update network"),
                                        ), 
                                        column(9,
                                            uiOutput("clusterNetwork") %>% withSpinner()
                                        )
                                    ),
                                    hidden(
                                        fluidRow(id="nodeLinkClusterMenu",
                                            column(12, 
                                                uiOutput("nodeLinkCluster")
                                            )
                                        )
                                    ),
                                    hidden(
                                        fluidRow(id="vennClusterMenu",
                                            column(4,
                                                uiOutput("vennClusterButtons")
                                            ),
                                            column(8,
                                                plotOutput(
                                                    outputId="vennCluster"
                                                ) %>% withSpinner()
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            })
            
            # Observers for chart/cluster heatmap color selectors
            observeEvent(input$hHeatmapColorControl__Chart, {
              hHeatmapColorChart$color <- input$hHeatmapColorControl__Chart
              # Display heatmap/network panels
              output[["chartHeatmap"]] <- renderUI(
                fluidRow(
                  column(12,
                         tabsetPanel(
                           tabPanel("Chart Heatmap", 
                                    div(
                                      column(1, 
                                             radioButtons(inputId=paste0("lHeatmapColorControl__Chart"), label="Select 0 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected=lHeatmapColorChart$color),
                                      ),
                                      column(1,
                                             radioButtons(inputId=paste0("hHeatmapColorControl__Chart"), label="Select 1 color for heatmap:", choices=c("white", "gray", "black", "red", "blue", "yellow", "green", "brown", "purple", "orange"), selected=hHeatmapColorChart$color)
                                      ),
                                      column(10,
                                             plot_ly(x=gctCASRNNamesChart, y=gctAnnoNamesChart, z=gctFileChartMatrix, colors=colorRamp(c(lHeatmapColorChart$color, hHeatmapColorChart$color)), type="heatmap", xgap=2, ygap=2, colorbar=list(title=list(text='<b>-log<sub>10</sub> (BH P-value)</b>'))) %>% 
                                               layout(
                                                 margin=list(l=300, r=200, b=160), 
                                                 xaxis=list(title="<b>Annotations</b>", tickfont=list(size=9), tickangle=15), 
                                                 yaxis=list(title="<b>Input Sets</b>", type="category"),
                                                 plot_bgcolor="transparent",
                                                 paper_bgcolor="transparent",
                                                 font=list(color=theme$textcolor)
                                               )
                                      )
                                    )
                           ), 
                           tabPanel("Chart Network", 
                                    fluidRow(
                                      column(3, 
                                             h4("Edge Selection Criteria"),
                                             numericInput(inputId="chartqval", label="q-value", value=0.05, step=0.01, max=1.00, min=0.01),
                                             checkboxGroupInput(label="Selected Input Sets", inputId="chartNetworkChoices", choices=names(enrichmentSets), selected=names(enrichmentSets)),
                                             checkboxGroupInput(label="Selected Annotation Classes", inputId="chartNetworkClasses", choices=chartClasses, selected=chartClasses),
                                             HTML("<h5><b>Other Options</b></h5>"),
                                             checkboxInput(inputId="physicsEnabledChart", label="Enable physics?", value=FALSE),
                                             checkboxInput(inputId="smoothCurveChart", label="Smooth curve for edges?", value=TRUE),
                                             actionButton(inputId="chartNetworkUpdateButton", label="Update network"),
                                             h4("More Information for Selected Annotation"),
                                             HTML("<p>Click on any <b>node</b> to view additional information for the annotation in the selected node.</p>"),
                                             h4("Overlapping Chemicals"),
                                             HTML("<p>Click on any <b>edge</b> to view a Venn diagram of the chemicals associated with the annotations in its two nodes.</p>"),
                                      ), 
                                      column(9,
                                             uiOutput("chartNetwork") %>% withSpinner()
                                      )
                                    ),
                                    hidden(
                                      fluidRow(id="nodeLinkChartMenu",
                                               column(12, 
                                                      uiOutput("nodeLinkChart")
                                               )
                                      )
                                    ),
                                    hidden(
                                      fluidRow(id="vennChartMenu",
                                               column(4,
                                                      uiOutput("vennChartButtons")
                                               ),
                                               column(8,
                                                      plotOutput(
                                                        outputId="vennChart"
                                                      ) %>% withSpinner()
                                               )
                                      )
                                    )
                           )
                         )
                  )
                )
              )
            })
            
            # Render networks in containers
            output$chartNetwork <- renderUI(chartFullNetwork)
            output$clusterNetwork <- renderUI(clusterFullNetwork)
            # Render default venn diagram
            output[["vennChart"]] <- renderPlot({})
            output[["vennCluster"]] <- renderPlot({})
            # Render default venn diagram buttons
            output[["vennChartButtons"]] <- renderUI({})
            output[["vennClusterButtons"]] <- renderUI({})
            # Create bar graph 
            # Query API to get chart simple
            resp <- NULL
            tryBargraph <- tryCatch({
                resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getBargraph"), query=list(transactionId=transactionId, enrichmentSets=paste0(names(enrichmentSets), collapse="__")))
                TRUE
            }, error=function(cond){
                return(FALSE)
            })
            if(!tryBargraph){
                showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                return(FALSE)
            }
            if(resp$status_code != 200){
                showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                return(FALSE)
            }
            bgChartAll <- lapply(content(resp), function(chartSimpleFile){
                bgChartCategory <- unlist(lapply(chartSimpleFile, function(x) x$Category))
                bgChartTerm <- unlist(lapply(chartSimpleFile, function(x) x$Term))
                bgChartPValue <- unlist(lapply(chartSimpleFile, function(x) as.numeric(x$PValue)))
                # Create a data frame with only the columns we need (Category, Term, PValue)
                bgChart <- data.frame(Category=bgChartCategory, Term=bgChartTerm, PValue=bgChartPValue)
                # return NULL if nothing
                if(nrow(bgChart) < 1){
                    return(NULL)
                }
                # Get a list of unique categories
                uniqueCategories <- unlist(lapply(seq_len(nrow(bgChart)), function(x) bgChart[x, "Category"]))
                uniqueCategories <- unique(uniqueCategories)
                uniquePValues <- lapply(uniqueCategories, function(x){
                    individualCategory <- subset(bgChart, bgChart$Category == x) 
                    PValueAvg <- unlist(lapply(seq_len(nrow(individualCategory)), function(i){
                        if(individualCategory[i, "PValue"] < 0.05){
                            return(individualCategory[i, "PValue"])
                        } else {
                            return(0) # TODO: is this right?
                        }
                    }))
                    names(PValueAvg) <- individualCategory[, "Term"]
                    # Remove values with non-significant p-values
                    PValueAvg <- PValueAvg[vapply(PValueAvg, function(x){
                        if(x == 0) {
                            return(FALSE)
                        }
                        return(TRUE)
                    }, FUN.VALUE=logical(1))]
                    return(PValueAvg)
                })
                names(uniquePValues) <- uniqueCategories
                # Remove empty categories
                uniquePValues <- uniquePValues[vapply(uniquePValues, function(x){
                    if(length(x) > 0) {
                        return(TRUE)
                    } else {
                        return(FALSE)
                    }
                }, FUN.VALUE=logical(1))]
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
                bgChartAllInner <- bgChartAllInner[!vapply(bgChartAllInner, is.null, FUN.VALUE=logical(1))]
            })
            # Get all existing category names
            bgChartAllCategories <- unlist(lapply(bgChartAll, function(x) names(x)))
            bgChartAllCategories <- unique(bgChartAllCategories)
            bgChartFull <- lapply(bgChartAllCategories, function(catName){
                beChartFullInner <- lapply(bgChartAll, function(innerList){
                    if(catName %in% names(innerList)){
                        return(innerList[[catName]])
                    } else {
                        return(NULL)
                    }
                })
                beChartFullInner <- beChartFullInner[!vapply(beChartFullInner, is.null, FUN.VALUE=logical(1))]
                return(beChartFullInner)
            })
            names(bgChartFull) <- bgChartAllCategories
            bgChartFullReactive$bgChartFull <- bgChartFull
            bgChartAllCategoriesReactive$bgChartAllCategories <- bgChartAllCategories
            # For each unique class name, render a bar plot
            createBargraph(bgChartFull=bgChartFull, bgChartAllCategories=bgChartAllCategories, orderSet=names(enrichmentSets)[1], colorsList=colorsList)
            
            # Display warning notification if reactive structure warnings exist
            tmpWarning <- unlist(warningList$warnings, recursive=FALSE)
            if(length(tmpWarning) > 0){
                showNotification(paste0("Warning: certain chemicals have generated a reactive structure warning for the following sets: ", paste0(names(tmpWarning), collapse=", ")), type="warning", duration=10)
            }
        }

        # Select/Deselect all chemicals for reenrichment
        selectAllReenrichButtonStatus <- reactiveValues(option="deselect")
        if(is.null(setFilesObservers$observers[["selectAllReenrichButtonObserver"]])){
            setFilesObservers$observers[["selectAllReenrichButtonObserver"]] <- observeEvent(input$selectAllReenrichButton, {
                allChemNames <- unname(unlist(lapply(checkboxList$checkboxes, function(i) unlist(lapply(names(i), function(j) j)))))
                if(selectAllReenrichButtonStatus$option == "select") { # Selecting
                    firstCaseWarningChems$casrns <- list()
                    selectAllReenrichButtonStatus$option <- "deselect"
                    updateActionButton(session, "selectAllReenrichButton", label="Deselect all chemicals")
                    lapply(checkboxList$checkboxes, function(i) lapply(names(i), function(j) updateCheckboxInput(session, j, value=TRUE)))
                    # Reset set chem selection button
                    selectAllSetButtonStatus$option="deselect"
                    updateActionButton(session, "selectAllSetButton", label="Deselect all chemicals for this set")
                    # Reset warnings selection button
                    selectAllWarningsReenrichButtonStatus$option="deselect"
                    updateActionButton(session, "selectAllWarningsReenrichButton", label="<div class=\"text-danger\">Deselect all chemicals with warnings</div>")
                    
                } else { # Deselecting
                    firstCaseWarningChems$casrns <- allChemNames # If deselecting, set this first case list to contain all chemicals
                    selectAllReenrichButtonStatus$option <- "select"
                    updateActionButton(session, "selectAllReenrichButton", label="Select all chemicals")
                    lapply (checkboxList$checkboxes, function(i) lapply(names(i), function(j) updateCheckboxInput(session, j, value=FALSE)))
                    
                    # Reset set chem selection button
                    selectAllSetButtonStatus$option="select"
                    updateActionButton(session, "selectAllSetButton", label="Select all chemicals for this set")
                    # Reset warnings selection button
                    selectAllWarningsReenrichButtonStatus$option="select"
                    updateActionButton(session, "selectAllWarningsReenrichButton", label="<div class=\"text-danger\">Select all chemicals with warnings</div>")
                    
                }
            }, ignoreInit=TRUE, ignoreNULL=TRUE)
        }
        
        # Select/Deselect all chemicals for a single set
        selectAllSetButtonStatus <- reactiveValues(option="deselect")
        if(is.null(setFilesObservers$observers[["selectAllSetButtonObserver"]])){
            setFilesObservers$observers[["selectAllSetButtonObserver"]] <- observeEvent(input$selectAllSetButton, {
                if(selectAllSetButtonStatus$option == "select") { # Selecting
                    selectAllSetButtonStatus$option="deselect"
                    updateActionButton(session, "selectAllSetButton", label="Deselect all chemicals for this set")
                    lapply (checkboxList$checkboxes, function(i) {
                        lapply(names(i), function(j){
                            if(unlist(str_split(j, "__"))[2] == input$tab){
                                updateCheckboxInput(session, j, value=TRUE)
                            }
                        })
                    })
                } else { # Deselecting
                    selectAllSetButtonStatus$option="select"
                    updateActionButton(session, "selectAllSetButton", label="Select all chemicals for this set")
                    lapply (checkboxList$checkboxes, function(i) {
                        lapply(names(i), function(j){
                            if(unlist(str_split(j, "__"))[2] == input$tab){
                                updateCheckboxInput(session, j, value=FALSE)
                            }
                        })
                    })
                }
            }, ignoreInit=TRUE, ignoreNULL=TRUE)
        }
        
        # Select/Deselect all chemicals with warnings for reenrichment
        selectAllWarningsReenrichButtonStatus <- reactiveValues(option="deselect")
        if(is.null(setFilesObservers$observers[["selectAllWarningsReenrichButtonObserver"]])){
            setFilesObservers$observers[["selectAllWarningsReenrichButtonObserver"]] <- observeEvent(input$selectAllWarningsReenrichButton, {
                # Get list of all the chemicals with warnings per set
                chemicalsWithWarnings <- lapply(finalTableToDisplay$table, function(dataset){
                    chemicalsWithWarningsInner <- lapply(seq_len(nrow(dataset)), function(line){
                        if(is.null(dataset[line, "warning"]) == FALSE){
                            if(dataset[line, "warning"] != "<p>None</p>") {
                                return(paste0(dataset[line, "CASRN"]))
                            }
                        }
                    })
                    return(unlist(chemicalsWithWarningsInner))
                })
                chemicalsWithWarningsList <- unname(unlist(lapply(names(chemicalsWithWarnings), function(dataset){
                    chemicalsWithWarningsListInner <- unlist(lapply(chemicalsWithWarnings[dataset], function(casrn) {
                        return(paste0(casrn, "__", dataset))
                    }))
                })))

                if(selectAllWarningsReenrichButtonStatus$option == "select") { # Selecting
                    firstCaseWarningChems$casrns <- list() # If selecting, remove warning chems from this list
                    selectAllWarningsReenrichButtonStatus$option <- "deselect"
                    updateActionButton(session, "selectAllWarningsReenrichButton", label="<div class=\"text-danger\">Deselect all chemicals with warnings</div>")
                    lapply (checkboxList$checkboxes, function(i) {
                        lapply(names(i), function(j){
                            if(j %in% chemicalsWithWarningsList){
                                updateCheckboxInput(session, j, value=TRUE)  
                            }
                        })
                    })
                } else { # Deselecting
                    firstCaseWarningChems$casrns <- chemicalsWithWarningsList # If deselecting, add warning chems to this list
                    selectAllWarningsReenrichButtonStatus$option <- "select"
                    updateActionButton(session, "selectAllWarningsReenrichButton", label="<div class=\"text-danger\">Select all chemicals with warnings</div>")
                    lapply (checkboxList$checkboxes, function(i) {
                        lapply(names(i), function(j){
                            if(j %in% chemicalsWithWarningsList){
                                updateCheckboxInput(session, j, value=FALSE)  
                            }
                        })
                    })
                }
            }, ignoreInit=TRUE, ignoreNULL=TRUE)
        }
        
        # Observer to select/deselect checkboxes on boot (workaround for issue #19 https://github.com/hurlab/tox21enricher/issues/19)
        observeEvent(input$tab, {
            chemicalsWithWarnings <- lapply(finalTableToDisplay$table, function(dataset){
                chemicalsWithWarningsInner <- lapply(seq_len(nrow(dataset)), function(line){
                    if(!is.null(dataset[line, "warning"])){
                        if(dataset[line, "warning"] != "None") {
                            return(paste0(dataset[line, "CASRN"]))
                        }
                    }
                })
                return(unlist(chemicalsWithWarningsInner))
            })
            chemicalsWithWarningsList <- unname(unlist(lapply(names(chemicalsWithWarnings), function(dataset){
                chemicalsWithWarningsListInner <- unlist(lapply(chemicalsWithWarnings[dataset], function(casrn) {
                    return(paste0(casrn, "__", dataset))
                }))
            })))
          
            # Check if we deselected all chems
            if(selectAllReenrichButtonStatus$option == "select") {
                lapply (checkboxList$checkboxes, function(i) {
                    lapply (names(i), function(j){
                        updateCheckboxInput(session, j, value=FALSE)
                    })
                })
            }
          
            # Check if we deselected all chems with warnings
            if(selectAllWarningsReenrichButtonStatus$option == "select") {
                chemicalsWithWarnings <- lapply(finalTableToDisplay$table, function(dataset){
                    chemicalsWithWarningsInner <- lapply(seq_len(nrow(dataset)), function(line){
                        if(!is.null(dataset[line, "warning"])){
                            if(dataset[line, "warning"] != "<p>None</p>") {
                                return(paste0(dataset[line, "CASRN"]))
                            }
                        }
                    })
                    return(unlist(chemicalsWithWarningsInner))
                })
                chemicalsWithWarningsList <- unname(unlist(lapply(names(chemicalsWithWarnings), function(dataset){
                    chemicalsWithWarningsListInner <- unlist(lapply(chemicalsWithWarnings[dataset], function(casrn) {
                        return(paste0(casrn, "__", dataset))
                    }))
                })))
                lapply (checkboxList$checkboxes, function(i) {
                    lapply(names(i), function(j){
                        if(j %in% chemicalsWithWarningsList){
                            updateCheckboxInput(session, j, value=FALSE)  
                        }
                    })
                })
            }
        })
        # Re-enable refresh button
        shinyjs::enable(id="refresh")
        # Re-enable results page
        shinyjs::enable(id="enrichmentResults")
        # Enable settings button
        shinyjs::enable(id="settingsButton")
    }
    
    # Re-order bar graphs with respect to given input set
    observeEvent(input$updateBargraphButton, {
        createBargraph(bgChartFull=bgChartFullReactive$bgChartFull, bgChartAllCategories=bgChartAllCategoriesReactive$bgChartAllCategories, orderSet=input$radioBargraph, colorsList=setColors$color)
    })
    
    # Shared code to create networks
    generateNetwork <- function(transactionId, cutoff, networkMode, inputNetwork, qval, physicsEnabled=FALSE, smoothCurve=TRUE, keep=list()){
        inputNetwork <- paste0(inputNetwork, collapse="#")
        # Error handling
        resp <- NULL
        tryNetwork <- tryCatch({
            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getNetwork"), query=list(transactionId=transactionId, cutoff=cutoff, mode=networkMode, input=inputNetwork, qval=qval))
            TRUE
        }, error=function(cond){
            return(FALSE)
        })
        if(!tryNetwork){
            showNotification("Error: An error occurred while generating the network.", type="error")
            return(FALSE)
        }
        if(resp$status_code != 200 | is.null(content(resp))){
            showNotification("Error: An error occurred while generating the network.", type="error")
            return(HTML("<p class=\"text-danger\"><b>Error:</b> An error occurred while generating the network.</p>"))
        }
        if(length(content(resp)) == 0){
            showNotification("Error: An error occurred while generating the network.", type="error")
            return(HTML("<p class=\"text-danger\"><b>Error:</b> The generated network has zero nodes.</p>"))
        }
        outpNetwork <- t(vapply(content(resp), function(x){
            return(list(
                "pairwiseid"=x[["pairwiseid"]],
                "term1uid"=x[["term1uid"]],
                "term2uid"=x[["term2uid"]],
                "term1size"=x[["term1size"]],
                "term2size"=x[["term2size"]],
                "common"=x[["common"]],
                "union"=x[["union"]],
                "jaccardindex"=x[["jaccardindex"]],
                "pvalue"=x[["pvalue"]],
                "qvalue"=x[["qvalue"]],
                "name1"=x[["name1"]],
                "name2"=x[["name2"]],
                "class1"=x[["class1"]],
                "class2"=x[["class2"]],
                "url1"=x[["url1"]],
                "url2"=x[["url2"]]
            ))
        }, FUN.VALUE=list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)))
        outpNetwork <- as.data.frame(outpNetwork)
        rownames(outpNetwork) <- seq_len(nrow(outpNetwork))
        # Define class colors
        classColors <- generateAnnoClassColors()
        
        # Create rows and classes
        rowsSet1 <- lapply(seq_len(nrow(outpNetwork)), function(x) paste0(outpNetwork[[x, "name1"]], "_@_", outpNetwork[[x, "class1"]], "_@_", outpNetwork[[x, "url1"]], "_@_", classToColor(outpNetwork[[x, "class1"]], classColors)))
        rowsSet2 <- lapply(seq_len(nrow(outpNetwork)), function(x) paste0(outpNetwork[[x, "name2"]], "_@_", outpNetwork[[x, "class2"]], "_@_", outpNetwork[[x, "url2"]], "_@_", classToColor(outpNetwork[[x, "class2"]], classColors)))
        rowsSet <- list(unlist(rowsSet1), unlist(rowsSet2))
        rowsSet <- unique(unlist(rowsSet, recursive=FALSE))
        classes1 <- lapply(seq_len(nrow(outpNetwork)), function(x) paste0(outpNetwork[[x, "class1"]]))
        classes2 <- lapply(seq_len(nrow(outpNetwork)), function(x) paste0(outpNetwork[[x, "class2"]]))
        classes <- list(unlist(classes1), unlist(classes2))
        classes <- unique(unlist(classes, recursive=FALSE))
        # Generate list of nodes for network
        networkFullNodes <- t(vapply(rowsSet, function(x){
            rowsSetSplit <- unlist(str_split(x, "_@_"))
            # split: 1=Term, 2=Class (Category), 3=URL, 4=Color 
            id <- paste0(rowsSetSplit[1], "_@_", rowsSetSplit[2], "_@_", networkMode)
            url <- paste0(rowsSetSplit[3], rowsSetSplit[1])
            rgbCss <- rowsSetSplit[4]
            # Put blank row if the class is not in the "keep" list
            if(!(rowsSetSplit[2] %in% keep)) {
                nodeList <- c(id="", label="", group="", shape="", url="", color="")
                return(nodeList)
            }
            nodeList <- c(id=id, label=rowsSetSplit[1], group=rowsSetSplit[2], shape="ellipse", url=url, color=rgbCss)
            return(nodeList)
        }, FUN.VALUE=character(6))) # 6 because of the number of columns: id, label, group, shape, url, color
        networkFullNodes <- data.frame(matrix(unlist(networkFullNodes), nrow=nrow(networkFullNodes)), stringsAsFactors=FALSE)
        rownames(networkFullNodes) <- seq_len(nrow(networkFullNodes))
        colnames(networkFullNodes) <- list("id", "label", "group", "shape", "url", "color")
        
        # Remove duplicates
        networkFullNodes <- networkFullNodes[!duplicated(networkFullNodes), ]
        # Remove nodes if their class is not in the "keep" list
        networkFullNodes <- networkFullNodes[complete.cases(networkFullNodes), ]
        # Generate list of edges for network
        networkFullEdges <- t(vapply(seq_len(nrow(outpNetwork)), function(p){
            if( (outpNetwork[[p, "class1"]] %in% keep) & (outpNetwork[[p, "class2"]] %in% keep) ){
                rgbCss=generateJaccardColor(as.numeric(outpNetwork[[p, "jaccardindex"]]))
                edgeUUID <- paste0(UUIDgenerate(), "__", paste0(outpNetwork[[p, "name1"]], "_@_", outpNetwork[[p, "class1"]]), "_@_", networkMode, "__", paste0(outpNetwork[[p, "name2"]], "_@_", outpNetwork[[p, "class2"]]), "_@_", networkMode, "__", networkMode)
                edgeList <- c(from=paste0(outpNetwork[[p, "name1"]], "_@_", outpNetwork[[p, "class1"]], "_@_", networkMode), to=paste0(outpNetwork[[p, "name2"]], "_@_", outpNetwork[[p, "class2"]], "_@_", networkMode), jaccard=outpNetwork[[p, "jaccardindex"]], color=rgbCss, id=edgeUUID)
                return(edgeList)
            }
            # If one node has a class not in the "keep" list, discard it
            edgeList <- c(from="", to="", jaccard="", color="", id="")
            return(edgeList)
        }, FUN.VALUE=character(5))) # 5 because of the number of columns: from, to, jaccard, color, id
        networkFullEdges <- data.frame(matrix(unlist(networkFullEdges), nrow=nrow(networkFullEdges)), stringsAsFactors=FALSE)
        rownames(networkFullEdges) <- seq_len(nrow(networkFullEdges))
        colnames(networkFullEdges) <- list("from", "to", "jaccard", "color", "id")
        # Remove edges if either node's class is not in the "keep" list
        networkFullEdges <- networkFullEdges[complete.cases(networkFullEdges), ]
        fullNetwork <- visNetwork(networkFullNodes, networkFullEdges, height="500px", width="100%") %>%
            visOptions(highlightNearest=TRUE, nodesIdSelection=TRUE, selectedBy=list(variable="group", multiple=TRUE)) %>%
            visLayout(randomSeed=runif(1)) %>%
            visPhysics(solver="forceAtlas2Based", enabled=physicsEnabled, stabilization=list(enabled=FALSE, iterations=1000, updateInterval=25)) %>%
            visEdges(smooth=smoothCurve) %>%
            visInteraction(navigationButtons=TRUE, keyboard=FALSE, selectable=TRUE, selectConnectedEdges=FALSE) %>%
            # Venn Diagram handler when clicking on network edges
            visEvents(
                selectEdge='
                    function(properties) {Shiny.setInputValue("selectEdge", properties, {priority:"event"});}',
                selectNode='
                    function(properties) {Shiny.setInputValue("selectNode", properties, {priority:"event"});}'
            )
        # Add groups for legend (have to use for, can't use *apply)
        for (x in classes) {
            groupColor <- paste0("rgb(", paste0(classColors[[x]], collapse=", "), ")")
            fullNetwork <- fullNetwork %>% visGroups(groupname=x, color=groupColor)
        }
        fullNetwork <- fullNetwork %>% visLegend()
        fullNetworkExport <- visExport(
            graph=fullNetwork,
            type="png"
        )
        return(fluidRow(id=paste0(networkMode, "NetworkContainer"), fullNetworkExport))
    }
    
    # Observe network node being clicked and display links to chemical details
    observeEvent(input$selectNode, {
        selectedNodeTerm <- unlist(str_split(input$selectNode$nodes[[1]], "_@_"))[1]
        selectedNodeClass <- unlist(str_split(input$selectNode$nodes[[1]], "_@_"))[2]
        networkMode <- unlist(str_split(input$selectNode$nodes[[1]], "_@_"))[3]
      
        # Get link for node annotation details
        resp <- NULL
        tryNode <- tryCatch({
            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getNodeDetails"), query=list(class=selectedNodeClass))
            TRUE
        }, error=function(cond){
            return(FALSE)
        })
        if(!tryNode){
            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
            return(FALSE)
        }
        
        if(resp$status_code != 200){
            if(networkMode == "chart") {
                output[["nodeLinkChart"]] <- renderUI({
                    HTML(paste0("<p>Could not fetch link for ", selectedNodeTerm, ".</p>"))
                }) 
            } else {
                output[["nodeLinkCluster"]] <- renderPlot({
                    HTML(paste0("<p>Could not fetch link for ", selectedNodeTerm, ".</p>"))
                }) 
            }
        } else {
            baseurl <- unname(unlist(content(resp)))
            # If missing baseurl, don't provide link
            if(networkMode == "chart") {
                shinyjs::show(id="nodeLinkChartMenu")
                shinyjs::hide(id="vennChartMenu")
                if(is.null(baseurl) | baseurl == "placeholder") {
                    output[["nodeLinkChart"]] <- renderUI({
                        HTML(paste0("<p>Could not fetch link for ", selectedNodeTerm, ".</p>"))
                    })  
                } else { # these classes have less-specific urls that need to be handled differently
                    if(!selectedNodeClass %in% list("CTD_CHEMICALS_DISEASES", "CTD_CHEMICALS_GENES", "CTD_CHEMICALS_GOENRICH_CELLCOMP", "CTD_CHEMICALS_GOENRICH_MOLFUNCT", "CTD_CHEMICALS_PATHWAYS", "CTD_GOFAT_BIOPROCESS", "CTD_GOSLIM_BIOPROCESS", "DRUGBANK_ATC", "DRUGBANK_ATC_CODE", "DRUGBANK_CARRIERS", "DRUGBANK_ENZYMES", "DRUGBANK_TARGETS", "DRUGBANK_TRANSPORTERS", "TOXINS_TARGETS", "MESH")){
                        output[["nodeLinkChart"]] <- renderUI({
                            HTML(paste0("<p><i>Click <b><a href='", baseurl, "'>here</a></b> to view more details about ", selectedNodeTerm, ".</i></p>"))
                        })
                    } else {
                        output[["nodeLinkChart"]] <- renderUI({
                            HTML(paste0("<p><i>Click <b><a href='", baseurl, selectedNodeTerm, "'>here</a></b> to view more details about ", selectedNodeTerm, ".</i></p>"))
                        })
                    }
                }
            } else {
                shinyjs::show(id="nodeLinkClusterMenu")
                shinyjs::hide(id="vennClusterMenu")
                if(is.null(baseurl) | baseurl == "placeholder") {
                    output[["nodeLinkCluster"]] <- renderUI({
                        HTML(paste0("<p>Could not fetch link for ", selectedNodeTerm, ".</p>"))
                    })  
                } else {
                    if(!selectedNodeClass %in% list("CTD_CHEMICALS_DISEASES", "CTD_CHEMICALS_GENES", "CTD_CHEMICALS_GOENRICH_CELLCOMP", "CTD_CHEMICALS_GOENRICH_MOLFUNCT", "CTD_CHEMICALS_PATHWAYS", "CTD_GOFAT_BIOPROCESS", "CTD_GOSLIM_BIOPROCESS", "DRUGBANK_ATC", "DRUGBANK_ATC_CODE", "DRUGBANK_CARRIERS", "DRUGBANK_ENZYMES", "DRUGBANK_TARGETS", "DRUGBANK_TRANSPORTERS", "TOXINS_TARGETS", "MESH")){
                        output[["nodeLinkCluster"]] <- renderUI({
                            HTML(paste0("<p><i>Click <b><a href='", baseurl, "'>here</a></b> to view more details about ", selectedNodeTerm, ".</i></p>"))
                        })
                    } else {
                        output[["nodeLinkCluster"]] <- renderUI({
                            HTML(paste0("<p><i>Click <b><a href='", baseurl, selectedNodeTerm, "'>here</a></b> to view more details about ", selectedNodeTerm, ".</i></p>"))
                        })
                    }
                }
            }
        }
    })
    
    # Observe network edge being clicked and display venn diagram and options for shared annotations
    observeEvent(input$selectEdge, {
        if(length(input$selectEdge$edges) == 1 & length(input$selectEdge$nodes) == 0) { #if selecting edge
            # Get from node (2), to node (3), and chart - chart or cluster (4)
            tmpSplit <- unlist(str_split(input$selectEdge$edges, "__"))
            tmpFrom <- unlist(str_split(tmpSplit[2], "_@_"))
            tmpTo <- unlist(str_split(tmpSplit[3], "_@_"))
            networkMode <- tmpSplit[4]
            termFrom <- tmpFrom[1]
            classFrom <- tmpFrom[2]
            termTo <- tmpTo[1]
            classTo <- tmpTo[2]
    
            # Get overlapping chemicals
            resp <- NULL
            tryNode <- tryCatch({
                resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getNodeChemicals"), query=list( termFrom=termFrom, termTo=termTo, classFrom=classFrom, classTo=classTo ))
                TRUE
            }, error=function(cond){
                return(FALSE)
            })
            if(!tryNode){
                showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
                return(FALSE)
            }
            
            if(resp$status_code != 200){
                output[["vennChart"]] <- renderPlot({
                    HTML("<p class=\"text-danger\"><b>Error:</b> An error occurred while fetching the chemicals.</p>")
                })
                output[["vennCluster"]] <- renderPlot({
                    HTML("<p class=\"text-danger\"><b>Error:</b> An error occurred while fetching the chemicals.</p>")
                })
            }
            casrnsFrom <- unlist(unname(unlist(content(resp)["casrnsFrom"], recursive=FALSE)))
            casrnsTo <- unlist(unname(unlist(content(resp)["casrnsTo"], recursive=FALSE)))
            casrnsShared <- intersect(casrnsFrom, casrnsTo)
          
            if(length(casrnsShared) < 1){
                casrnsShared <- "No shared chemicals."
            }
          
            # Set venn diagram color to match theme settings
            vennColor <- "black"
            vennColorBG <- "#FFFFFF"
            if(theme$textcolor == "#FFFFFF"){ # Dark theme
                vennColor <- "white"
                vennColorBG <- "#333333"
            } else { # Light theme
                vennColor <- "black"
                vennColorBG <- "#FFFFFF"
            }
            
            # Get annotation colors
            classColors <- generateAnnoClassColors()
            classColorsFrom <- classColors[[classFrom]]
            classColorsTo <- classColors[[classTo]]
          
            # Color from class of FROM annotation
            colorFrom <- rgb(classColorsFrom[1], classColorsFrom[2], classColorsFrom[3], maxColorValue=255)
            # Color from class of TO annotation
            colorTo <- rgb(classColorsTo[1], classColorsTo[2], classColorsTo[3], maxColorValue=255)
            # Average color from classes of FROM and TO annotations
            darkenValue <- 0
            if(colorFrom == colorTo){
                darkenValue <- 50 # If both region colors are identical, just darken it a bit
            }
            greenAvg <- as.integer(((strtoi(classColorsFrom[1]) + strtoi(classColorsTo[1]))/2)-darkenValue)
            redAvg <- as.integer(((strtoi(classColorsFrom[2]) + strtoi(classColorsTo[2]))/2)-darkenValue)
            blueAvg <- as.integer(((strtoi(classColorsFrom[3]) + strtoi(classColorsTo[3]))/2)-darkenValue)
            if(greenAvg < 0) {
                greenAvg <- 0
            }
            if(redAvg < 0) {
                redAvg <- 0
            }
            if(blueAvg < 0) {
                blueAvg <- 0
            }
            colorBoth <- rgb(greenAvg, redAvg, blueAvg, maxColorValue=255)
          
            # Prepare data and create Venn diagram
            vennList <- list(casrnsFrom, casrnsTo)
            names(vennList) <- list(str_wrap(paste0(classFrom, " | ", termFrom), 30) , str_wrap(paste0(classTo, " | ", termTo), 30)) 
            venn <- Venn(vennList)
            vennData <- process_data(venn)
            vennDiagramPlot <- ggplot() +
                geom_sf(aes(fill=list(colorFrom, colorTo, colorBoth)), data=venn_region(vennData)) +
                geom_sf(size=1, lty="solid", color=theme$textcolor, data=venn_setedge(vennData), show.legend=FALSE) +
                geom_sf_text(aes(label=name), fontface="bold", color=theme$textcolor, nudge_y=c(1, 1, 1), data=venn_setlabel(vennData)) +
                geom_sf_label(size=18, aes(label=count), fontface="bold", data=venn_region(vennData)) +
                expand_limits(x=c(-4, 4), y=c(-4, 6)) + # expand y-axis of graph to make room for top set labels
                theme_void() +
                theme(
                    legend.position="none",
                    panel.background=element_rect(
                      fill=vennColorBG,
                      color=NA,
                      size=0,
                      linetype="solid"
                    )
                ) # remove legend, not really needed
            vennDiagram <- ggplotGrob(vennDiagramPlot)
            vennDiagram$respect <- FALSE
          
            if(networkMode == "chart"){
                # Render Venn diagram
                shinyjs::show("vennChartMenu")
                shinyjs::show("vennChart")
                shinyjs::hide("nodeLinkChartMenu")
                output[["vennChart"]] <- renderPlot(
                    plot(vennDiagram)
                )
              
                # Render buttons for Venn diagram
                shinyjs::show("vennChartButtons")
                output[["vennChartButtons"]] <- renderUI({
                    fluidRow(
                        column(12,
                            actionButton(inputId="vennFromButtonChart", label=HTML(paste0("View chemicals for<br/>", classFrom, " |<br/>", termFrom)) )      
                        ),
                        column(12,
                            actionButton(inputId="vennToButtonChart", label=HTML(paste0("View chemicals for<br/>", classTo, " |<br/>", termTo)) )
                        ),
                        column(12,
                            actionButton(inputId="vennSharedButtonChart", label="View shared chemicals")
                        ),
                        column(12,
                            downloadLink("vennChartDownloadImg", "Download plot as .png")
                        ),
                        column(12,
                            downloadLink("vennChartDownloadPdf", "Download plot as .pdf")
                        )
                    )
                })
                # Create observers for Venn Diagram buttons
                observeEvent(input$vennFromButtonChart, {
                    # disable refreshbutton
                    shinyjs::disable(id="refresh")
                    showModal(
                        modalDialog(
                            title=paste0("Chemicals for ", termFrom),
                            footer=div(rclipButton("clipboardChartFrom", HTML(paste0(icon("clipboard"), " Copy chemical list to clipboard (comma-separated)")), paste0(casrnsFrom, collapse=","), modal=TRUE), actionButton(inputId="vennFromButtonCloseChart", label="Close")),
                            size="l",
                            fluidRow(
                                column(12, 
                                    HTML(paste0(casrnsFrom, collapse="<br/>"))
                                )
                            )
                        )
                    )
                }, ignoreNULL=TRUE, ignoreInit=TRUE)
                observeEvent(input$vennFromButtonCloseChart, {
                    # enable refreshbutton
                    shinyjs::enable(id="refresh")
                    removeModal()
                })
                observeEvent(input$vennToButtonChart, {
                    # disable refreshbutton
                    shinyjs::disable(id="refresh")
                    showModal(
                        modalDialog(
                            title=paste0("Chemicals for ", termTo),
                            footer=div(rclipButton("clipboardChartTo", HTML(paste0(icon("clipboard"), " Copy chemical list to clipboard (comma-separated)")), paste0(casrnsTo, collapse=","), modal=TRUE), actionButton(inputId="vennToButtonCloseChart", label="Close")),
                            size="l",
                            fluidRow(
                                column(12, 
                                    HTML(paste0(casrnsTo, collapse="<br/>"))
                                )
                            )
                        )
                    )
                }, ignoreNULL=TRUE, ignoreInit=TRUE)
                observeEvent(input$vennToButtonCloseChart, {
                    # enable refreshbutton
                    shinyjs::enable(id="refresh")
                    removeModal()
                })
                observeEvent(input$vennSharedButtonChart, {
                    # disable refreshbutton
                    shinyjs::disable(id="refresh")
                    showModal(
                        modalDialog(
                            title=paste0("Shared chemicals"),
                            footer=div(rclipButton("clipboardChartShared", HTML(paste0(icon("clipboard"), " Copy chemical list to clipboard (comma-separated)")), paste0(casrnsShared, collapse=","), modal=TRUE), actionButton(inputId="vennSharedButtonCloseChart", label="Close")),
                            size="l",
                            fluidRow(
                                column(12, 
                                    HTML(paste0(casrnsShared, collapse="<br/>"))
                                )
                            )
                        )
                    )
                }, ignoreNULL=TRUE, ignoreInit=TRUE)
                observeEvent(input$vennSharedButtonCloseChart, {
                    # enable refreshbutton
                    shinyjs::enable(id="refresh")
                    removeModal()
                })
                output$vennChartDownloadImg <- downloadHandler(
                    filename=paste0("venndiagram.png"),
                    content=function(file){
                        ggsave(file, plot=vennDiagramPlot)
                    }
                )
                output$vennChartDownloadPdf <- downloadHandler(
                    filename=paste0("venndiagram.pdf"),
                    content=function(file){
                        ggsave(file, plot=vennDiagramPlot)
                    }
                )
            } else { # Cluster
                # Render Venn diagram
                shinyjs::show("vennClusterMenu")
                shinyjs::show("vennCluster")
                shinyjs::hide("nodeLinkClusterMenu")
                output[["vennCluster"]] <- renderPlot(
                    plot(vennDiagram)
                )
                
                # Render buttons for venn diagram
                shinyjs::show("vennClusterButtons")
                output[["vennClusterButtons"]] <- renderUI({
                    fluidRow(
                        column(12,
                            actionButton(inputId="vennFromButtonCluster", label=HTML(paste0("View chemicals for<br/>", classFrom, " |<br/>", termFrom)) )      
                        ),
                        column(12,
                            actionButton(inputId="vennToButtonCluster", label=HTML(paste0("View chemicals for<br/>", classTo, " |<br/>", termTo)) )
                        ),
                        column(12,
                            actionButton(inputId="vennSharedButtonCluster", label="View shared chemicals")
                        ),
                        column(12,
                            downloadLink("vennClusterDownloadImg", "Download plot as .png")
                        ),
                        column(12,
                             downloadLink("vennClusterDownloadPdf", "Download plot as .pdf")
                        )
                    )
                })
                # Create observers for Venn Diagram buttons
                observeEvent(input$vennFromButtonCluster, {
                    # disable refreshbutton
                    shinyjs::disable(id="refresh")
                    showModal(
                        modalDialog(
                            title=paste0("Chemicals for ", termFrom),
                            footer=div(rclipButton("clipboardClusterFrom", HTML(paste0(icon("clipboard"), " Copy chemical list to clipboard (comma-separated)")), paste0(casrnsFrom, collapse=","), modal=TRUE), actionButton(inputId="vennFromButtonCloseCluster", label="Close")),
                            size="l",
                            fluidRow(
                                column(12, 
                                    HTML(paste0(casrnsFrom, collapse="<br/>"))
                                )
                            )
                        )
                    )
                }, ignoreNULL=TRUE, ignoreInit=TRUE)
                observeEvent(input$vennFromButtonCloseCluster, {
                    # enable refreshbutton
                    shinyjs::enable(id="refresh")
                    removeModal()
                })
                observeEvent(input$vennToButtonCluster, {
                    # disable refreshbutton
                    shinyjs::disable(id="refresh")
                    showModal(
                        modalDialog(
                            title=paste0("Chemicals for ", termTo),
                            footer=div(rclipButton("clipboardClusterTo", HTML(paste0(icon("clipboard"), " Copy chemical list to clipboard (comma-separated)")), paste0(casrnsTo, collapse=","), modal=TRUE), actionButton(inputId="vennToButtonCloseCluster", label="Close")),
                            size="l",
                            fluidRow(
                                column(12, 
                                    HTML(paste0(casrnsTo, collapse="<br/>"))
                                )
                            )
                        )
                    )
                }, ignoreNULL=TRUE, ignoreInit=TRUE)
                observeEvent(input$vennToButtonCloseCluster, {
                    # enable refreshbutton
                    shinyjs::enable(id="refresh")
                    removeModal()
                })
                observeEvent(input$vennSharedButtonCluster, {
                    # disable refreshbutton
                    shinyjs::disable(id="refresh")
                    showModal(
                        modalDialog(
                            title=paste0("Shared chemicals"),
                            footer=div(rclipButton("clipboardClusterShared", HTML(paste0(icon("clipboard"), " Copy chemical list to clipboard (comma-separated)")), paste0(casrnsShared, collapse=","), modal=TRUE), actionButton(inputId="vennSharedButtonCloseCluster", label="Close")),
                            size="l",
                            fluidRow(
                                column(12, 
                                    HTML(paste0(casrnsShared, collapse="<br/>"))
                                )
                            )
                        )
                    )
                }, ignoreNULL=TRUE, ignoreInit=TRUE)
                observeEvent(input$vennSharedButtonCloseCluster, {
                    # enable refreshbutton
                    shinyjs::enable(id="refresh")
                    removeModal()
                })
                output$vennClusterDownloadImg <- downloadHandler(
                    filename=paste0("venndiagram.png"),
                    content=function(file){
                        ggsave(file, plot=vennDiagramPlot)
                    }
                )
                output$vennClusterDownloadPdf <- downloadHandler(
                    filename=paste0("venndiagram.pdf"),
                    content=function(file){
                        ggsave(file, plot=vennDiagramPlot)
                    }
                )
            }
        } else { # selecting a node
        }
    }, ignoreNULL=FALSE, ignoreInit=TRUE )
    
    # Re-generate chart network
    observeEvent(input$chartNetworkUpdateButton, {
        # Hide old network
        output$chartNetwork <- renderUI(div())
        #TODO: error handle no results
        if(length(input$chartNetworkChoices) == 0){
            output$chartNetwork <- renderUI(HTML("<p class=\"text-danger\"><b>Error:</b> No input sets selected.</p>"))
        } else {
            chartFullNetwork <- generateNetwork(transactionId=reactiveTransactionId$id, cutoff=input$nodeCutoffRe, networkMode="chart", inputNetwork=input$chartNetworkChoices, qval=input$chartqval, physicsEnabled=input$physicsEnabledChart, smoothCurve=input$smoothCurveChart, keep=input$chartNetworkClasses)
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
            clusterFullNetwork <- generateNetwork(transactionId=reactiveTransactionId$id, cutoff=input$nodeCutoffRe, networkMode="cluster", inputNetwork=input$clusterNetworkChoices, qval=input$clusterqval, physicsEnabled=input$physicsEnabledCluster, smoothCurve=input$smoothCurveCluster, keep=input$clusterNetworkClasses)
            # Render networks
            output$clusterNetwork <- renderUI(clusterFullNetwork)
        }
    })
    
    # Update network
    observeEvent(input$updateNetworkButton, {
        updateNetworkBox <- paste0(lapply(seq_len(length(enrichmentSetsList$enrichmentSets)), function(i){
            paste0("#", names(enrichmentSetsList$enrichmentSets)[i], "\n", paste0(enrichmentSetsList$enrichmentSets[[i]], collapse="\n"))
        }), collapse="\n")
        
        # Reset reenrichResultsList$reenrichResults and enrichmentSetsList$enrichmentSets
        reenrichResultsList$reenrichResults <- NULL
        enrichmentSetsList$enrichmentSets <- NULL
        
        checkboxList$checkboxes <- NULL
        firstCaseWarningChems$casrns <- NULL
        
        # Reset setFilesObservers$observers
        lapply(setFilesObservers$observers, function(x) x$destroy())
        setFilesObservers$observers <- NULL
        if(!performEnrichment(updateNetworkBox, reenrichFlag=TRUE)){
            changePage(page="enrichment")
        }
    })
    
    # Get colors for corresponding network nodes
    classToColor <- function(annoClass, classColors) {
        if (!is.null(classColors[[annoClass]])) {
            return(paste0("rgb(", paste0(classColors[[annoClass]], collapse=", "), ")"))
        } else {
            return(NULL)
        }
    }
    
    # Generate specific colors for annotation classes in network
    generateAnnoClassColors <- function() {
        resp <- NULL
        tryColor <- tryCatch({
            resp <- GET(url=paste0(API_PROTOCOL, API_ADDR, "/getNodeColors"))
            TRUE
        }, error=function(cond){
            return(FALSE)
        })
        if(!tryColor){
            showNotification("Error: The application cannot connect to the Tox21 Enricher server. Please try again later.", type="error")
            return(FALSE)
        }
        
        if(resp$status_code != 200){
            return(list())
        } else {
            tmp <- do.call(rbind, content(resp))
            colorsList <- lapply(tmp[, "networkcolor"], function(x){
                tmpSplit <- unlist(str_split(x, ","))
                tmpSplit <- unlist(lapply(tmpSplit, function(y){
                    return(as.numeric(y)) # convert strings to ints (i.e., "12" to 12)
                }))
                return(tmpSplit)
            })
            names(colorsList) <- tmp[, "annoclassname"]
            return(colorsList)
        }
    }
    
    # Generate specific colors for network edges based on jaccard index
    generateJaccardColor <- function(jaccard) {
        if (jaccard < 0.0) {
            # invalid
            return("-1")
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
        c(
            "aquamarine", "cadetblue2", "blue", "deeppink", "darkorchid1", "gray", "gray2", "greenyellow", "green4", "khaki4", "orange", "red", "tan4", "darkred", "yellow", "violet"
        )
    }
    
    # Generate bargraphs
    createBargraph <- function(bgChartFull=bgChartFull, bgChartAllCategories=bgChartAllCategories, orderSet, colorsList){
        output[["bargraph"]] <- renderUI(
            do.call(tabsetPanel, c(id="pvaluetab", lapply(bgChartAllCategories, function(catName){
                tmpBgNames <- unique(unlist(unname(lapply(bgChartFull[[catName]], function(x) names(x)))))
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
                dataTableBgValues <- t(as.data.frame(do.call(rbind, tmpBgCleaned)))
                dataTableBgValues <- cbind("Annotation"=rownames(dataTableBgValues), data.frame(dataTableBgValues, row.names=NULL)) # Set row names as first column
              
                bgDisplay <- plot_ly(
                    x=tmpBgCleaned[[1]], # p-values
                    y=tmpBgNames, # term names
                    name=names(tmpBgCleaned)[1],
                    type="bar",
                    marker=list(color=colorsList[names(tmpBgCleaned[1])]),
                    height=1000
                ) %>% layout(
                    title=catName,
                    margin=list(l=300, r=200, b=160), 
                    xaxis=list(title="<b>-log<sub>10</sub> (P-value)</b>", tickfont=list(size=12), automargin=TRUE),
                    yaxis=list(title="<b>Annotation Terms</b>", tickfont=list(size=10), type="category", automargin=TRUE),
                    barmode="group",
                    autosize=FALSE,
                    plot_bgcolor="transparent",
                    paper_bgcolor="transparent",
                    font=list(color=theme$textcolor)
                )
                # Remove first input set since we have already plotted it
                tmpBgCleaned <- tmpBgCleaned[-1]
                # Add additional plots to bar graph if more than 1 valid input set
                if(length(tmpBgCleaned) > 0){
                    for (i in seq_len(length(tmpBgCleaned))){
                        bgDisplay <- bgDisplay %>% add_trace(x=tmpBgCleaned[[i]], name=names(tmpBgCleaned)[i], marker=list(color=colorsList[names(tmpBgCleaned[i])])  )
                    }
                }
                return(tabPanel(title=catName, 
                    div(
                        fluidRow(
                            bgDisplay
                        ),
                        fluidRow(
                            DT::datatable({dataTableBgValues},
                                caption=HTML(paste0("<b>-log<sub>10</sub> (P-value) for ", catName, "</b>")),
                                escape=FALSE,
                                rownames=FALSE,
                                class="row-border stripe compact",
                                style="bootstrap",
                                select="none",
                                options=list( 
                                    paging=FALSE,
                                    dom="Bfrtip",
                                    pageLength=10,
                                    buttons=list("copy", "csv", "excel", "pdf", "print")
                                ),
                                extensions="Buttons"
                            ) %>% formatRound(columns=colnames(subset(dataTableBgValues, select=-c(Annotation))), digits=2)
                        )
                    )
                ))
            })))
        )
    }
    
    # Confirmation messages for Venn diagram copy buttons
    observeEvent(input$clipboardChartFrom, {
        showNotification(paste0("UUID copied!"), type="message")
    }, ignoreNULL=TRUE, ignoreInit=TRUE)
    observeEvent(input$clipboardChartTo, {
        showNotification(paste0("UUID copied!"), type="message")
    }, ignoreNULL=TRUE, ignoreInit=TRUE)
    observeEvent(input$clipboardChartShared, {
        showNotification(paste0("UUID copied!"), type="message")
    }, ignoreNULL=TRUE, ignoreInit=TRUE)

    # Perform re-enrichment on selected result chemicals
    observeEvent(input$reenrichButton, {
        # change to waiting page
        changePage(page="waiting")
        # Get set names
        reenrichSetNames <- unlist(lapply(checkboxList$checkboxes, function(i) {
            unlist(lapply(names(i), function(j){
                if(is.null(input[[j]]) & !(j %in% firstCaseWarningChems$casrns)){
                    tmpSplit <- unlist(str_split(j, "__"))
                    return(tmpSplit[2])
                } else if(!is.null(input[[j]])){
                    if(input[[j]] == TRUE) {
                        tmpSplit <- unlist(str_split(j, "__"))
                        return(tmpSplit[2])
                    } else {
                        return(NULL)
                    }
                } else {
                    return(NULL)
                }
            }))
        }))
        reenrichSetNames <- reenrichSetNames[!vapply(reenrichSetNames, is.null, FUN.VALUE=logical(1))]
        reenrichSetNames <- unique(reenrichSetNames)
        reenrichCASRNBox <- ""

        # Construct text string
        reenrichCASRNBox <- lapply(reenrichSetNames, function(x){
            reenrichSets <- unname(unlist(lapply(checkboxList$checkboxes, function(i) {
                reenrichSetsInner <- unlist(lapply(names(i), function(j){
                    tmpSplit <- unlist(str_split(j, "__"))
                    if(tmpSplit[2] == x) {
                        if(is.null(input[[j]]) & !(j %in% firstCaseWarningChems$casrns)){
                            return(tmpSplit[1])
                        } else if((!is.null(input[[j]]))){
                            if(input[[j]] == TRUE) {
                                return(tmpSplit[1])
                            }
                        }
                    }
                    return(NULL)
                }))
            })))
            reenrichSets <- reenrichSets[!vapply(reenrichSets, is.null, FUN.VALUE=logical(1))]
        })
        names(reenrichCASRNBox) <- reenrichSetNames
        reenrichCASRNBox <- lapply(names(reenrichCASRNBox), function(x) paste0("#", x, "\n", paste0(reenrichCASRNBox[[x]], collapse="\n")))
        reenrichCASRNBox <- paste0(reenrichCASRNBox, collapse="\n")

        if(reenrichCASRNBox == ""){
            # Display error if nothing selected
            showNotification("Error: No chemicals are selected.", type="error")
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
        firstCaseWarningChems$casrns <- NULL
      
        # Reset setFilesObservers$observers
        lapply(setFilesObservers$observers, function(x) x$destroy())
        setFilesObservers$observers <- NULL
      
        # Perform enrichment again
        if(!performEnrichment(reenrichCASRNBox, reenrichFlag=TRUE, originalNamesToReturn=originalNamesToReturn)){
            changePage(page="enrichment")
        }
    })
    
    # Check if a previous saved cookie exists for a recently-submitted previous request and display prompt to user
    js$getSession()
    observeEvent(input$prevSessionId, {
        showNotification(HTML(paste0("<p>You have previously submitted request(s). Click ", actionLink(inputId=paste0("prevSessionLink"), label="here"), " to view your previous session(s).</p>")), type="message")
    }, ignoreInit=FALSE, ignoreNULL=TRUE)
    
    # Open search previous enrichment menu
    observeEvent(input$prevSessionLink, {
        loadEnrichList()
    })
    
    # Check default theme value
    js$checkDefaultTheme()
    observeEvent(input$defaultTheme, {
        if(!is.null(input$defaultTheme)){
            if(input$defaultTheme == 'dark'){
                js$saveSessionTheme('dark')
            } else {
                js$saveSessionTheme('light')
            }
        } else {
            js$saveSessionTheme('light')
        }
    }, ignoreInit=TRUE, ignoreNULL=FALSE)
    
    js$getSessionTheme()
    js$getSessionThemePreferred()
    # First find the user's default theme and set
    observeEvent(input$sessionTheme, {
        # first, check if theme was set to preferred:
        if(!is.null(input$sessionThemePref)){
            if(input$sessionThemePref == "light"){
                js$initDarkTheme("light")
                theme$textcolor="#000000"
                updateRadioButtons(session, "changeThemeToggle", selected="Light")
            } else if (input$sessionThemePref == "dark"){
                js$initDarkTheme("dark")
                theme$textcolor="#FFFFFF"
                updateRadioButtons(session, "changeThemeToggle", selected="Dark")
            } else {
                if(!is.null(input$sessionTheme)){
                    js$initDarkTheme("default") # Set dark theme as default if preferred by user's browser
                    if(input$sessionTheme == "dark"){
                        theme$textcolor="#FFFFFF"
                        updateRadioButtons(session, "changeThemeToggle", selected="Auto")
                    } else if(input$sessionTheme == "light"){
                        theme$textcolor="#000000"
                        updateRadioButtons(session, "changeThemeToggle", selected="Auto")
                    }
                }
            }
        }
    }, ignoreInit=TRUE, ignoreNULL=FALSE)
})


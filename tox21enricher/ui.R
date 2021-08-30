#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define custom javascript to open manual in new tab
js_code <- "
        shinyjs.browseURL = function(url) {
          window.open(url,'_blank');
        }
      "

# Custom javascript for checking/unchecking checkboxes
js_cbx <- "
        shinyjs.check = function(cbx){
          document.getElementById(cbx).checked = true;
        }
        shinyjs.uncheck = function(cbx){
          document.getElementById(cbx).checked = false;
        }
      "

# Theme toggle js
# vvv solution from here: https://stackoverflow.com/questions/61632272/r-shiny-light-dark-mode-switch vvv
js_theme <-  "
        shinyjs.toggleTheme = function() {
        
                // define css theme filepaths
                const themes = {
                    dark: 'css/tox21enricher-dark.css',
                    light: 'css/tox21enricher-light.css'
                }

                // function that creates a new link element
                function newLink(theme) {
                    let el = document.createElement('link');
                    el.setAttribute('rel', 'stylesheet');
                    el.setAttribute('text', 'text/css');
                    el.setAttribute('href', theme);
                    return el;
                }
        
                // function that remove <link> of current theme by href
                function removeLink(theme) {
                    let el = document.querySelector(`link[href='${theme}']`)
                    return el.parentNode.removeChild(el);
                }
        
                // define vars
                const darkTheme = newLink(themes.dark);
                const lightTheme = newLink(themes.light);
                const head = document.getElementsByTagName('head')[0];
                const toggle = document.getElementById('changeThemeToggle');
        
                // define extra css and add as default
                const extraDarkThemeCSS = '.dataTables_length label, .dataTables_filter label, .dataTables_info {       color: white!important;} .paginate_button { background: white!important;} thead { color: white;}'
                const extraDarkThemeElement = document.createElement('style');
                extraDarkThemeElement.appendChild(document.createTextNode(extraDarkThemeCSS));
                head.appendChild(extraDarkThemeElement);
        
        
                // define event - checked === 'light'
                toggle.addEventListener('input', function(event) {
                  // if checked, switch to dark theme
                  console.log('>> checking');
                  console.log(toggle.checked);
                  if (toggle.checked) {
                      removeLink(themes.light);
                      head.appendChild(extraDarkThemeElement);
                      head.appendChild(darkTheme);
                  }  else {
                      // else add light theme
                      removeLink(themes.dark);
                      head.removeChild(extraDarkThemeElement);
                      head.appendChild(lightTheme);
                  }
                })
          }
        "

# Define UI for Tox21Enricher application
shinyUI(fluidPage(
    # Theme
    theme = "css/tox21enricher-light.css",

    # Lang
    lang = "en",
    
    # Set up clipboard copying
    rclipboardSetup(),

    # Application title
    title = "Tox21 Enricher",
    titlePanel("Tox21 Enricher"),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(width=2,
            style="position: fixed; overflow: visible; width: 15%",
            useShinyjs(),
            extendShinyjs(text = js_code, functions = 'browseURL'),
            extendShinyjs(text = js_theme, functions = 'toggleTheme'),
            extendShinyjs(text = js_cbx, functions = c('check', 'uncheck')),
            p("Welcome to Tox21 Enricher! Please see this ", actionLink(inputId="manualLink", label="link"), "for instructions on using this application and the descriptions about the chemical / biological categories. Other resources from the Tox21 toolbox can be viewed", tags$a(href="https://ntp.niehs.nih.gov/results/tox21/tbox/","here."), "A sufficiently robust internet connection and JavaScript are required to use all of this application's features."),
            # Manual link error output
            hidden(
              uiOutput("manualError")
            ),
            # Display API connection status
            uiOutput("apiConnection"),
            # Display enrichment total count
            uiOutput("totalEnrichments"),
            # Settings button
            actionButton(inputId="settingsButton", label="Settings", icon=icon("cog")),
            # Search enrichments button
            actionButton(inputId="searchButton", label="View previous enrichment", icon=icon("search")),
            # Theme toggle
            checkboxInput(inputId="changeThemeToggle", label=HTML(paste0(icon("moon"), " Dark theme"))),
            # Enrichment type selection
            selectInput("enrich_from", h4("Select Input Type"),
                        choices = list("User-provided CASRN list" = "user-provided CASRN list",
                                       "Chemicals with shared substructures" = "chemicals with shared substructures",
                                       "Chemicals with structural similarity" = "chemicals with structural similarity",
                                       "View annotations for Tox21 chemicals" = "View annotations for Tox21 chemicals")),
            # Hidden buttons for View previous enrichment menu
            hidden(
              fluidRow(id = "searchButtonsMenu",
                column(12, actionButton("searchPrevButton", label="View results", icon=icon("search"))),
                column(12, actionButton("searchDeleteSelected", label="Delete selected", icon=icon("trash"))),
                column(12, actionButton("searchDeleteAll", label="Delete all", icon=icon("dumpster")))
              )
            ),
            hidden(
              actionButton("refresh", "Start over", icon=icon("undo"))
            )
        ),
        
        mainPanel(id = "enrichmentPanel", width=10,
            style="width: 80%;",
            hidden(
              column(id = "searchForm", 12,
                h1("View Results from Previous Request"),
                fluidRow(
                  h3("Select Request to View Results for"),
                  hidden(
                    column(id="warningSearchColumn", 12,
                           uiOutput("searchWarning")
                    )
                  ),
                  fluidRow(
                    uiOutput("enrichmentTable") %>% withSpinner()
                  )
                )
              )
            ),
            column(id = "enrichmentForm", 12,
                h1(textOutput("selected_enrich_from")),
                # Annotation selection
                fluidRow(
                    h3("Select Chemical/Biological Annotation Categories"),
                    column(2,
                        actionButton("select_all_annotations", "Deselect all")
                    ),
                    column(10,
                        HTML("<p><b>Note</b>: Selecting no annotation categories will cause enrichment to just use the default categories.</p>")
                    ),
                ),
                fluidRow(
                    # Annotation class selection tabs
                    uiOutput("annotations") %>% withSpinner(),
                ),
                hr(),
                fluidRow(
                    h3("Select Enrichment Cutoff"),
                    bsTooltip(id="nodeCutoff", title="This will determine the maximum number of results per data set and may affect how many nodes are generated during network generation. (default = 10). Higher values may cause the enrichment process to take longer (Not available when viewing annotations for Tox21 chemicals).", placement="right", trigger="hover"),
                    sliderInput(inputId = "nodeCutoff", label="Select enrichment cutoff", value=10, min=1, max=50, step=1, width="85%"),
                    
                    hidden(
                        sliderInput(inputId = "tanimotoThreshold", label="Select Tanimoto similarity threshold (%)", value=50, min=1, max=100, step=1, width="85%")
                    )
                    
                ),
                hr(),
                # Chemical input
                fluidRow(
                    h3(textOutput("input_type")),
                    fluidRow(id = "casrnExamples",
                        column(3,
                            actionButton("example_casrns", "CASRNs example single set"),
                            actionButton("example_casrnsMulti", "CASRNs example multiple sets"))
                    ),
                    hidden( #hide SMILES example button by default
                        fluidRow(id = "smilesExamples",
                            column(3,
                                actionButton("example_smiles", "SMILES/InChI example set"),
                                actionButton("jsme_button", "Draw molecules with JSME")),
                        )
                    ),
                    fluidRow(
                        column(3,
                            actionButton("clear_casrns", "Clear input box")),
                    ),
                    hidden(
                        fluidRow(id="jsmeInput",
                            column(12,
                                includeHTML("www/html/jsme.html")
                            ),
                            column(6,
                                p("For instructions on using JSME to draw chemicals, ", tags$a(href="https://jsme-editor.github.io/help.html", target="_blank", "view the guide here."), 
                                  "When finished drawing, right-click by the drawing and select \"Copy as SMILES\" or \"Copy as InChI\" to copy a SMILES or InChI string to paste below. JSME is created by Bruno Bienfait and Peter Ertl."),
                                tags$i("B. Bienfait and P. Ertl, ", tags$a(href="https://jcheminf.biomedcentral.com/articles/10.1186/1758-2946-5-24", target="_blank", "JSME: a free molecule editor in JavaScript"), ", J. Cheminformatics 5:24 (2013)")
                            )
                        )
                    ),
                    column(12,
                        p(tags$b("Note:"), "Please verify you are using the correct chemical identifiers by referencing the ", tags$a(href="https://comptox.epa.gov/dashboard", "EPA's CompTox Chemicals Dashboard."))
                    ),
                    column(12,
                        uiOutput("inputInstructions"),
                    ),
                    column(12,
                        tags$textarea(id="submitted_chemicals", rows=12,cols=100,""),
                    )
                ),
                hidden( # hide error box; only display if there's an error
                    fluidRow(id = "errorBox",
                        column(12,
                            uiOutput("error_box"),
                        ),
                    )
                ),
                actionButton("submit", "Submit", icon=icon("arrow-alt-circle-right"))
            ),
            hidden(
                fluidRow(id="resultsContainer",
                    column(id="enrichmentResults", 12,
                        uiOutput("resultsTabset") %>% withSpinner()
                    )
                )
            ),
            hidden(
                fluidRow(id="waitingPage",
                    column(12,
                        p("Your request has been submitted and placed in the queue. After your request is processed and completed, the results can be accessed by clicking the \"Results\" button."),
                        column(3, 
                            actionButton("fetchResults", "Results", icon=icon("arrow-alt-circle-right"))
                        ),
                        column(3, 
                               actionButton("refreshWaitingPageButton", "Refresh queue", icon=icon("redo"))
                        ),
                        column(3, 
                               uiOutput("clipboard")
                        ),
                        column(3, 
                            actionButton("cancelEnrichment", "Cancel enrichment", icon=icon("times-circle"))
                        )
                    ),
                    column(12,
                        h3("Your Request"),
                        uiOutput("waitingTable") %>% withSpinner()
                    ),
                    column(12,
                        uiOutput("results_error_box"),
                    )
                )
            )
        )
    )
))

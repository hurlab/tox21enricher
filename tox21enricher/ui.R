# Custom javascript for dynamic link navigation
js_link <- "
    Shiny.addCustomMessageHandler('msg', function(message) { window.location = message;});
"

# Custom javascript for checking/unchecking checkboxes
js_cbx <- "
    shinyjs.check=function(cbx){
        document.getElementById(cbx).checked=true;
    }
    shinyjs.uncheck=function(cbx){
        document.getElementById(cbx).checked=false;
    }
"

# Custom javascript for saving transaction ID in a cookie
js_session <- "
    shinyjs.saveSession=function(params){
        transactionId = params[0];
        hours = params[1];
        // transactionId = session ID
        // get properly-formatted expiration date
        var formattedExpiryTime = 2; // default 48 hour live time (2 days)
        try {
            formattedExpiryTime = (parseInt(hours) / 24);
        } catch (err) {
            console.log(err);
            console.log('Bad time value for expiry date. Defaulting to 48 hours.');
            formattedExpiryTime = 2;
        }
        if(isNaN(formattedExpiryTime)) {
            formattedExpiryTime = 2;
        } else if (formattedExpiryTime <= 0) {
            formattedExpiryTime = 2;
        }
        var date = new Date();
        date.setTime(date.getTime() + (formattedExpiryTime * 24 * 60 * 60 * 1000));
        // save cookie
        document.cookie = 'previous_session_id__' + transactionId + '=' + transactionId + '; expires=' + date.toUTCString() + '; path=/; SameSite=Strict;';
    }
    shinyjs.getSession=function(){
        // get cookie storing transaction ID if it exists
        var prevSessionId = '';
        var cookieList = document.cookie.split(';');
        for(var i=0; i < cookieList.length; i++){
            var tmp = cookieList[i].trim();
            var tmpSplit = tmp.split('=');
            if(tmpSplit[0].startsWith('previous_session_id__')) {
                prevSessionId += (tmpSplit[1] + ';');
            }
        }
        if(prevSessionId.length > 0){
            Shiny.setInputValue('prevSessionId', prevSessionId);
        }
    }
    shinyjs.clearSession=function(params){
        transactionId = params[0];
        // delete cookie for previous session
        var date = new Date();
        date.setTime(date.getTime());
        // save cookie
        document.cookie = 'previous_session_id__' + transactionId + '=; expires=' + date.toUTCString() + '; path=/; SameSite=Strict;';
    }
    
    // saving/getting/clearing user-defined host and port information
    shinyjs.saveHostInfo=function(params){
        host = params[0];
        port = params[1];
        // save cookie with host and port info and arbitrarily long expiration date
        document.cookie = 'host_info=' + host + ':' + port + '; expires=Fri, 31 Dec 9999 23:59:59 GMT; path=/; SameSite=Strict;';
    }
    shinyjs.getHostInfo=function(){
        // get cookie storing transaction ID if it exists
        var hostInfo = null;
        var cookieList = document.cookie.split(';');
        for(var i=0; i < cookieList.length; i++){
            var tmp = cookieList[i].trim();
            var tmpSplit = tmp.split('=');
            if(tmpSplit[0] === 'host_info') {
                hostInfo = tmpSplit[1];
            }
        }
        Shiny.setInputValue('hostInfo', hostInfo);
    }
    shinyjs.clearHostInfo=function(){
        // delete cookie for previous session
        var date = new Date();
        date.setTime(date.getTime());
        // save cookie
        document.cookie = 'host_info=; expires=' + date.toUTCString() + '; path=/; SameSite=Strict;';
    }
"

# Define UI for Tox21Enricher application
shinyUI(function(){
    fluidPage(
        # Language
        lang="en",
        # Set up clipboard copying
        rclipboardSetup(),
        # Application title
        title="Tox21Enricher",
        tags$head(tags$script(js_link)),
        titlePanel(actionLink(inputId="titleLink", label="Tox21Enricher")),
        # Sidebar with options
        sidebarLayout(
            sidebarPanel(
                id="sidebar",
                width=2,
                style="position:fixed; overflow:visible; width:15%; height:600px; z-index:9999; overflow-y:scroll;",
                # Define JS for additional config & functions
                useShinyjs(),
                extendShinyjs(text=js_cbx, functions=c('check', 'uncheck')),
                extendShinyjs(text=js_session, functions=c('saveSession', 'getSession', 'clearSession', 'saveHostInfo', 'getHostInfo', 'clearHostInfo')),
                p("Welcome to Tox21Enricher! Please see this ", tags$a(href="https://github.com/hurlab/tox21enricher/blob/main/docs/Tox21Enricher_Manual.pdf", "link"), "for instructions on using this application and the descriptions about the chemical / biological categories. Other resources from the Tox21 toolbox can be viewed", tags$a(href="https://ntp.niehs.nih.gov/whatwestudy/tox21/toolbox/index.html", "here."), "A sufficiently robust internet connection and JavaScript are required to use all of this application's features."),
                p("An older version of Tox21Enricher using the", tags$a(href="https://grails.org/", "Grails framework"), "is hosted", tags$a(href="http://hurlab.med.und.edu/tox21enricher-grails", "here.")),
                # Display API connection status
                uiOutput("apiConnection"),
                # Display enrichment total count
                uiOutput("totalEnrichments"),
                # Search enrichment requests button
                actionButton(inputId="searchButton", label="View previous results", icon=icon("search")),
                hidden(
                    actionButton("refresh", "Start over", icon=icon("undo"))
                )
            ),
            # Main panel depicting enrichment instructions and options
            mainPanel(id="enrichmentPanel", width=10,
                style="width: 80%;",
                # View previous request
                hidden(
                    column(id="searchForm", 12,
                        h1("View Results from Previous Request"),
                        fluidRow(
                            fluidRow(
                                uiOutput("enrichmentTable") %>% withSpinner()
                            ),
                            fluidRow(
                                uiOutput("prevEnrichmentRecent") %>% withSpinner()
                            )
                        )
                    )
                ),
                # Submit new request
                column(id="enrichmentForm", 12,
                    h1(textOutput("selected_enrich_from")),
                    # Annotation selection
                    fluidRow(
                        h3("Select Chemical/Biological Annotation Categories"),
                        column(12, 
                            HTML(paste0("
                                <p>Select any number of annotation categories in the tabs below to allow Tox21Enricher to search them for relevant annotations during the enrichment process. Annotation categories are divided into five groups:</p>
                                <ul>
                                <li><b>PubChem Compound Annotations</b>: annotations retrieved from PubChem</li>
                                <li><b>DrugMatrix Annotations</b>: annotations retrieved from DrugMatrix</li>
                                <li><b>DrugBank Annotations</b>: annotations retrieved from DrugBank</li>
                                <li><b>CTD Annotations</b>: annotations retrieved from the Comparative Toxicogenomics Database (CTD)</li>
                                <li><b>Other Annotations</b>: annotations retrieved from miscellaneous sources</li>
                                </ul>
                                ", actionLink(inputId="annotationSelectionInfoLink", label="Click here to view our sources and selection criteria for the annotations."), "<br><br>"
                            ))
                        ),
                        column(4,
                            actionButton("select_all_annotations", "Deselect all"),
                            tipify(actionButton("selectAllLarge", "Deselect all large classes"), "Deselect/select all annotation classes with more than 1000 terms. Deselecting large classes may enhance performance.", placement="right")
                        ),
                        column(8,
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
                        tipify(sliderInput(inputId="nodeCutoff", label="Select enrichment cutoff", value=10, min=1, max=50, step=1, width="100%"), "This will determine the maximum number of results per data set and may affect how many nodes are generated during network generation. (default=10). Higher values may cause the enrichment process to take longer (Not available when viewing annotations for Tox21 chemicals).", placement="bottom")
                    ),
                    hr(),
                    fluidRow(
                        # Enrichment type selection
                        selectInput("enrich_from", h3("Select Input Type"),
                            choices=list(
                                "User-provided CASRN list"="user-provided CASRN list",
                                "Chemicals with shared substructures"="chemicals with shared substructures",
                                "Chemicals with structural similarity"="chemicals with structural similarity",
                                "View annotations for Tox21 chemicals"="View annotations for Tox21 chemicals"
                            )
                        ),
                        hidden(
                            tipify(sliderInput(inputId="tanimotoThreshold", label="Select Tanimoto similarity threshold (%)", value=50, min=2, max=100, step=1, width="100%"), "This will set the threshold for how structurally similar to the input a chemical should be to be included in enrichment.", placement="bottom")
                        )
                    ),
                    hr(),
                    # Chemical input
                    fluidRow(
                        h3(textOutput("input_type")),
                        fluidRow(id="casrnExamples",
                            column(3,
                                actionButton("example_casrns", "CASRNs example single set"),
                                actionButton("example_casrnsMulti", "CASRNs example multiple sets"))
                        ),
                        hidden( #hide SMILES example button by default
                            fluidRow(id="smilesExamples",
                                column(3,
                                    actionButton("example_smiles", "SMILES example set"),
                                    actionButton("jsme_button", "Draw molecules with JSME")),
                            )
                        ),
                        fluidRow(
                            column(3,
                                actionButton("clear_casrns", "Clear input box")),
                        ),
                        # Display JSME interface
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
                            p(tags$b("Notes:"), "Please verify you are using the correct chemical identifiers by referencing the ", tags$a(href="https://comptox.epa.gov/dashboard", "EPA's CompTox Chemicals Dashboard."))
                        ),
                        column(12,
                            uiOutput("inputInstructions"),
                        ),
                        column(12,
                            tipify(checkboxInput(inputId="includeChemsWithWarnings", label="Ignore chemicals with reactive structure warnings?", value=TRUE), title="A submitted chemical may contain a reactive functional group (nitrile, isocyanate, aldehyde, and/or epoxide) that does not appear in one or more similar chemicals. The opposite may also occur in which similar chemicals contain a reactive functional group not present in the submitted chemical. Checking this box will cause Tox21Enricher to ignore all similar chemicals that have a reactive functional group discrepancy with the submitted chemical. These chemicals will not be used in enrichment.", placement="top")
                        ),
                        column(12,
                            textAreaInput(inputId="submitted_chemicals", label=NULL, rows=12, width="100%", value="", resize="both"),
                        )
                    ),
                    column(12,
                        actionButton(inputId="submit", "Submit", icon=icon("arrow-alt-circle-right"))
                    ), 
                ),
                # Results page, hidden by default until request has completed
                hidden(
                    fluidRow(id="resultsContainer",
                        column(id="enrichmentResults", 12,
                            uiOutput("resultsTabset") %>% withSpinner()
                        )
                    )
                ),
                # Waiting page, hidden by default until a request is submitted
                hidden(
                    fluidRow(id="waitingPage",
                        column(12,
                            HTML("<p>Your request has been submitted and placed in the queue. After your request is processed and completed, the results can be accessed by clicking the \"Results\" button. <b>Please make sure to save the request's UUID for future reference and access to the results.</b></p>"),
                            column(6, 
                                uiOutput("clipboard")
                            ),
                            column(6, 
                                actionButton("cancelEnrichment", "Cancel enrichment", icon=icon("times-circle"))
                            )
                        ),
                        column(12,
                            h3("Your Request"),
                            uiOutput("waitingTable") %>% withSpinner()
                        )
                    )
                )
            )
        )
    )
})

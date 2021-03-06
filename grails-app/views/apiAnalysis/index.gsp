<!DOCTYPE html>

<html>

<head>

    <title>Tox21 Enrichment Analysis</title>
    <meta name="layout" content="main" />

    <asset:javascript src="enrichmentForm.js" />
<!-- may want to look for an alternative for the jquery form validator as it is no longer being developed -->
    <script src="//cdnjs.cloudflare.com/ajax/libs/jquery-form-validator/2.3.26/jquery.form-validator.min.js"></script>

    <style>
    .loader {
        border: 16px solid #f3f3f3;
        border-radius: 50%;
        border-top: 16px solid #3498db;
        width: 120px; 
        height: 120px;
        -webkit-animation: spin 2s linear infinite;
        animation: spin 2s linear infinite;
        align-content: center;
    }

    @-webkit-keyframes spin {
        0% { -webkit-transform: rotate(0deg); }
        100% { -webkit-transform: rotate(360deg); }
    }

    @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
    }

    .column.small-centered {
        margin-left: auto;
        margin-right: auto;
        float: none !important;
    }
    </style>

    <script>
        function exampleSmile() {
            var setSmile = "CC(=O)C1=CC=C(C=C1)[N+]([O-])\n" +
                    "ClCC1=CC=CC=C1\n" +
                    "CN(C)C1=CC=C(C=C1)\n";
            document.getElementById("SMILEBox").value= setSmile;
        }
        function singleSet() {
            var set1 = "965-90-2\n" +
                    "50-50-0\n" +
                    "979-32-8\n" +
                    "4245-41-4\n" +
                    "143-50-0\n" +
                    "17924-92-4\n"+
                    "297-76-7\n" +
                    "152-43-2\n" +
                    "313-06-4\n" +
                    "4956-37-0\n" +
                    "112400-86-9";
            document.getElementById("CASRNBox").value= set1;
            CASRNBox.value= set1;
        }

        function multiSet() {
            var setMany = "#BPA analogs\n" +
                            "2081-08-5\n" +
                            "2467-02-9\n" +
                            "1478-61-1\n" +
                            "41481-66-7\n" +
                            "5613-46-7\n" +
                            "57-63-6\n" +
                            "620-92-8\n" +
                            "77-40-7\n" +
                            "79-94-7\n" +
                            "79-95-8\n" +
                            "79-97-0\n" +
                            "80-05-7\n" +
                            "80-09-1\n" +
                            "843-55-0\n" +
                            "94-18-8\n\n" +

                            "#Flame retardants\n" +
                            "115-86-6\n" +
                            "115-96-8\n" +
                            "1241-94-7\n" +
                            "1330-78-5\n" +
                            "13674-87-8\n" +
                            "29761-21-5\n" +
                            "5436-43-1\n" +
                            "56803-37-3\n" +
                            "68937-41-7\n" +
                            "78-30-8\n" +
                            "79-94-7\n\n" +

                            "#PAH\n" +
                            "120-12-7\n" +
                            "129-00-0\n" +
                            "191-24-2\n" +
                            "206-44-0\n" +
                            "218-01-9\n" +
                            "50-32-8\n" +
                            "53-70-3\n" +
                            "56-55-3\n" +
                            "83-32-9\n" +
                            "85-01-8\n";

            document.getElementById("CASRNBox").value = setMany;
            CASRNBox.value= setMany;
        }

        function clearText() {
            document.getElementById("CASRNBox").value= "";
            CASRNBox.value= "";
        }

        function submitSmiles() {
            document.getElementById("analysisType").value = "SMILES";
            $('#enrichForm').submit();
        }

        function submitCasrns() {
            document.getElementById("analysisType").value = "CASRNS";
            $('#enrichForm').submit();
        }

        function toggleAccordions() {
            $("ul.accordion").each(function() {
                var isDown = $(this).data("tox21_isExpanded");
                var direction = "up";
                if (!isDown)
                    direction = "down";

                if (direction == "down") {
                    $("#btnToggleAccordions").text("Collapse All");
                } else {
                    $("#btnToggleAccordions").text("Expand All");
                }

                $(this).data("tox21_isExpanded", !$(this).data("tox21_isExpanded"));
                var wrapper = this;
                $(wrapper).find(".accordion-content").each(function() {
                    $(wrapper).foundation(direction, $(this));
                });
            });
        }

        function toggleCategories() {       //toggle enrichment category checkboxes
            $("input[type=checkbox]").each(function() {
                var isChecked = $(this).data("tox21_isChecked");
                var doCheck = true;
                if (!isChecked)
                    doCheck = false;

                if(doCheck == true) {
                    $("#btnToggleCategories").text("Deselect All");
                } else {
                    $("#btnToggleCategories").text("Select All");
                }

                $(this).data("tox21_isChecked", !$(this).data("tox21_isChecked"));
                if ($(this).attr('id') != "goBiop") {
                    $(this).prop('checked', doCheck);
                }
            });
        }

    </script>
</head>

<body>
    <form action="analysis/enrich" method="post" id="enrichForm">
        <div class="row" id="checkboxes">
            <br>

            Please see <g:link controller="analysisResults" action="serveUserManual" params="[filename: 'Tox21Enricher_Manual_v2.1.pdf']" target="_blank">this link </g:link> for instructions on using this application and the descriptions about the chemical/biological categories. Other resources from the Tox21 toolbox can be viewed <g:link url="https://ntp.niehs.nih.gov/results/tox21/tbox/">here.</g:link>
            <br>
            <div class="accordion" id="categoriesHeader">
                <h3>Select chemical/biological annotation categories</h3>

                <button class="button" type="button" id="btnToggleAccordions" onclick="toggleAccordions();">Expand All</button>
                <button class="button" type="button" id="btnToggleCategories" onclick="toggleCategories();">Deselect All</button>

            </div>

        </div>
        <div class="row">
            <div class="columns">
		<br>
                <h3>Input</h3>
            </div>
        </div>

        <div class="row">
            <div class="columns">
                <ul class="tabs" data-tabs id="example-tabs">
                    <li class="tabs-title is-active"><a href="#panel1" aria-selected="true">From CASRN</a></li>
                    <li class="tabs-title"><a href="#panel2">From SMILES</a></li>
                </ul>

                <div class="tabs-content" data-tabs-content="example-tabs">
                    <div class="tabs-panel is-active" id="panel1">
                        <h4>From CASRNs</h4>

                        <label for="CASRNBox">Add '#SetName' before each set, if using multiple sets at once. Ex)
                            <button class="tiny button" type="button" onclick = "singleSet()">Single Set</button>
                            <button class="tiny button" type="button" onclick ="multiSet()">Multiple Sets</button>
                            <g:if test="${isCasrnErrors}">
                                <g:textArea class="extralarge" name="CASRNBox" ><%--
                            --%><g:each in="${goodCasrns}">
                                <%--                                --%>${it}<%--
                            --%></g:each><%--
                        --%></g:textArea>
                                <g:each in="${errorCasrns}">
                                    <p style="color:red">Invalid CASRN "${it.casrn}" on line ${it.index + 1}</p>
                                </g:each>
                                <g:if test="${noCasrnInput}">
                                    <p style="color:red">Input is required to perform enrichment</p>
                                </g:if>
                            </g:if>
                            <g:else>
                                <g:textArea class="extralarge" name="CASRNBox" ></g:textArea>
                                <br>
                            </g:else>

                            <div class="reveal tiny" id="casrnLoadingModal" data-reveal data-options="closeOnClick:false; closeOnEsc: true;">
                                <p class="lead">Performing enrichment...</p>
                                <p>You will be directed to the results page shortly. Please do not use your browser's back button.</p>
                                <div class="row">
                                    <div class="small-12 small-centered columns">
                                        <div class="loader small-centered"></div>
                                    </div>
                                </div>
                            </div>

                            <input type="button" class="button" name="begin" value="Begin Enrichment Analysis" onclick="submitCasrns()" data-open="casrnLoadingModal" />
                        </label>

                    </div>
                    <div class="tabs-panel" id="panel2">
                        <h4>From SMILES strings</h4>

                        <input type="radio" name="smilesSearchType" value="Substructure" id="substructureRadio" checked><label for="substructureRadio">Substructure</label>
                        <input type="radio" name="smilesSearchType" value="Similarity" id="similarityRadio"><label for="similarityRadio">Similarity</label>
                        <div>
                            <label id="thresholdSelect">Select similarity threshold
                                <select name="thresholdSelectValue" id="thresholdSelectValue">
                                    <option value=0.5>50%</option>
                                    <option value=0.85>85%</option>
                                    <option value=0.9>90%</option>
                                    <option value=0.95>95%</option>
                                </select>
                            </label>
                        </div>

                        <label for="SMILEBox">Enter partial or complete SMILES strings, one per line Ex)
                            <button class="tiny button" type="button" onclick = "exampleSmile()">SMILES strings</button>
                        </label>



                        <g:if test="${isSmileErrors}">
                            <!--This line is all on one line with no whitespace because of the way the textArea is being populated.-->
                            <g:textArea class="extralarge" name="SMILEBox" ><%--
                        --%><g:each in="${psqlGoodSmiles}">
                            <%--                            --%>${it.smile}<%--
                        --%></g:each><%--
                --%></g:textArea>
                            <g:each in="${psqlErrorSmiles}">
                                <p style="color:red">Invalid SMILE "${it.smile}" on line ${it.index + 1}</p>
                            </g:each>
                            <g:if test="${noSmileInput}">
                                <p style="color:red">Input is required to perform enrichment</p>
                            </g:if>
                        </g:if>
                        <g:else>
                            <g:textArea class="extralarge" name="SMILEBox" ></g:textArea>
                            <br>
                        </g:else>



                        <div class="reveal tiny" id="smileLoadingModal" data-reveal data-options="close_on_background_click:false; close_on_esc: true;">
                            <p class="lead">Performing enrichment...</p>
                            <p>You will be directed to the results page shortly. Please do not use your browser's back button.</p>
                            <div class="row">
                                <div class="small-12 small-centered columns">
                                    <div class="loader small-centered"></div>
                                </div>
                            </div>
                        </div>

                        <input type="button" class="button" name="begin" value="Begin Enrichment Analysis" onclick="submitSmiles()" data-open="smileLoadingModal" />
                    </div>
                </div>
            </div>
        </div>
    </div>
        <input type="hidden" name="analysisType" id="analysisType" value="CASRNS" />
    </form>
</body>
</html>

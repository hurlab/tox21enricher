

function renderCheckboxes() {

    $.get("/tox21enricher/init/getAnnoClassAsJson", function(data) {
        //alert("Data: " + data)
        var checkboxGroups = {"PubChem Compound Annotation":[], "DrugMatrix Annotation":[], "DrugBank Annotation":[], "CTD Annotation":[], "Other Annotations":[]};
        var stringMap = {"CTD Annotation":"CTD Annotation","DrugMatrix Annotation":"DrugMatrix Annotation","PubChem Compound Annotation":"PubChem Compound Annotation","DrugBank Annotation":"DrugBank Annotation"};
        //var stringMap = {"CTD Annotation":"CTD","DrugMatrix Annotation":"DrugMatrix","PubChem Compound Annotation":"PubChem","DrugBank Annotation":"DrugBank"};

        for (var annoclassid in data) {
            var record = data[annoclassid];
            var annotype = record.annotype;
            var annodesc = record.annodesc; //annotation descriptions for tooltips
            var name = "Other Annotations";
            if (stringMap.hasOwnProperty(annotype)) {
                name = stringMap[annotype];
            }
            if (record.annoclassname == "CTD_GOFAT_BIOPROCESS") {
                checkboxGroups[name].push(["CTD_GOFAT_BIOPROCESS <sub>Extremely Slow</sub>", record.annogroovyclassname, record.annodesc, false]);
            }
            else {
                checkboxGroups[name].push([record.annoclassname, record.annogroovyclassname, record.annodesc]);
            }
        }

        var totalContainer = $("<ul>").addClass("accordion");
        $(totalContainer).attr("data-multi-expand", true);
        $(totalContainer).attr("data-allow-all-closed", true);
        $(totalContainer).attr("data-accordion", "");
        for (var group in checkboxGroups) {
            var ckGroup = checkboxGroups[group];
            var accordionBox = $("<li>").addClass("accordion").attr("data-accordion-item", "");
            var header = $("<a>").addClass("accordion-title").text(group);
            var accordionContent = $("<div>").addClass("accordion-content")
            $(accordionContent).attr("data-tab-content", "");
            var rowDiv = $("<div>").addClass("row");
            $(accordionBox).append(header)
            for (var box in ckGroup) {
                var el = ckGroup[box];
                var wrapperDiv = $("<div>").addClass("column").addClass("small-3");
                var switchDiv = $("<div>").addClass("switch").addClass("radius").addClass("tiny");
                var checkbox = $("<input>").addClass("switch-input").prop("id", el[1]).prop("type", "checkbox").prop("name", el[1]).val("on");
                if (el.length < 4 || el[3] !== false) {
                    checkbox.prop("checked", true);
                }
                var label = $("<label>").addClass("switch-paddle").prop("for", el[1]);
                label.append($("<span>").addClass("switch-active").text("On"));
                label.append($("<span>").addClass("switch-inactive").text("Off"));
                $(switchDiv).append(checkbox);
                $(switchDiv).append(label);
                $(wrapperDiv).append(switchDiv);
                $(wrapperDiv).addClass("end");

                //Tooltips for each annotation class
                $(wrapperDiv).attr("data-tooltip", "");
                $(wrapperDiv).attr("aria-haspopup", true);
                $(wrapperDiv).addClass("has-tip");
                $(wrapperDiv).attr("data-disable-hover", false);
                $(wrapperDiv).attr("tabindex", 0);
                //$(wrapperDiv).attr("data-position", "right");
                //$(wrapperDiv).attr("data-alignment", "center");
                $(wrapperDiv).attr("title", el[2]);

                $(wrapperDiv).prepend($("<label>").addClass("finger").prop("for", el[1]).html(el[0]));
                $(rowDiv).append(wrapperDiv);
                $(accordionContent).append(rowDiv);
            }
            $(accordionBox).append(accordionContent);
            $(totalContainer).append(accordionBox);
        }
        $("#checkboxes").append(totalContainer);
        $(document).foundation();
    } );


}

$(document).ready(function() { renderCheckboxes();
$(document).foundation();
});

function renderEnrichCount() {

    $.get("/tox21enricher/init/getEnrichCount", function(data) {
        var countValue = JSON.stringify(data).split(":");   
        prepCountValue = countValue[1].replace("}","").replace("]","");
        //var countValueContainer = $("<div>").addClass("row");
        //$(countValueContainer).value("Number of enrichment processes performed: " + prepCountValue);
        //$(countValueContainer).text("Number of enrichment processes performed: " + prepCountValue);

        $("#enrichCountValue").append("Total number of enrichment processes performed: " + prepCountValue);
        
        $(document).foundation();
    } );


}

$(document).ready(function() { renderEnrichCount();
$(document).foundation();
});

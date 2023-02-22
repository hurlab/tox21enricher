<h1>Tox21Enricher-Shiny</h1>
Tox21Enricher-Shiny is a web application built using the Shiny framework for R. It performs PubChem enrichment analysis on a set or sets of chemicals included in the the Tox21 chemical dataset, and it is being devloped in collaboration with the NIEHS as part of their suite of Tox21-related tools.

<br/>
<br/>
The Hur Lab at the University of North Dakota School of Medicine and Health Sciences hosts an instance of the Tox21Enricher-Shiny application at [hurlab(dot)med(dot)und(dot)edu/tox21enricher](hurlab.med.und.edu/tox21enricher) and an instance of the API at [hurlab(dot)med(dot)und(dot)edu/tox21enricher-api](hurlab.med.und.edu/tox21enricher-api)

<br/>
<br/>
The Tox21Enricher database may be downloaded in its entirety here: [http://hurlab(dot)med(dot)und(dot)edu/tox21enricher_db.tar.gz](http://hurlab.med.und.edu/tox21enricher_db.tar.gz). (You may need to right-click, copy the link address, and paste it in a new window to download.)

<br/>
<br/>
An empty copy of Tox21Enricher's "queue" database used during processing of enrichment requests can be downloaded here: [http://hurlab(dot)med(dot)und(dot)edu/tox21enricher_queue.sql](http://hurlab.med.und.edu/tox21enricher_queue.sql). (You may need to right-click, copy the link address, and paste it in a new window to download.)

<h2>Process</h2>
Chemicals are specified directly via their corresponding CASRN or indirectly with a SMILE/InChI string upon which a substructure search is executed. Once chemicals containing the given chemical string(s) are identified, CASRNs are used and enrichment proceeds as if the system was given CASRN input originally. Enrichment is then performed on the CASRNs.

After performing enrichment, the results are displayed (heatmap images per set, .gct, .xls, .txt files, cluster and chart full heatmaps) along with an option to view the cluster and chart full heatmaps visualized as networks. In the networks displayed, two nodes with a connecting edge indicate two annotations that have a statistically significant connection.

<h2>Development Tools</h2>
Tools used in the development of Tox21Enricher-Shiny:

<br/>Plumber
<br/>PostgreSQL
<br/>RDKit
<br/>R
<br/>Shiny

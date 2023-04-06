<h1>Tox21Enricher-Shiny</h1>
Tox21Enricher-Shiny is a web application built using the Shiny framework for R. It performs PubChem enrichment analysis on a set or sets of chemicals included in the the Tox21 chemical dataset, and it is being devloped in collaboration with the NIEHS as part of their suite of Tox21-related tools.

The Hur Lab at the University of North Dakota School of Medicine and Health Sciences hosts an instance of the Tox21Enricher-Shiny application [here](http://hurlab.med.und.edu/tox21enricher) and an instance of the API [here](http://hurlab.med.und.edu/tox21enricher-api).

The Tox21Enricher database may be downloaded in its entirety [here](http://hurlab.med.und.edu/tox21enricher_db.tar.gz). (You may need to right-click, copy the link address, and paste it in a new window to download.)

An empty copy of Tox21Enricher's "queue" database used during processing of enrichment requests can be downloaded [here](http://hurlab.med.und.edu/tox21enricher_queue.sql). (You may need to right-click, copy the link address, and paste it in a new window to download.)

A Docker Compose project to deploy Tox21Enricher-Shiny on your own machine can be found [here](http://hurlab.med.und.edu/tox21enricher-docker.zip).

<h2>Process</h2>
Chemicals are specified directly via their corresponding CASRN or indirectly with a SMILE/InChI string upon which a substructure search is executed. Once chemicals containing the given chemical string(s) are identified, CASRNs are used and enrichment proceeds as if the system was given CASRN input originally. Enrichment is then performed on the CASRNs.

After performing enrichment, the results are displayed (heatmap images per set, .gct, .xls, .txt files, cluster and chart full heatmaps) along with an option to view the cluster and chart full heatmaps visualized as networks. In the networks displayed, two nodes with a connecting edge indicate two annotations that have a statistically significant connection.

<h2>Specifications and Dependencies</h2>
The following packages, libraries, and tools are required to build Tox21Enricher-Shiny from source. Similar operating systems and versions of packages different from those specified may work, but were not tested and are not guaranteed to perform correctly:

| Dependency       | Version                           | Additional Notes                                      | Citation                                      |
| ---------------- | --------------------------------- | ----------------------------------------------------- | --------------------------------------------- |
| Ubuntu           | 20.04 LTS                         | OS, necessary for parallel processing                 | 
| PostgreSQL       | 12.2                              | Database management                                   |
| RDKit            | 3.8                               | Postgres database extension                           | RDKit: Open-source cheminformatics. https://www.rdkit.org |
| R                | 3.6.3 "Holding the Windsock"      |                                                       |
| bslib            | 0.2.4                             | R package                                             |
| catmaply         | 0.9.0                             | R package                                             |
| config           | 0.3.1                             | R package                                             |
| data.table       | 1.14.0                            | R package                                             |
| DBI              | 1.1.1                             | R package                                             |
| dplyr            | 1.0.5                             | R package                                             |
| DT               | 0.18                              | R package                                             |
| future           | 1.31.0                            | R package                                             |
| ggplot2          | 3.3.3                             | R package                                             |
| httr             | 1.4.2                             | R package                                             |
| igraph           | 1.2.6                             | R package                                             |
| igraphdata       | 1.0.1                             | R package                                             |
| parallel         | 3.6.3                             | R package                                             |
| plotly           | 4.9.3                             | R package                                             |
| plumber          | 1.1.0                             | R package                                             |
| plyr             | 1.8.6                             | R package                                             |
| pool             | 0.1.6                             | R package                                             |
| promises         | 1.2.0.1                           | R package                                             |
| rclipboard       | 0.1.3                             | R package                                             |
| rjson            | 0.2.20                            | R package                                             |
| RPostgres        | 1.3.3                             | R package                                             |
| shiny            | 1.6.0                             | R package                                             |
| shinyBS          | 0.61                              | R package                                             |
| shinycssloaders  | 1.0.0                             | R package                                             |
| shinydashboard   | 0.7.1                             | R package                                             |
| shinyjs          | 2.0.0                             | R package                                             |
| stringr          | 1.4.0                             | R package                                             |
| tidyverse        | 1.3.0                             | R package                                             |
| utils            | 3.6.3                             | R package                                             |
| uuid             | 0.1.4                             | R package                                             |
| VennDiagram      | 1.6.20                            | R package                                             |
| visNetwork       | 2.0.9                             | R package                                             |


Additionally, building the project images using Docker or running the project using Docker Compose requires the following:

| Dependency       | Version                           | Additional Notes                                      |
| ---------------- | --------------------------------- | ----------------------------------------------------- |
| Docker           | 23.0.1                            |                                                       |
| Docker Compose   | 1.25.0                            | Not required but recommended for local deployment     |

<br/>

Parallel processing is not supported if running the API directly on a Windows machine. Parallel processing also needs to allocate at least 2000 MiB or 2 GiB of memory to function properly.

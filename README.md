<h1>Tox21Enricher-Shiny</h1>
Tox21Enricher-Shiny is a web application built using the Shiny framework for R. It performs PubChem enrichment analysis on a set or sets of chemicals included in the the Tox21 chemical dataset, and it is being devloped in collaboration with the NIEHS as part of their suite of Tox21-related tools.

The **user manual** for Tox21Enricher-Shiny can be found [in the docs/ subdirectory](https://github.com/hurlab/tox21enricher/blob/main/docs/Tox21Enricher_Manual.pdf).
Additional documentation for acquiring, configuring, and deploying Tox21Enricher-Shiny can also be found [in the docs/ subdirectory](https://github.com/hurlab/tox21enricher/blob/main/docs/Tox21Enricher_Setup.pdf).

The Hur Lab at the University of North Dakota School of Medicine and Health Sciences hosts an **instance of the Tox21Enricher-Shiny application** [here](http://hurlab.med.und.edu/tox21enricher) and an instance of the API [here](http://hurlab.med.und.edu/tox21enricher-api).

The **Tox21Enricher database** may be downloaded in its entirety [here](http://hurlab.med.und.edu/tox21enricher_db.tar.gz). (You may need to right-click, copy the link address, and paste it in a new window to download.)

An empty **copy of Tox21Enricher's "queue" database** used during processing of enrichment requests can be downloaded [here](http://hurlab.med.und.edu/tox21enricher_queue.sql). (You may need to right-click, copy the link address, and paste it in a new window to download.)

**A Docker Compose project to deploy Tox21Enricher-Shiny** on your own machine can be found [here](http://hurlab.med.und.edu/tox21enricher-docker.zip).

Tox21Enricher-Shiny is an updated version of and replaces the original Tox21 Enricher (Now called Tox21Enricher-Grails). The Grails version is hosted at [The UND SMHS hurlab server](http://hurlab.med.und.edu/tox21enricher-grails/) and its original publication can be found at [doi:10.1002/minf.201700129](https://www.doi.org/10.1002/minf.201700129).

<h2>Usage</h2>
Chemicals are specified directly via their corresponding CASRN or indirectly with a SMILES/InChI string upon which a substructure search is executed. Once chemicals containing the given chemical string(s) are identified, CASRNs are used and enrichment proceeds as if the system was given CASRN input originally. Enrichment is then performed on the CASRNs.

After performing enrichment, the results are displayed (heatmap images per set, .gct, .xls, .txt files, cluster and chart full heatmaps) along with an option to view the cluster and chart full heatmaps visualized as networks. In the networks displayed, two nodes with a connecting edge indicate two annotations that have a statistically significant connection.

<h2>Specifications and Dependencies</h2>
The following packages, libraries, and tools are required to build Tox21Enricher-Shiny from source. Similar operating systems and versions of packages different from those specified may work, but were not tested and are not guaranteed to perform correctly:

| Dependency       | Version                           | Additional Notes                                      | Citation                                      |
| ---------------- | --------------------------------- | ----------------------------------------------------- | --------------------------------------------- |
| Ubuntu           | 20.04.2 LTS (Focal Fossa)         | OS, necessary for parallel processing                 | Canonical Ltd. Ubuntu 20.04.2 LTS (Focal Fossa) Beta. https://old-releases.ubuntu.com/releases/20.04.2/ |
| PostgreSQL       | 12.2                              | Database management                                   | The PostgreSQL Global Development Group (2023). PostgreSQL: The World's Most Advanced Open Source Relational Database. https://www.postgresql.org/
| RDKit            | 3.8                               | Postgres database extension                           | RDKit: Open-source cheminformatics. https://www.rdkit.org |
| R                | 3.6.3 "Holding the Windsock"      |                                                       | https://www.r-project.org/ |
| base             | 3.6.3                             | R package                                             | https://cran.r-project.org/bin/windows/base/old/3.6.3/ |
| config           | 0.3.1                             | R package                                             | https://cran.r-project.org/package=config |
| datasets         | 3.6.3                             | R package                                             | https://cran.r-project.org/bin/windows/base/old/3.6.3/ |
| data.table       | 1.14.0                            | R package                                             | Dowle M, Srinivasan A (2023). data.table: Extension of 'data.frame'. https://r-datatable.com, https://Rdatatable.gitlab.io/data.table, https://github.com/Rdatatable/data.table. |
| DBI              | 1.1.1                             | R package                                             | R Special Interest Group on Databases (R-SIG-DB), Wickham H, Müller K (2022). DBI: R Database Interface. https://dbi.r-dbi.org, https://github.com/r-dbi/DBI. |
| dplyr            | 1.0.5                             | R package                                             | Wickham H, François R, Henry L, Müller K, Vaughan D (2023). dplyr: A Grammar of Data Manipulation. https://dplyr.tidyverse.org, https://github.com/tidyverse/dplyr. |
| DT               | 0.18                              | R package                                             | https://github.com/rstudio/DT |
| forcats          | 0.5.1                             | R package                                             | Wickham H (2023). forcats: Tools for Working with Categorical Variables (Factors). https://forcats.tidyverse.org/, https://github.com/tidyverse/forcats. |
| futile.logger    | 1.4.3                             | R package                                             | https://cran.r-project.org/package=futile.logger |
| future           | 1.31.0                            | R package                                             | Bengtsson H (2021). “A Unifying Framework for Parallel and Distributed Processing in R using Futures.” The R Journal, 13(2), 208–227. doi:10.32614/RJ-2021-048, https://doi.org/10.32614/RJ-2021-048. |
| ggplot2          | 3.3.3                             | R package                                             | H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016. |
| graphics         | 3.6.3                             | R package                                             | R Core Team (2020). graphics. https://cran.r-project.org/bin/windows/base/old/3.6.3/ |
| grDevices        | 3.6.3                             | R package                                             | R Core Team (2020). grDevices. https://cran.r-project.org/bin/windows/base/old/3.6.3/ |
| grid             | 3.6.3                             | R package                                             | https://cran.r-project.org/bin/windows/base/old/3.6.3/ |
| heatmaply        | 1.4.2                             | R package                                             | Galili T, O'Callaghan A, Sidi J, Sievert C (2017). “heatmaply: an R package for creating interactive cluster heatmaps for online publishing.” Bioinformatics. doi:10.1093/bioinformatics/btx657. |
| httr             | 1.4.2                             | R package                                             | Wickham H (2023). httr: Tools for Working with URLs and HTTP. https://httr.r-lib.org/, https://github.com/r-lib/httr. |
| igraph           | 1.2.6                             | R package                                             | Csardi G, Nepusz T (2006). “The igraph software package for complex network research.” InterJournal, Complex Systems, 1695. https://igraph.org. |
| igraphdata       | 1.0.1                             | R package                                             | Csardi G, Nepusz T (2006). “The igraph software package for complex network research.” InterJournal, Complex Systems, 1695. https://igraph.org. |
| methods          | 3.6.3                             | R package                                             | https://cran.r-project.org/bin/windows/base/old/3.6.3/ |
| parallel         | 3.6.3                             | R package                                             | R Core Team (2022). parallel. https://cran.r-project.org/bin/windows/base/old/3.6.3/ |
| plotly           | 4.9.3                             | R package                                             | Sievert C (2020). Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and Hall/CRC. ISBN 9781138331457, https://plotly-r.com. |
| plumber          | 1.1.0                             | R package                                             | Schloerke B, Allen J (2023). plumber: An API Generator for R. https://www.rplumber.io, https://github.com/rstudio/plumber. |
| plyr             | 1.8.6                             | R package                                             | Wickham H (2011). “The Split-Apply-Combine Strategy for Data Analysis.” Journal of Statistical Software, 40(1), 1–29. https://www.jstatsoft.org/v40/i01/. |
| pool             | 0.1.6                             | R package                                             | Cheng J, Borges B, Wickham H (2023). pool: Object Pooling. https://github.com/rstudio/pool, http://rstudio.github.io/pool/. |
| promises         | 1.2.0.1                           | R package                                             | Cheng J (2023). Promises. https://rstudio.github.io/promises/ |
| purrr            | 0.3.4                             | R package                                             | Wickham H, Henry L (2023). purrr: Functional Programming Tools. https://purrr.tidyverse.org/, https://github.com/tidyverse/purrr. |
| rclipboard       | 0.1.3                             | R package                                             | https://github.com/sbihorel/rclipboard/ |
| readr            | 1.4.0                             | R package                                             | Wickham H, Hester J, Bryan J (2023). readr: Read Rectangular Text Data. https://readr.tidyverse.org, https://github.com/tidyverse/readr. |
| rjson            | 0.2.20                            | R package                                             | https://github.com/alexcb/rjson |
| RPostgres        | 1.3.3                             | R package                                             | Wickham H, Ooms J, Müller K (2023). RPostgres: Rcpp Interface to PostgreSQL. https://rpostgres.r-dbi.org, https://github.com/r-dbi/RPostgres. |
| shiny            | 1.6.0                             | R package                                             | https://shiny.rstudio.com/ |
| shinyBS          | 0.61                              | R package                                             | https://ebailey78.github.io/shinyBS/ |
| shinycssloaders  | 1.0.0                             | R package                                             | https://github.com/daattali/shinycssloaders |
| shinyjs          | 2.0.0                             | R package                                             | https://deanattali.com/shinyjs/ |
| stats            | 3.6.3                             | R package                                             | R Core Team (2020). stats. https://cran.r-project.org/bin/windows/base/old/3.6.3/ |
| stringr          | 1.4.0                             | R package                                             | Wickham H (2022). stringr: Simple, Consistent Wrappers for Common String Operations. https://stringr.tidyverse.org, https://github.com/tidyverse/stringr. |
| tibble           | 3.1.0                             | R package                                             | Müller K, Wickham H (2023). tibble: Simple Data Frames. https://tibble.tidyverse.org/, https://github.com/tidyverse/tibble. |
| tidyr            | 1.1.3                             | R package                                             | Wickham H, Vaughan D, Girlich M (2023). tidyr: Tidy Messy Data. https://tidyr.tidyverse.org, https://github.com/tidyverse/tidyr. |
| tidyverse        | 1.3.0                             | R package                                             | https://www.tidyverse.org/ |
| utils            | 3.6.3                             | R package                                             | https://cran.r-project.org/bin/windows/base/old/3.6.3/ |
| uuid             | 0.1.4                             | R package                                             | https://www.rforge.net/uuid/ |
| VennDiagram      | 1.6.20                            | R package                                             | https://cran.r-project.org/package=VennDiagram |
| visNetwork       | 2.0.9                             | R package                                             | DataStorm (2021). visNetwork, an R package for interactive network visualization. https://datastorm-open.github.io/visNetwork/


Additionally, building the project images using Docker or running the project using Docker Compose requires the following:

| Dependency       | Version                           | Additional Notes                                      | Citation                                        |
| ---------------- | --------------------------------- | ----------------------------------------------------- | ----------------------------------------------- |
| Docker           | 23.0.1                            |                                                       | https://www.docker.com/ |
| Docker Compose   | 1.25.0                            | Not required but recommended for local deployment     | https://docs.docker.com/compose/ |

<br/>

Parallel processing is not supported if running the API directly on a Windows machine. Parallel processing also needs to allocate at least 2000 MiB or 2 GiB of memory to function properly.

The development machine for Tox21Enricher-Shiny uses 64-bit Linux, so that is the recommended architecture. Other builds may not perform as expected.

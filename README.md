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
| PostgreSQL       | 11.8                              | Database management                                   | The PostgreSQL Global Development Group (2023). PostgreSQL: The World's Most Advanced Open Source Relational Database. https://www.postgresql.org/
| RDKit            |                                   | Postgres database extension                           | RDKit: Open-source cheminformatics. https://www.rdkit.org |

<h3>R and R Packages</h3>

- R version 4.2.3 (2023-03-15 ucrt): R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.
- assertthat-0.2.1: Wickham H (2019). _assertthat: Easy Pre and Post Assertions_. R package version 0.2.1, <https://CRAN.R-project.org/package=assertthat>.
- base-4.2.3: R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.
- bit-4.0.5: Oehlschlägel J, Ripley B (2022). _bit: Classes and Methods for Fast Memory-Efficient Boolean Selections_. R package version 4.0.5, <https://CRAN.R-project.org/package=bit>.
- bit64-4.0.5: Oehlschlägel J, Silvestri L (2020). _bit64: A S3 Class for Vectors of 64bit Integers_. R package version 4.0.5, <https://CRAN.R-project.org/package=bit64>.
- blob-1.2.4: Wickham H (2023). _blob: A Simple S3 Class for Representing Vectors of Binary Data ('BLOBS')_. R package version 1.2.4, <https://CRAN.R-project.org/package=blob>.
- ca-0.71.1: Nenadic O, Greenacre M (2007). “Correspondence Analysis in R, with two- and three-dimensional graphics: The ca package.” _Journal of Statistical Software_, *20*(3), 1-13. <http://www.jstatsoft.org>.
- cli-3.6.0: Csárdi G (2023). _cli: Helpers for Developing Command Line Interfaces_. R package version 3.6.0, <https://CRAN.R-project.org/package=cli>.
- codetools-0.2.19: Tierney L (2023). _codetools: Code Analysis Tools for R_. R package version 0.2-19, <https://CRAN.R-project.org/package=codetools>.
- colorspace-2.1.0
    - Zeileis A, Fisher JC, Hornik K, Ihaka R, McWhite CD, Murrell P, Stauffer R, Wilke CO (2020). “colorspace: A Toolbox for Manipulating and Assessing Colors and Palettes.” _Journal of Statistical Software_, *96*(1), 1-49. doi:10.18637/jss.v096.i01 <https://doi.org/10.18637/jss.v096.i01>.
    - Zeileis A, Hornik K, Murrell P (2009). “Escaping RGBland: Selecting Colors for Statistical Graphics.” _Computational Statistics \& Data Analysis_, *53*(9), 3259-3270. doi:10.1016/j.csda.2008.11.033 <https://doi.org/10.1016/j.csda.2008.11.033>.
    - Stauffer R, Mayr GJ, Dabernig M, Zeileis A (2009). “Somewhere over the Rainbow: How to Make Effective Use of Colors in Meteorological Visualizations.” _Bulletin of the American Meteorological Society_, *96*(2), 203-216. doi:10.1175/BAMS-D-13-00155.1 <https://doi.org/10.1175/BAMS-D-13-00155.1>.
- compiler-4.2.3: R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.
- config-0.3.1: Allaire J (2020). _config: Manage Environment Specific Configuration Values_. R package version 0.3.1, <https://CRAN.R-project.org/package=config>.
- datasets-4.2.3: R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.
- dendextend-1.17.1: Galili T (2015). “dendextend: an R package for visualizing, adjusting, and comparing trees of hierarchical clustering.” _Bioinformatics_. doi:10.1093/bioinformatics/btv428 <https://doi.org/10.1093/bioinformatics/btv428>, https://academic.oup.com/bioinformatics/article-pdf/31/22/3718/17122682/btv428.pdf, <https://academic.oup.com/bioinformatics/article/31/22/3718/240978/dendextend-an-R-package-for-visualizing-adjusting>.
- digest-0.6.33: Lucas DEwcbA, Tuszynski J, Bengtsson H, Urbanek S, Frasca M, Lewis B, Stokely M, Muehleisen H, Murdoch D, Hester J, Wu W, Kou Q, Onkelinx T, Lang M, Simko V, Hornik K, Neal R, Bell K, de Queljoe M, Suruceanu I, Denney B, Schumacher D, Chang W, Attali. D (2023). _digest: Create Compact Hash Digests of R Objects_. R package version 0.6.33, <https://CRAN.R-project.org/package=digest>.
- ellipsis-0.3.2: Wickham H (2021). _ellipsis: Tools for Working with ..._. R package version 0.3.2, <https://CRAN.R-project.org/package=ellipsis>.
- fansi-1.0.3: Gaslam B (2022). _fansi: ANSI Control Sequence Aware String Functions_. R package version 1.0.3, <https://CRAN.R-project.org/package=fansi>.
- fastmap-1.1.1: Chang W (2023). _fastmap: Fast Data Structures_. R package version 1.1.1, <https://CRAN.R-project.org/package=fastmap>.
- foreach-1.5.2: Microsoft, Weston S (2022). _foreach: Provides Foreach Looping Construct_. R package version 1.5.2, <https://CRAN.R-project.org/package=foreach>.
- formatR-1.14: Xie Y (2023). _formatR: Format R Code Automatically_. R package version 1.14, <https://CRAN.R-project.org/package=formatR>.
- futile.options-1.0.1: Rowe BLY (2018). _futile.options: Futile Options Management_. R package version 1.0.1, <https://CRAN.R-project.org/package=futile.options>.
- generics-0.1.3: Wickham H, Kuhn M, Vaughan D (2022). _generics: Common S3 Generics not Provided by Base R Methods Related to Model Fitting_. R package version 0.1.3, <https://CRAN.R-project.org/package=generics>.
- globals-0.16.2: Bengtsson H (2022). _globals: Identify Global Objects in R Expressions_. R package version 0.16.2, <https://CRAN.R-project.org/package=globals>.
- glue-1.6.2: Hester J, Bryan J (2022). _glue: Interpreted String Literals_. R package version 1.6.2, <https://CRAN.R-project.org/package=glue>.
- graphics-4.2.3: R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.
- grDevices-4.2.3: R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.
- grid-4.2.3: R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.
- gridExtra-2.3: Auguie B (2017). _gridExtra: Miscellaneous Functions for "Grid" Graphics_. R package version 2.3, <https://CRAN.R-project.org/package=gridExtra>.
- gtable-0.3.3: Wickham H, Pedersen T (2023). _gtable: Arrange 'Grobs' in Tables_. R package version 0.3.3, <https://CRAN.R-project.org/package=gtable>.
- hms-1.1.3: Müller K (2023). _hms: Pretty Time of Day_. R package version 1.1.3, <https://CRAN.R-project.org/package=hms>.
- htmltools-0.5.5: Cheng J, Sievert C, Schloerke B, Chang W, Xie Y, Allen J (2023). _htmltools: Tools for HTML_. R package version 0.5.5, <https://CRAN.R-project.org/package=htmltools>.
- htmlwidgets-1.6.2: Vaidyanathan R, Xie Y, Allaire J, Cheng J, Sievert C, Russell K (2023). _htmlwidgets: HTML Widgets for R_. R package version 1.6.2, <https://CRAN.R-project.org/package=htmlwidgets>.
- httpuv-1.6.11: Cheng J, Chang W, Reid S, Brown J, Trower B, Peslyak A (2023). _httpuv: HTTP and WebSocket Server Library_. R package version 1.6.11, <https://CRAN.R-project.org/package=httpuv>.
- iterators-1.0.14: Analytics R, Weston S (2022). _iterators: Provides Iterator Construct_. R package version 1.0.14, <https://CRAN.R-project.org/package=iterators>.
- jsonlite-1.8.7: Ooms J (2014). “The jsonlite Package: A Practical and Consistent Mapping Between JSON Data and R Objects.” _arXiv:1403.2805 [stat.CO]_. <https://arxiv.org/abs/1403.2805>.
- lambda.r-1.2.4: Rowe BLY (2019). _lambda.r: Modeling Data with Functional Programming_. R package version 1.2.4, <https://CRAN.R-project.org/package=lambda.r>.
- later-1.3.1: Chang W, Cheng J (2023). _later: Utilities for Scheduling Functions to Execute Later with Event Loops_. R package version 1.3.1, <https://CRAN.R-project.org/package=later>.
- lazyeval-0.2.2: Wickham H (2019). _lazyeval: Lazy (Non-Standard) Evaluation_. R package version 0.2.2, <https://CRAN.R-project.org/package=lazyeval>.
- lifecycle-1.0.3: Henry L, Wickham H (2022). _lifecycle: Manage the Life Cycle of your Package Functions_. R package version 1.0.3, <https://CRAN.R-project.org/package=lifecycle>.
- listenv-0.9.0: Bengtsson H (2022). _listenv: Environments Behaving (Almost) as Lists_. R package version 0.9.0, <https://CRAN.R-project.org/package=listenv>.
- magrittr-2.0.3: Bache S, Wickham H (2022). _magrittr: A Forward-Pipe Operator for R_. R package version 2.0.3, <https://CRAN.R-project.org/package=magrittr>.
- methods-4.2.3: R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.
- mime-0.12: Xie Y (2021). _mime: Map Filenames to MIME Types_. R package version 0.12, <https://CRAN.R-project.org/package=mime>.
- munsell-0.5.0: Wickham C (2018). _munsell: Utilities for Using Munsell Colours_. R package version 0.5.0, <https://CRAN.R-project.org/package=munsell>.
- parallel-4.2.3: R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.
- parallelly-1.36.0: Bengtsson H (2023). _parallelly: Enhancing the 'parallel' Package_. R package version 1.36.0, <https://CRAN.R-project.org/package=parallelly>.
- pillar-1.9.0: Müller K, Wickham H (2023). _pillar: Coloured Formatting for Columns_. R package version 1.9.0, <https://CRAN.R-project.org/package=pillar>.
- pkgconfig-2.0.3: Csárdi G (2019). _pkgconfig: Private Configuration for 'R' Packages_. R package version 2.0.3, <https://CRAN.R-project.org/package=pkgconfig>.
- R6-2.5.1: Chang W (2021). _R6: Encapsulated Classes with Reference Semantics_. R package version 2.5.1, <https://CRAN.R-project.org/package=R6>.
- RColorBrewer-1.1.3: Neuwirth E (2022). _RColorBrewer: ColorBrewer Palettes_. R package version 1.1-3, <https://CRAN.R-project.org/package=RColorBrewer>.
- Rcpp-1.0.9:
    - Eddelbuettel D, François R (2011). “Rcpp: Seamless R and C++ Integration.” _Journal of Statistical Software_, *40*(8), 1-18. doi:10.18637/jss.v040.i08 <https://doi.org/10.18637/jss.v040.i08>.
    - Eddelbuettel D (2013). _Seamless R and C++ Integration with Rcpp_. Springer, New York. doi:10.1007/978-1-4614-6868-4 <https://doi.org/10.1007/978-1-4614-6868-4>, ISBN 978-1-4614-6867-7.
    - Eddelbuettel D, Balamuta JJ (2018). “Extending extitR with extitC++: A Brief Introduction to extitRcpp.” _The American Statistician_, *72*(1), 28-36. doi:10.1080/00031305.2017.1375990 <https://doi.org/10.1080/00031305.2017.1375990>.
- registry-0.5.1: Meyer D (2019). _registry: Infrastructure for R Package Registries_. R package version 0.5-1, <https://CRAN.R-project.org/package=registry>.
- rlang-1.1.1: Henry L, Wickham H (2023). _rlang: Functions for Base Types and Core R and 'Tidyverse' Features_. R package version 1.1.1, <https://CRAN.R-project.org/package=rlang>.
- rstudioapi-0.15.0: Ushey K, Allaire J, Wickham H, Ritchie G (2023). _rstudioapi: Safely Access the RStudio API_. R package version 0.15.0, <https://CRAN.R-project.org/package=rstudioapi>.
- scales-1.2.1: Wickham H, Seidel D (2022). _scales: Scale Functions for Visualization_. R package version 1.2.1, <https://CRAN.R-project.org/package=scales>.
- seriation-1.5.1:
    - Hahsler M, Buchta C, Hornik K (2023). _seriation: Infrastructure for Ordering Objects Using Seriation_. R package version 1.5.1, <https://CRAN.R-project.org/package=seriation>.
    - Hahsler M, Hornik K, Buchta C (2008). “Getting things in order: An introduction to the R package seriation.” _Journal of Statistical Software_, *25*(3), 1-34. ISSN 1548-7660, doi:10.18637/jss.v025.i03 <https://doi.org/10.18637/jss.v025.i03>.
    - Hahsler M (2017). “An experimental comparison of seriation methods for one-mode two-way data.” _European Journal of Operational Research_, *257*(1), 133-143. doi:10.1016/j.ejor.2016.08.066 <https://doi.org/10.1016/j.ejor.2016.08.066>.
- stats-4.2.3: R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.
- stringi-1.7.8: Gagolewski M (2022). “stringi: Fast and portable character string processing in R.” _Journal of Statistical Software_, *103*(2), 1-59. doi:10.18637/jss.v103.i02 <https://doi.org/10.18637/jss.v103.i02>.
- tidyselect-1.2.0: Henry L, Wickham H (2022). _tidyselect: Select from a Set of Strings_. R package version 1.2.0, <https://CRAN.R-project.org/package=tidyselect>.
- timechange-0.2.0: Spinu V (2023). _timechange: Efficient Manipulation of Date-Times_. R package version 0.2.0, <https://CRAN.R-project.org/package=timechange>.
- tools-4.2.3: R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.
- TSP-1.2.4:
    - Hahsler M, Hornik K (2023). _TSP: Traveling Salesperson Problem (TSP)_. R package version 1.2-4, <https://CRAN.R-project.org/package=TSP>.
    - Hahsler M, Hornik K (2007). “TSP - Infrastructure for the traveling salesperson problem.” _Journal of Statistical Software_, *23*(2), 1-21. ISSN 1548-7660, doi:10.18637/jss.v023.i02 <https://doi.org/10.18637/jss.v023.i02>.
- tzdb-0.4.0: Vaughan D (2023). _tzdb: Time Zone Database Information_. R package version 0.4.0, <https://CRAN.R-project.org/package=tzdb>.
- utf8-1.2.2: Perry PO (2021). _utf8: Unicode Text Processing_. R package version 1.2.2, <https://CRAN.R-project.org/package=utf8>.
- utils-4.2.3: R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.
- vctrs-0.6.5: Wickham H, Henry L, Vaughan D (2023). _vctrs: Vector Helpers_. https://vctrs.r-lib.org/, https://github.com/r-lib/vctrs.
- webshot-0.5.5: Chang W (2023). _webshot: Take Screenshots of Web Pages_. R package version 0.5.5, <https://CRAN.R-project.org/package=webshot>.
- withr-2.5.0: Hester J, Henry L, Müller K, Ushey K, Wickham H, Chang W (2022). _withr: Run Code 'With' Temporarily Modified Global State_. R package version 2.5.0, <https://CRAN.R-project.org/package=withr>.
- xtable-1.8.4: Dahl D, Scott D, Roosen C, Magnusson A, Swinton J (2019). _xtable: Export Tables to LaTeX or HTML_. R package version 1.8-4, <https://CRAN.R-project.org/package=xtable>.
- yaml-2.3.6: Garbett SP, Stephens J, Simonov K, Xie Y, Dong Z, Wickham H, Horner J, reikoch, Beasley W, O'Connor B, Warnes GR, Quinn M, Kamvar ZN (2022). _yaml: Methods to Convert R Data to YAML and Back_. R package version 2.3.6, <https://CRAN.R-project.org/package=yaml>.

Additionally, building the project images using Docker or running the project using Docker Compose requires the following:

| Dependency       | Version                           | Additional Notes                                      | Citation                                        |
| ---------------- | --------------------------------- | ----------------------------------------------------- | ----------------------------------------------- |
| Docker           | 23.0.1                            |                                                       | https://www.docker.com/ |
| Docker Compose   | 1.25.0                            | Not required but recommended for local deployment     | https://docs.docker.com/compose/ |

<br/>

Parallel processing is not supported if running the API directly on a Windows machine. Parallel processing also needs to allocate at least 2000 MiB or 2 GiB of memory to function properly.

The development machine for Tox21Enricher-Shiny uses 64-bit Linux, so that is the recommended architecture. Other builds may not perform as expected.

<h2>Credit</h2>
If using this work, please cite the following publication(s):

- Combs P, Erickson J, Hsieh J-H, Guo K, Nolte S, Schmitt C, Auerbach S and Hur J (2023). _Tox21Enricher-Shiny: an R Shiny application for toxicity functional annotation analysis_. Front. Toxicol. 5:1147608. doi: 10.3389/ftox.2023.1147608.
- Combs P, Erickson J, Hsieh J-H, Guo K, Nolte S, Schmitt C, Auerbach S and Hur J (2023). _Corrigendum: Tox21Enricher-Shiny: an R Shiny application for toxicity functional annotation analysis_. Front. Toxicol. 5:1278066. doi: 10.3389/ftox.2023.1278066.

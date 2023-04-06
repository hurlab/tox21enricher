pkgs <- c(
    'config',
    'dplyr',
    'DT',
    'httr',
    'igraph',
    'igraphdata',
    'plotly',
    'rclipboard',
    'rjson',
    'shiny',
    'shinyBS',
    'shinycssloaders',
    'shinyjs',
    'stringr',
    'uuid',
    'VennDiagram',
    'visNetwork'
)
install.packages(pkgs, dependencies=TRUE, repos='https://cran.rstudio.com/')
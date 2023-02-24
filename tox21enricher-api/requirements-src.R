pkgs <- c(
    'config',
    'data.table',
    'DBI',
    'future',
    'ggplot2',
    'httr',
    'parallel',
    'plyr',
    'pool',
    'promises',
    'rjson',
    'RPostgres',
    'stringr',
    'tidyverse',
    'utils',
    'uuid'
)
install.packages(pkgs, dependencies=TRUE, repos='https://cran.rstudio.com/')
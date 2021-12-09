pkgs <- c(
    'config', 'DBI', 'future', 'ggplot2', 'httr', 'parallel', 'plyr', 'pool', 'promises', 'reticulate', 'rjson', 'RPostgres', 'stringr', 'tidyverse', 'uuid'
)
install.packages(pkgs, dependencies=TRUE, repos='https://cran.rstudio.com/')
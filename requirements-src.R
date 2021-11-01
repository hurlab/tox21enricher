pkgs <- c(
    'config', 'future', 'ggplot2', 'httr', 'parallel', 'plyr', 'pool', 'promises', 'rjson', 'RPostgreSQL', 'stringr', 'tidyverse', 'uuid', 'xlsx'
)
install.packages(pkgs, dependencies=TRUE, repos='https://cran.rstudio.com/')
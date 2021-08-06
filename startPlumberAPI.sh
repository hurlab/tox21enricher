#!/bin/bash
# Start Tox21 Enricher R Plumber API
R -e 'library(plumber); r <- plumb("./performEnrichment.R"); r$run(host="127.0.0.1",port=8000)'

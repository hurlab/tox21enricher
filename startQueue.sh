#!/bin/bash
# Start Tox21 Enricher R Queue

# To just run once
Rscript ./queue.R

# To restart if crash
#while true; do Rscript ./queue.R && break; done

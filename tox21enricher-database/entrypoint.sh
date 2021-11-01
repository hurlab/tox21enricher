#!/bin/bash

pg_ctl -D $HOME/tox21enricher-database/ -l $HOME/tox21enricher-database/logfile restart -m smart
while true; do
    sleep 3
done
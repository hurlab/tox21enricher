#!/bin/bash
$HOME/miniconda/bin/pg_ctl -D $HOME/tox21enricher/db/ -l $HOME/tox21enricher/db/logfile restart
exec "$@"

#!/bin/sh
start=$(date +%s.%N)
NO_EM_RESTART=1 \
  exec $HOME/b/ff/tmopt/dist/bin/firefox \
  "file:///$HOME/a/scripts/m-measure_startup.html?s=$start"

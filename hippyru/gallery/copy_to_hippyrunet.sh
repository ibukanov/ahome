#!/bin/bash

dir="$(dirname "$0")"
exec rsync -a -v --delete "$dir/" hippyru.net:/www/script/gallery
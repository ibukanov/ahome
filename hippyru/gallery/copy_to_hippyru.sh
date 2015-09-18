#!/bin/bash

dir="$(dirname "$0")"
exec rsync -a -v --delete "$dir/" hippy.ru:script/gallery
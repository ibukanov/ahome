#!/bin/sh
exec mv "$1" "${1// /_}"

#!/bin/sh
function process()
{
  rm -f "$@"
}

dir=`dirname $0`
list_file="${dir}/.cvsignore"
if [ -r "$list_file" ]; then
  list=`cat $list_file`
  process $list
fi

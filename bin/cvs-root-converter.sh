#!/bin/sh

if test "$1" != --dirlist; then
  export ROOT_FROM="$1"
  export ROOT_TO="$2"
  find . -type d -name CVS -print0 | exec xargs -0 "$0" --dirlist
  exit
fi

shift

from="$ROOT_FROM"
if test -z "$from"; then 
  from="igor.bukanov%gmail.com@cvs.mozilla.org:/cvsroot"
fi
to="$ROOT_TO"
if test -z "$to"; then 
  to="igor%mir2.org@cvs.mozilla.org:/cvsroot"
fi

on_file() {
  line=`cat "$1"`
#  if [ "$line" = "$from" ]; then
	  echo "$to" > "$1"
#  fi
}

for i do 
  name="$i/Root"
  if test -f "$name"; then
    on_file "$name"
  fi
done
  

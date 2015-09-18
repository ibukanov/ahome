#!/bin/sh

if test "$1" != --dirlist; then
  find . -type d -name .svn -print0 | exec xargs -0 "$0" --dirlist
  exit
fi

shift

# Note: 'from' is regexp for ed wich is not standard regexp. See 'man ed'.

from='\<svn+ssh://igor@mimas.runitsoft.com/home/igor/.MY-SVN'
to='svn+ssh://igor@lucky.runitsoft.com/home/igor/.MY-SVN'

on_file() {
  chmod u+w $1
  ed -s "$1" <<EOF
,s%$from%$to%g
wq $1
EOF
  chmod u-w $1
}

for i do 
  name="$i/entries"
  if test -f "$name"; then
    on_file "$name"
  fi
done
  

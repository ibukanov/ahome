#!/bin/sh
for i in *.[jJ][pP][gG]; do
    lc=$(perl -e "print(lc('$i'))")
    if test "$i" != "$lc"; then
	mv "$i" "$lc"
    fi
done

counter=0
for i in *_.jpg; do
  name="${i%%_.jpg}"
  mv "${name}.jpg" "$(printf %03d $counter).jpg"
  mv -- "$i" "-$(printf %03d $counter).jpg"
  counter=$(($counter + 1))
done
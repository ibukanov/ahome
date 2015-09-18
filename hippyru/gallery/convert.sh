#!/bin/bash

data="$HOME/suspect/cgi-bin/gallery/data"
gal="$HOME/public_html/gal"
html="$HOME/public_html"
dir_list="$HOME/tmp/dir_list.txt"

check_htaccess() {
    dir=$1
    if ! test -f "$dir/.htaccess"; then
	return 1;
    fi
    if ! grep -q -F -e "/cgi-bin/gallery/a.pl?mode=0&g=$2" "$dir/.htaccess"; then
	return 1;
    fi
    return 0
}

cd $data
for i in *; do
    if ! test -f $i/index.txt; then
	echo "$i: do not have index.txt" 1>&2
	continue
    fi
    if test "$(/bin/ls -A $i)" != "$(cd $i; /bin/ls *.txt)"; then
	echo "$i: contains non-txt files" 1>&2
	continue
    fi
    if ! test -d "$gal/$i"; then
	echo "$i: $gal/$i does not exist or is not a directory" 1>&2
	continue
    fi

    count=0;
    html_dir=""
    for j in $(grep -e "/$i\$" "$dir_list"); do
	if test "$j" = "$gal/$i"; then
	    continue
	fi
	if ! test -f "$j/.htaccess"; then
	    continue
	fi
	if ! grep -q -F -e "/cgi-bin/gallery/a.pl?mode=0&g=$i&" "$j/.htaccess"; then
	    continue
	fi
	count=$(($count + 1))
	if test $count -gt 1; then
	    if test $count -eq 2; then
		echo "$i: more then one dir with .htaccess" 1>&2
		echo "$html_dir" 1>&2
	    fi
	    echo "$j" 1>&2
	fi
	html_dir="$j"
    done

    if test $count -eq 0; then
	echo "Cannot find dir $i with .htaccess" 1>&2
	continue
    elif test $count -ne 1; then
	continue
    fi

    if echo $html_dir | grep -q "/gal/"; then
	echo "$i: cannot deal with gal subdirectories " 1>&2
	continue
    fi
    
    rel_html=${html_dir#$html/}
    soft_link="gal/$i"
    while test "$(dirname $rel_html)" != .; do
	soft_link="../$soft_link"
	rel_html="$(dirname $rel_html)"
    done

    echo "*************" $i $html_dir $soft_link
#    continue
    mv $i/*.txt $gal/$i
    rmdir $i || exit 1
    rm -f $gal/$i/.htaccess
    for j in $gal/$i/*.JPG; do mv -- "$j" "${j%.JPG}.jpg"; done
    ~/script/gallery/a.pl $gal/$i
    rm $html_dir/.htaccess || exit 1
    rm $html_dir/* || exit 1
    rmdir $html_dir || exit 1
    ln -s $soft_link $html_dir
#    break;
done
#!/bin/bash

set -e -u

shopt -s nullglob

log() {
    local i
    for i in "$@" ; do 
	printf "%s\n" "$i" 1>&2
    done
}

err() {
    log "$@"
    exit 1
}

tmp_files=()

cleanup() {
    if [[ ${#tmp_files[@]} -ge 1 ]]; then
	rm -f "${tmp_files[@]}"
    fi
}

trap cleanup EXIT


dir="."
if test $# -eq 1; then
  cd "$1"
fi 

rename="rename.lst"

date_format=%Y%m%d
current_date="$(date "+$date_format")"

counter=1
state_file="$HOME/.local/var/hippyru-galery/day_counter"



mkdir -p "$(dirname "$state_file")"
if [[ -f "$state_file" ]]; then
    read prev_date prev_counter < $state_file
    [[ "$prev_date" =~ [0-9]{8} && "$prev_counter" =~ [0-9]+ ]] || \
	    err "$state_file содержит данные в неизвестном формате." \
		"Удалите этот файл и попробуйте снова."
    if [[ "$prev_date" == "$current_date" ]]; then
	let counter=prev_counter+1
    fi
fi


old_list=(thumbnails/[0-9][0-9]-[0-9]*.jpg)
if [[ ${#old_list[@]} -ne 0 ]]; then
    echo "Удаление ${#old_list[@]} сгенерированных раньше картинок для загрузки..."
    rm "${old_list[@]}"
fi


list=(*.[Jj][Pp][Gg])

echo "Генерация ${#list[@]} картинок для загрузки..." 
mkdir -p thumbnails

file_prefix="${current_date:6:2}-"

for file in "${list[@]}" ; do
  convert_name="$(printf '%s%03u' "$file_prefix" "$counter").jpg"
  echo "$convert_name $file" >> "$rename"
  printf '[%d/%d] %s -> %s\n' $counter ${#list[@]} "$file" "$convert_name"
  convert -auto-orient -resize x700 "$file" -quality 95 "thumbnails/$convert_name"
  let counter+=1
done

echo "Закончено"

tmp="$(mktemp "$state_file.XXXXXXXXXX")"
tmp_files+=("$tmp")
printf '%s %s\n' "$current_date" "$counter" > "$tmp"
mv "$tmp" "$state_file"

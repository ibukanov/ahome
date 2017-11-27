#!/bin/bash

set -e -u

readonly NL=$'\n'

log() {
    printf "%s\n" "$*" 1>&2
}

err() {
    if [[ $# -ne 0 ]]; then
	log "$@"
    else
	log "error"
    fi
    log "stack: ${FUNCNAME[*]}"
    exit 1
}

cmd_log() {
    local no_stderr=0
    while :; do
	case $1 in
	    --no-stderr ) no_stderr=1 ;;
	    * ) break ;;
	esac
	shift
    done
    if let no_stderr; then
	log "$* 2> /dev/null"
	"$@" 2> /dev/null
    else
	log "$*"
	"$@"
    fi
}

declare -a tmp_files=()

exit_hook() {
    if [[ ${#tmp_files[@]} -ne 0 ]]; then
	rm -f "${tmp_files[@]}"
    fi
}

trap exit_hook EXIT

read_file() {
    local path="$1"
    local -a lines
    mapfile lines < $path
    printf -v R %s "${lines[@]:+${lines[@]}}"
}

declare -a environment_lines=()
declare -A pathnames=()

declare -A historic_file_set=()
declare -A current_file_set=()
declare -A current_dir_set=()
declare -a current_dir_list=()

init_path_list() {
    local f=.local/hsetup/list
    if let Setup; then
	if [[ -f .local/hsetup/list.new ]]; then
	    log "Adding .local/hsetup/list.new left from a failed run into $f"
	    cat .local/hsetup/list.new >> "$f"
	    rm .local/hsetup/list.new
	fi
    fi
    if [[ -e $f ]]; then
	local -a lines
	mapfile -t lines < "$f"
	local i
	for ((i=0; i<${#lines[@]}; i+=1)); do
	    local line="${lines[i]}"
	    local pattern='^([^[:blank:]]+)([[:blank:]]+(.+))?$'
	    [[ $line =~ $pattern ]] || \
		err "$f:$((i+1)): line does not match $pattern"
	    local kind="${BASH_REMATCH[1]}"
	    local value="${BASH_REMATCH[3]}"
	    case "$kind" in
	    file | symlink )
		historic_file_set[$value]="$kind"
		;;
	    dir ) ;;
	    * )
		log "$f:$((i+1)): ignoring unknown entry $kind"
		;;
	    esac
	done
    fi
}

record_path_entry() {
    local kind="$1"
    [[ $kind =~ ^dir|file|symlink$ ]] || err "unknown kind '$kind'"
    local path="$2"
    [[ $path =~ $'\n' ]] && err "path contains newlines - '$path'"
    [[ $path =~ "'" ]] && err "path contains single quotas - '$path'"
    [[ $path =~ [[:blank:]] ]] && err "path contains blanks - '$path'"

    if [[ $kind == dir ]]; then
	[[ ${current_dir_set[$path]-} ]] && return 1
	current_dir_set[$path]=1
    else
	[[ ${current_file_set[$path]-} ]] && return 1
	current_file_set[$path]=1
    fi
    if let Setup; then
	if [[ ! -d .local/hsetup ]]; then
	    cmd_log mkdir -p .local/hsetup
	fi
	printf "%s %s\n" "$kind" "$path" >> .local/hsetup/list.new
    fi
    return 0
}

clean_files() {
    local cleanup_list=()
    if let Setup; then
	local path
	if let ${#historic_file_set[@]}; then
	    for path in "${!historic_file_set[@]}"; do
		if [[ ! ${current_file_set[$path]-} ]]; then
		    local kind=${historic_file_set[$path]}
		    case "$kind" in
		    file )
			if [[ -f $path && ! -h $path ]]; then
			    cleanup_list+=("$path")
			fi
			;;
		    symlink )
			if [[ -e $path && -h $path ]]; then
			    cleanup_list+=("$path")
			fi
			;;
		    * ) err "unknown kind - $kind" ;;
		    esac
		fi
	    done
	    if let ${#cleanup_list[@]}; then
		log "Removing ${#cleanup_list[@]} previously created but" \
		    "no longer established files or symlinks"
		cmd_log rm "${cleanup_list[@]}"
	    fi
	fi
	mv .local/hsetup/list.new .local/hsetup/list
    fi
    if let Clean; then
	# Try to remove both historic and this run files in case the historic
	# DB was damaged or removed
	if let ${#historic_file_set[@]}; then
	    cleanup_list+=("${!historic_file_set[@]}")
	fi
	if let ${#current_file_set[@]}; then
	    cleanup_list+=("${!current_file_set[@]}")
	fi
	cleanup_list+=(.local/hsetup/list)
	cmd_log rm -f "${cleanup_list[@]}"
    fi
}

action_dir() {
    local dir="$1"
    [[ $dir == . ]] && return 0
    [[ $dir ]] || err "dir empty"
    [[ dir =~ ^/ ]] && err "dir is absolute - $dir"
    [[ dir =~ /$ ]] && err "dir ends with slash - $dir"
    [[ dir =~ // ]] && err "dir contains double slash - $dir"
    [[ dir =~ (^|/)\.(/|$) ]] && \
	err "dir contains path component that is single dot - $dir"

    local s=
    while :; do
	local name="${dir%%/*}"
	s+="${s:+/}$name"
	if record_path_entry dir "$s"; then
	    if let Setup; then
		if [[ ! -d "$s" ]]; then
		    [[ -e "$s" ]] && err "$s exists and is not directory"
		    cmd_log mkdir "$s"
		fi
	    fi
	    if let Clean; then
		if [[ -e "$s" ]]; then
		    [[ -d "$s" ]] || err "$s exists and is not directory"
		fi
	    fi
	fi
	[[ $name == "$dir" ]] && break
	dir="${dir#*/}"
    done
}

action_symlink() {
    local OPTIND opt link_is_dir= skip_if_not_found=

    while getopts ds opt; do
	case "$opt" in
	    d ) link_is_dir=1;;
	    s ) skip_if_not_found=1;;
	    * ) err "unknown option '$opt'";;
	esac
    done
    shift $((OPTIND - 1))

    local from="$1" link_dir="$2"
    [[ -n $link_dir ]] || err "empty link_dir"
    local link_name="${3-${from##*/}}"
    local link_path="$link_dir/$link_name"

    record_path_entry symlink "$link_path" || \
	err "duplicated action_symlink for $link_path"

    local target
    if [[ $link_dir == "." ]]; then
	target="$from"
    else
	local s="$link_dir"
	target=""
	while :; do
	    local name="${s%%/*}"
	    [[ name != . && name != .. ]] || err "link_dir contains . or .. as path component"
	    target+="../"
	    s="${s:$((${#name}+1))}"
	    if [[ -z $s ]]; then
		break
	    fi
	done
	target="$target$from"
    fi

    if let Setup; then
	if [[ ! -e $from ]]; then
	    if [[ $skip_if_not_found ]]; then
		return 0
	    fi
	    err "$from does not exist"
	fi
	[[ -h $from ]] && err "$from is symbolic link"
	if [[ -n $link_is_dir ]]; then
	    [[ -d $from ]] || err "$from is not an existing directory"
	else
	    [[ -f $from ]] || err "$from is not an existing regular file"
	fi
    fi

    action_dir "$link_dir"

    if let Setup; then
	if [[ -h "$link_path" ]]; then
	    [[ "$(readlink "$link_path")" == "$target" ]] && return 0
	    cmd_log rm "$link_path"
	elif [[ -d "$link_path" ]]; then
	    [[ -z "$(ls -A "$link_path")" ]] ||\
		err "$link_path is non-empty directory, remove it manually and re-run"
	    cmd_log rmdir "$link_path"
	elif [[ -f "$link_path" ]]; then
	    cmd_log rm "$link_path"
	elif [[ -e "$link_path" ]]; then
	    err "$link_path is a socket or other special file, remove it manually and re-run"
	fi
	cmd_log ln -s "$target" "$link_path"
    fi
}

# On failures this may remove the file or leave it with inconsistent
# content. This is fine as the script should be rerun in this case.
action_write_file() {
    local OPTIND opt executable= mode=

    while getopts m:x opt; do
	case "$opt" in
	    m ) mode="$OPTARG";;
	    x ) executable=1;;
	    * ) err "unknown option '$opt'";;
	esac
    done
    shift $((OPTIND - 1))
    [[ -z $executable || -z $mode ]] || err "only one of -m, -x can be given"

    local dir="$1" name="$2"
    shift 2

    local text
    if [[ $# -eq 0 ]]; then
	# Read stdin
	local -a lines
	mapfile lines
	printf -v text %s "${lines[@]:+${lines[@]}}"
    elif [[ $# -eq 1 ]]; then
	text="$1"
    else
	err "unexpected arguments"
    fi

    action_dir "$dir"
    local path="$dir/$name"

    record_path_entry file "$path" || \
	err "duplicated action_file for $path"

    if let Setup; then
	local new_file= mode_mismatch=
	if [[ ! -f "$path" || -h "$path" ]]; then
	    new_file=1
	elif read_file "$path" && [[ $text == "$R" ]]; then
	    # ensure the mode matches umask
	    local expected_mode
	    if [[ -n $mode ]]; then
		expected_mode="$mode"
	    else
		expected_mode=666
		if [[ -n $executable ]]; then
		    expected_mode=777
		fi
		# prefix with 0 to force octal interpretation
		printf -v expected_mode %o "$((0$expected_mode&~0$(umask)))"
	    fi
	    local s
	    s="$(exec find "$path" -maxdepth 0 -perm "$expected_mode" -printf 1)"
	    if [[ -n $s ]]; then
		R=
		return 0
	    fi
	    mode_mismatch=1
	fi

	if [[ $new_file ]]; then
	    log "creating new file $path"
	else
	    if [[ -z $mode_mismatch ]]; then
 		log "re-creating $path with new content"
	    else
 		log "re-creating $path to ensure proper permissions"
	    fi
	fi

	# removing the file first deals with symlinks amd special
	# files and allows to skip permission check when context is
	# different
	rm -f "$path"

	printf %s "$text" > $path
	if [[ -n $mode ]]; then
	    chmod "$mode" "$path"
	elif [[ -n $executable ]]; then
	    chmod +x "$path"
	fi
    fi
    R=1
}

add_env() {
    local name="$1"
    local value="$2"
    printf -v value %q "$value"
    environment_lines+=("export $name=$value")
}

check_dir() {
    local dir="$1"
    if [[ "${dir:0:1}" != / ]]; then
	dir="$HOME/$dir"
    fi
    if [[ -d "$dir" ]]; then
	R="$dir"
	return 0
    fi
    unset R
    return 1
}

env_dir() {
    local name=$1 dir="$2"
    if check_dir "$dir"; then
	add_env "$name" "$R"
    fi
}

path_dir() {
    local name value prev
    if [[ $# -eq 1 ]]; then
	name=PATH
	value="$1"
    elif [[ $# -eq 2 ]]; then
	name="$1"
	value="$2"
    else
	err
    fi

    local more=1
    while let more; do
	local s="${value%%:*}"
	if [[ $s == "$value" ]]; then
	    more=0
	fi
	if check_dir "$s"; then
	    s="$R"
	    prev="${pathnames[$name]-}"
	    if [[ $prev ]]; then
		s="$prev:$s"
	    fi
	    pathnames[$name]="$s"
	fi
    done
}

array_join() {
    local separator="$1"
    shift
    local s= i
    for i in "$@"; do
	s+="${s:+$separator}$i"
    done
    R="$s"
}

link_to_bin() {
    local target
    for target in "$@"; do
	if [[ -x "$target" ]]; then
	    action_symlink "../$target" "bin/${target##*/}"
	fi
    done
}

setup_env() {
    local platform
    platform="$(uname -i)"

    path_dir opt/bin

    path_dir .local/bin

    path_dir "opt/$platform/rust/bin"

    path_dir "opt/virgil/bin"

    path_dir "opt/node/bin"

    path_dir "node_modules/.bin"

    path_dir .local/bin

    add_env TEXINPUTS "$HOME/a/dev/tex_lib:"

    env_dir ELM_HOME "node_modules/elm/share"

    env_dir PERL5LIB "a/perl/mylib"

    local cygopt=/cygdrive/c/opt
    if [[ -d "$cygopt" ]]; then
	if [[ -d "$cygopt/jdk8" ]]; then
	    path_dir "$cygopt/jdk8/bin"
	else
	    path_dir "$cygopt/jdk7/bin"
	fi
	path_dir "$cygopt/ant/bin"
	path_dir "$cygopt/haskell/bin"
	path_dir "$cygopt/haskell/lib/extralibs/bin"
	path_dir "$cygopt/nodejs"

	win_home="$(cygpath -u "$USERPROFILE")"
	path_dir "$win_home/AppData/Roaming/npm"
	path_dir "$win_home/node_modules/.bin"

	path_dir "$cygopt/go/bin"
	add_env GOROOT "$(cygpath -w "$cygopt/go")"
	add_env GOPATH "$(cygpath -w "$HOME/gocode")"

    else
	path_dir "$HOME/opt/$platform/jdk1.7/bin"
	env_dir GOPATH "$HOME/gocode"
	env_dir GOPATH "/usr/share/gocode"
    fi

    local extra_file
    for extra_file in "$HOME/.opam/opam-init/variables.sh"; do
	if [[ -f $extra_file ]]; then
	    local -a lines
	    mapfile -t lines < "$extra_file"
	    local i
	    for ((i=0; i<${#lines[@]}; i+=1)); do
		local pattern='^([A-Z0-9_-]+)="([^"\\`!]*)"'
		local line="${lines[i]}"
		if ! [[ $line =~ $pattern ]]; then
		    log "$extra_file:$((i+1)): line does not match $pattern for environment extraction"
		else
		    local name="${BASH_REMATCH[1]}"
		    local value="${BASH_REMATCH[2]}"
		    local extra_pattern=
		    case "$name" in
		    PATH ) extra_pattern='^(.*):\$PATH$' ;;
		    MANPATH ) extra_pattern='^\$MANPATH:(.*)$' ;;
		    PERL5LIB ) extra_pattern='^(.*):\$PERL5LIB$' ;;
		    esac
		    if [[ $extra_pattern && $value =~ $extra_pattern ]]; then
			value="${BASH_REMATCH[1]}"
		    fi
		    if [[ $value =~ [\\``!''$] ]]; then
			log "$extra_file:$((i+1)): value contains unexpected characters - $value"
		    else
			path_dir "$name" "$value"
		    fi
		fi
	    done
	fi
    done
}

setup_emacs() {
    local emacs_dir=.emacs.d
    local emacs_init="$emacs_dir/init.el"
    local emacs_load_command='(load "~/a/emacs/my-emacs.el" t t t)'"$NL"

    action_dir "$emacs_dir/backup"

    if ! [[ -f $emacs_init ]]; then
	if let Setup; then
	    log "creating $emacs_init"
	    printf %s "$emacs_load_command" > $emacs_init
	fi
    else
	local -a lines
	mapfile -n 1 lines < $emacs_init
	if [[ ${#lines[@]} -eq 1 && ${lines[0]} == "$emacs_load_command" ]]; then
	    if let Clean; then
		log "removing custom init file into $emacs_init"
		mapfile lines < $emacs_init
		printf %s "${lines[@]:1}" > $emacs_init
	    fi
	    return 0
	fi
	if let Setup; then
	    log "inserting custom init file into $emacs_init"
	    mapfile lines < $emacs_init
	    printf %s "$emacs_load_command" "${lines[@]:+${lines[@]}}" > $emacs_init
	fi
    fi
}

setup_git_config() {

    # Array of (section name value)
    local -a config_entries=(
	push default simple
    )

    local -a credential_helper_paths=()
    local desktop_session="${DESKTOP_SESSION-}"
    case "${desktop_session,,}" in
	gnome | gnome-* | xubuntu | lubuntu )
	    credential_helper_paths+=(
		"/usr/libexec/git-core/git-credential-libsecret"
		"/usr/share/doc/git/contrib/credential/gnome-keyring/git-credential-gnome-keyring"
	    )
	    ;;
    esac
    if [[ ${#credential_helper_paths[@]} -ne 0 ]]; then
	local credential_helper=""
	if let Setup; then
	    for credential_helper in "${credential_helper_paths[@]}"; do
		if [[ -x "$credential_helper" ]]; then
		    break
		fi
		credential_helper=""
	    done
	    [[ $credential_helper ]] || \
		err "none of credential helpers for git from (${credential_helper_paths[@]}) exists or executable"
	fi
	config_entries+=(credential helper "$credential_helper")
    fi

    if let Setup; then
	local -A current_config_entries=()
	{
	    while :; do
		local name value
		read -r section_and_name || break
		read -r -d '' value
		current_config_entries[$section_and_name]="$value"
	    done
	} < <(git config --global --list --null)
	local i mismatch=0
	for ((i=0; i<${#config_entries[@]}; i+=3)); do
	    local section="${config_entries[$((i+0))]}"
	    local name="${config_entries[$((i+1))]}"
	    local value="${config_entries[$((i+2))]}"
	    if [[ ${current_config_entries[$section.$name]-} != "$value" ]]; then
		mismatch=1
	    fi
	done
	let mismatch || return 0
    fi

    if let Setup || let Clean; then
	local -A section_set=()
	local i
	for ((i=0; i<${#config_entries[@]}; i+=3)); do
	    local section="${config_entries[$((i+0))]}"
	    local name="${config_entries[$((i+1))]}"
	    section_set[$section]=1
	    cmd_log git config --global --unset-all "$section.$name" || :
	done

	# workaround for git config bug that keeps empty sections
	local section
	for section in "${!section_set[@]}"; do
	    if ! git config --get-regexp "^$section\\." > /dev/null; then
		cmd_log --no-stderr git config --global --remove-section "$section" || :
	    fi
	done
    fi

    if let Setup; then
	local i
	for ((i=0; i<${#config_entries[@]}; i+=3)); do
	    local section="${config_entries[$((i+0))]}"
	    local name="${config_entries[$((i+1))]}"
	    local value="${config_entries[$((i+2))]}"
	    cmd_log git config --global "$section.$name" "$value"
	done
    fi
}

setup_lxde() {
    local rc_dir=.config/openbox
    local rc_name rc=
    for rc_name in lxde-rc.xml lubuntu-rc.xml; do
	rc="$rc_dir/$rc_name"
	[[ -s $rc ]] && break
	rc=
    done
    if [[ ! $rc ]]; then
	return 0
    fi

    local -a keys=()
    # Send-to-untrusted the content of the keyboard
    keys+=("<keybind key='W-U'><action name='Execute'><command>$HOME/a/bin/stu</command></action></keybind>")

    # First read the file and remove any marks with previously generated code
    local insert_start='<!-- #generated# -->'
    local insert_end='<!-- #/generated# -->'

    local -a lines
    local -a without_inserts=()
    mapfile -t lines < $rc
    local line inside_insert=
    for line in "${lines[@]}"; do
	if [[ -z $inside_insert ]]; then
	    if [[ $line == "$insert_start" ]]; then
		inside_insert=1
	    else
		without_inserts+=("$line")
	    fi
	else
	    if [[ $line == "$insert_end" ]]; then
		inside_insert=
	    fi
	fi
    done
    [[ -z $inside_insert ]] || err "$rc contains $insert_start without $insert_end"

    [[ ${#without_inserts[@]} -ne 0 ]] || err "$rc contains only previusly generated lines"

    if let Setup; then
	lines=()
	local found_keyboard=
	for line in "${without_inserts[@]}"; do
	    lines+=("$line")
	    if [[ $line == "  <keyboard>" ]]; then
		[[ -z $found_keyboard ]] || err "$rc contains duplicated <keyboard> sections"
		found_keyboard=1
		lines+=("$insert_start")
		local k
		for k in "${keys[@]}"; do
		    lines+=("    $k")
		done
		lines+=("$insert_end")
	    fi
	done
	printf '%s\n' "${lines[@]}" > "$rc.tmp"
	mv "$rc.tmp" "$rc"
    fi

    if let Clean; then
	printf '%s\n' "${without_inserts[@]}" > "$rc.tmp"
	mv "$rc.tmp" "$rc"
    fi
}

setup_gnome() {

    if type -p gsettings > /dev/null; then
	# disable blinking cursor in terminals
	local profile_uuid
	profile_uuid="$(gsettings get org.gnome.Terminal.ProfilesList default 2>/dev/null || :)"
	if [[ $profile_uuid ]]; then
	    [[ $profile_uuid =~ ^\'(.*)\'$ ]] && profile_uuid="${BASH_REMATCH[1]}"
	    local p="org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$profile_uuid/"
	    local value
	    value="$(gsettings get "$p" cursor-blink-mode || :)"
	    if [[ -z $value ]]; then
		log "failed to read cursor-blink-mode for gnome terminal"
	    else
		if let Setup; then
		    [[ $value == "'off'" ]] || gsettings set "$p" cursor-blink-mode off
		fi
		if let Clean; then
		    gsettings reset "$p" cursor-blink-mode
		fi
	    fi
	fi
    fi
}

main() {
    local i
    local Setup=0
    local Clean=0

    if [[ $# -eq 0 ]]; then
	Setup=1
    else
	case $1 in
	    clean ) Clean=1 ;;
	    setup ) Setup=1 ;;
	    * ) err "unknown action $1" ;;
	esac
	shift
	[[ $# -eq 0 ]] || err "unexpected extra action argument $1"
    fi

    local hsetup_source_dir
    hsetup_source_dir="$(realpath --relative-base="$HOME" "${BASH_SOURCE[0]}")"
    hsetup_source_dir="${hsetup_source_dir%/*}"

    local -a path_dirs=()
    local -a manp_dirs=()

    cd "$HOME"

    init_path_list

    action_dir ".local/hsetup"
    action_dir ".local/hsetup/bin"

    path_dir a/bin

    action_symlink -s p/git-subrepo/lib/git-subrepo .local/hsetup/bin
    path_dir MANPATH p/git-subrepo/man

    # dot files that are symlinked to home
    for i in "$hsetup_source_dir/"*.dot; do
	name="${i##*/}"
	action_symlink "$i" . ".${name%.dot}"
    done

    # extra bash symlinks besides .bashrc
    action_symlink "$hsetup_source_dir/bashrc.dot" . .bash_profile

    action_symlink -d "$hsetup_source_dir/xfce-terminal" .config/xfce4 terminal

    action_symlink "$hsetup_source_dir/lxterminal.conf" .config/lxterminal lxterminal.conf

    action_dir .config/autostart
    action_write_file .config/autostart u-autostart.desktop "\
[Desktop Entry]
Type=Application
Exec=$HOME/a/bin/u-start-session
Hidden=false
X-GNOME-Autostart-enabled=true
Name=Custom Session
Comment=Start custom session script
"

    action_symlink "$hsetup_source_dir/u-term.desktop" .local/share/applications

    setup_env

    setup_emacs

    setup_lxde

    setup_gnome

    setup_git_config

    if type -t extra_setup > /dev/null; then
	extra_setup
    fi

    if let Setup; then
	local d
	for d in /usr/local/bin /usr/local/sbin /bin /sbin /usr/bin /usr/sbin; do
	    # Do not add /bin if it is a symlink to /usr/bin
	    if [[ $d != /usr/* && $(realpath -qm "$d") == "$(realpath -qm "/usr/$d")" ]]; then
		continue
	    fi
	    path_dir "$d"
	done
	if [[ ${pathnames[MANPATH]-} ]]; then
	    pathnames[MANPATH]+=":"
	fi
	local -a sorted_names
	mapfile -t sorted_names < <(printf '%s\n' "${!pathnames[@]}" | sort)
	local name
	for name in ${sorted_names[@]}; do
	    add_env "$name" "${pathnames[$name]}"
	done

	local s
	printf -v s '%s\n' "${environment_lines[@]}"
	action_write_file .local/hsetup env "$s"
    fi

    clean_files

    if let Clean; then
	if let ${#current_dir_set[@]}; then
	    local -a reverse_sorted
	    mapfile -t reverse_sorted < \
		<(printf '%s\n' "${!current_dir_set[@]}" | sort -r)
	    cmd_log rmdir --ignore-fail-on-non-empty "${reverse_sorted[@]}"
	fi
	if [[ -d .local/hsetup ]]; then
	    cmd_log rm -rf .local/hsetup
	fi
    fi
}

main "$@"

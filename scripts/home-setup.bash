#!/bin/bash

set -e -u

readonly NL=$'\n'

log() {
    printf "%s\n" "$*" 1>&2
}

err() {
    log "$@"
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
    local path="$1" var_name="$2"
    local -a lines
    mapfile lines < $path
    printf -v "$var_name" %s "${lines[@]:+${lines[@]}}"
}

declare -A cleanup_files=()
declare -A cleanup_dirs=()

action_dir() {
    local dir="$1"
    if [[ $dir == . ]]; then
	return 0
    fi
    if let Setup; then
	[[ -d "$dir" ]] && return 0
	[[ -e "$dir" ]] && err "$dir exists and is not directory"
	cmd_log mkdir -p "$dir"
    fi
    if let Clean; then
	if [[ -e "$dir" ]]; then
	    [[ -d "$dir" ]] || err "$dir exists and is not directory"
	    cleanup_dirs[$dir]=1
	fi
    fi
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
    if let Clean; then
	cleanup_files[$link_path]=1
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

    [[ $# -ge 1 ]] || err "missing path argument"
    local path="$1"
    shift

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

    if let Setup; then
	local new_file= mode_mismatch= old_text
	if [[ ! -f "$path" || -h "$path" ]]; then
	    new_file=1
	elif read_file "$path" old_text && [[ $text == "$old_text" ]]; then
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
    if let Clean; then
	cleanup_files[$path]=1
    fi
}

env_dir() {
    local name=$1 dir="$2"
    if [[ "${dir:0:1}" != / ]]; then
	dir="$HOME/$dir"
    fi
    if [[ -d "$dir" ]]; then
	env+=("$name" "$dir")
    fi
}

path_dir() {
    local dir
    for dir in "$@"; do
	if [[ $dir ]]; then
	    if [[ "${dir:0:1}" != "/" ]]; then
		dir="$HOME/$dir"
	    fi
	    if [[ -d "$dir" ]]; then
		path_value+=":$dir"
	    fi
	fi
    done
    return 0
}

man_dir() {
    for dir in "$@"; do
	if [[ -n "$dir" ]]; then
	    if [[ "${dir:0:1}" != "/" ]]; then
		dir="$HOME/$dir"
	    fi
	    if [[ -d "$dir" ]]; then
		manpath_value+="$dir:"
	    fi
	fi
    done
    return 0
}

link_to_bin() {
    local target
    for target in "$@"; do
	if [[ -x "$target" ]]; then
	    action_symlink "../$target" "bin/${target##*/}"
	fi
    done
}

write_dot_env() {
    local -a env=()

    local platform
    platform="$(uname -i)"

    path_dir opt/bin
    
    path_dir "opt/$platform/rust/bin"
    
    path_dir "opt/virgil/bin"
    
    path_dir "opt/node/bin"
    
    path_dir "node_modules/.bin"

    path_dir .local/bin
    
    env+=(TEXINPUTS "$HOME/a/dev/tex_lib:")
    
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
	env+=(GOROOT "$(cygpath -w "$cygopt/go")")
	env+=(GOPATH "$(cygpath -w "$HOME/gocode")")
	
    else
	path_dir "$HOME/opt/$platform/jdk1.7/bin"
	env+=(GOPATH "$HOME/gocode")
    fi

    env+=(PATH "$path_value")
    if [[ -n $manpath_value ]]; then
	env+=(MANPATH "$manpath_value")
    fi

    local s= i
    for ((i=0; i<${#env[@]}; i+=2)); do
	s+="export ${env[$i]}=$(printf %q "${env[$((i+1))]}")$NL"
    done
    
    action_write_file ~/.env "$s"
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
	user name "$USER_NAME"
	user email "$USER_EMAIL"
	push default simple
    )
    
    local -a credential_helper_paths=()
    local desktop_session="${DESKTOP_SESSION-}"
    case "${desktop_session,,}" in
	gnome | gnome-* | xubuntu | lubuntu ) 
	    credential_helper_paths+=(
		"/usr/libexec/git-core/git-credential-gnome-keyring" 
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
    local rc="$HOME/.config/openbox/lxde-rc.xml"
    [[ -s $rc ]] || return 0
    local -a keys=()
    # Send-to-untrusted the content of the keyboard
    keys+=("<keybind key='W-U'><action name='Execute'><command>stu</command></action></keybind>")

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
	action_write_file "$rc" "$(printf '%s\n' "${lines[@]}")"
    fi

    if let Clean; then
	# Force write on under clean
	Setup=1 Clean=0 action_write_file "$rc" "$(printf '%s\n' "${without_inserts[@]}")"
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

    local path_value="$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin"
    local manpath_value=""
    
    cd "$HOME"

    # executables
    for i in $(find a/scripts a/mozilla -mindepth 1 -maxdepth 1 -type f -executable); do
	action_symlink "$i" bin "${i##*/}"
    done

    action_symlink -s p/git-subrepo/lib/git-subrepo bin
    man_dir p/git-subrepo/man

    # dot files that are symlinked to home
    for i in a/scripts/*.dot; do
	name="${i##*/}"
	action_symlink "$i" . ".${name%.dot}"
    done

    # extra bash symlinks besides .bashrc
    action_symlink "a/scripts/bashrc.dot" . .bash_profile

    action_symlink -d a/scripts/xfce-terminal .config/xfce4 terminal

    action_symlink a/scripts/lxterminal.conf .config/lxterminal lxterminal.conf

    action_symlink a/scripts/autostart.desktop .config/autostart

    action_symlink a/scripts/u-term.desktop .local/share/applications

    action_symlink a/scripts/u-xstartup-vnc .vnc xstartup

    setup_emacs

    setup_lxde

    setup_git_config

    if type -t extra_setup > /dev/null; then
	extra_setup
    fi

    write_dot_env

    if let Clean; then
	
	if [[ ${#cleanup_files[@]} -ne 0 ]]; then
	    cmd_log rm -f "${!cleanup_files[@]}"
	fi
	if [[ ${#cleanup_dirs[@]} -ne 0 ]]; then
	    cmd_log rmdir -p --ignore-fail-on-non-empty "${!cleanup_dirs[@]}"
	fi
    fi
}

main "$@"

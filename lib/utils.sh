# shellcheck shell=dash
# shellcheck enable=all
#shellcheck disable=SC2250

set -e -u

readonly NL='
'

#shellcheck disable=SC2034
readonly DEFAULT_IFS="${IFS}"

# Variable to return the results
R=

LANG=C.UTF-8
export LANG

log() {
  local s
  s="${0##*/}"
  printf '%s: %s\n' "$s" "$*" >&2
}

err() {
  local s
  s="${0##*/}:"
  # take advantage of few bash-exposed variables when available
  if test "${BASH_LINENO-}"; then
    eval 's="$s${BASH_LINENO[0]}:"'
  fi
  if test "${FUNCNAME-}"; then
    eval 's="$s${FUNCNAME[1]}:"'
  fi
  printf '%s %s\n' "$s" "$*" >&2
  exit 1
}

warn() {
  log "WARNING:" "$@"
}

cmd_log() {
  log "$*"
  "$@"
}

getopts_err() {
  local name msg
  name="$1"
  case "$name" in
  : ) msg="-$OPTARG requires an argument" ;;
  \? ) msg="unknown option -$OPTARG" ;;
  * ) msg="-$name is listed in getopts arguments but not processed" ;;
  esac

  # If this is called in a function, print function name
  local function_name parent_name
  function_name=
  parent_name=
  if test "${FUNCNAME-}"; then
    if eval 'test 2 -lt "${#FUNCNAME[@]}"'; then
      eval 'function_name="${FUNCNAME[1]}";'
      eval 'parent_name="${FUNCNAME[2]}";'
    fi
  fi
  if test "$function_name"; then
    err "$parent_name->$function_name - $msg"
  else
    err "$msg"
  fi
}


# Append to eargs space-separated arguments with spaces and other special
# characters escaped.
earg() {
  local arg do_escape escaped before_quote
  for arg in "$@"; do
    do_escape=
    case "$arg" in
    "" | *[!A-Z0-9a-z_.,/:-]*) do_escape=1 ;;
    *) ;;
    esac
    if test "$do_escape"; then
      escaped=
      while : ; do
        before_quote="${arg%%\'*}"
        if test "$arg" = "$before_quote"; then
          break
        fi
        escaped="$escaped$before_quote'\\''"
        arg="${arg#*\'}"
      done
      arg="'$escaped$arg'"
    fi
    if test "$eargs"; then
      eargs="$eargs "
    fi
    eargs="$eargs$arg"
  done
}

# Set R to concatenation of arguments with spaces escaped if necessary.
escape_for_shell() {
  test 1 -eq $# || \
    err "escape_for_shell takes exactly one argument while $# were given"
  R=
  local eargs
  eargs=
  earg "$1"
  R="$eargs"
}

pl(){
  lines="$lines$*$NL"
}

read_stdin() {
  local line
  if test -t 0; then
    err "cannot read from stdin when it is a terminal"
  fi
  R=
  line=
  while IFS= read -r line; do
    R="$R$line$NL"
    line=
  done
  # line is not empty when the last line was not terminated by eof
  R="$R$line"
}

current_user_group=

get_user_group() {
  local user_name
  user_name="$1"
  if test "$user_name" = "$USER" && test "$current_user_group"; then
    R="$current_user_group"
    return 0
  fi
  R="$(id -gn "$user_name")"
  if test "$user_name" = "$USER"; then
    current_user_group="$R"
    readonly current_user_group
  fi
}

# Helper to override in case tracking of all written paths is desired
write_file_note_path() {
  :
}

file_update=
file_update_count=0

write_file() {
  local user group mode
  user=
  group=
  mode=
  local opt OPTIND
  while getopts :g:m:u: opt; do
    case "$opt" in
    g) group="$OPTARG" ;;
    m) mode="$OPTARG" ;;
    u) user="$OPTARG" ;;
    *) getopts_err "$opt" ;;
    esac
  done

  shift $((OPTIND - 1))
  test $# -eq 2 -o $# -eq 1 || \
      err "write_file takes path and optional body arguments"

  local path body
  path="$1"
  if test $# -eq 2; then
    body="$2"
  else
    read_stdin
    body="$R"
  fi

  local use_default_ownership
  use_default_ownership=
  if ! test "$user" && ! test "$group"; then
    use_default_ownership=1
  fi
  if ! test "$user"; then
    user="$USER"
  fi
  if ! test "$group"; then
    get_user_group "$user"
    group="$R"
  fi

  local saved_umask
  saved_umask="$(umask)"

  local wanted_umask need_chmod
  wanted_umask=
  need_chmod=
  if test "$mode"; then
    case "$mode" in
    0644) wanted_umask=0022 ;;
    0640) wanted_umask=0027 ;;
    0600) wanted_umask=0077 ;;
    0660) wanted_umask=0007 ;;
    0755) wanted_umask=0022 need_chmod=1 ;;
    *) err "unsupported mode - $mode" ;;
    esac
    if test "$wanted_umask" = "$saved_umask"; then
      wanted_umask=
    fi
  else
    case "$saved_umask" in
    0022) mode=0644 ;;
    0002) mode=0664 ;;
    *) err "unsupported umask - $saved_umask" ;;
    esac
  fi

  local do_update
  do_update=1
  while :; do
    if ! test -f "$path" || test -h "$path"; then
      log "creating new $path"
      break;
    fi
    local s
    s="$(find "$path" \
        -maxdepth 0 -perm "$mode" -user "$user" -group "$group" \
        -printf 1 \
    )"
    if ! test "$s"; then
      log "updating $path - permission changes"
      break;
    fi

    if printf %s "$body" | cmp -s "$path" -; then
      # Permissions and text matches
      do_update=
      break
    fi

    log "updating $path - content changes"
    break
  done

  if ! test "$do_update"; then
    file_update=
  else
    # Use temporary to ensure atomic operation on filesystem
    local tmp
    tmp="$path.tmp"
    if test -f "$tmp"; then
      rm "$tmp"
    fi

    if test "$wanted_umask"; then
      umask "$wanted_umask"
    fi
    printf %s "$body" > "$tmp"
    if test "$wanted_umask"; then
      umask "$saved_umask"
    fi

    if test "$need_chmod"; then
      chmod "${mode}" "${tmp}"
    fi
    if ! test "$use_default_ownership"; then
      chown "$user:$group" "$tmp"
    fi
    mv -fT "$tmp" "$path"

    # shellcheck disable=SC2034
    file_update=1
    : $((file_update_count+=1))
  fi

  write_file_note_path "$path"
}

# Capture the standard output of the command passed as arguments in R and the
# standard error in R2.
capture_stdout_stderr() {
  # Separator between stderr and stdout
  local separator
  separator="$capture_stdout_stderr_separator"
  if ! test "$separator"; then
    separator="$(dd if=/dev/urandom count=1 bs=9 status=none | base64)"
    test "${#separator}" -eq 12 || err "Failed to get random data"
    capture_stdout_stderr_separator="$separator"
  fi
  local stderr_stdout
  stderr_stdout="$(
      # Print the separator after stdout to detect the absence of \n in stdout.
      { stdout="$("$@" && printf '%s\n' "$separator" || exit "$?")" ; } 2>&1
      printf '%s\n' "$separator"
      printf '%s\n' "$stdout"

  )"
  # stderr
  R2="${stderr_stdout%"$separator$NL"*}"

  # stdout and separator
  R="${stderr_stdout#*"$separator$NL"}"

  # stdout
  R="${R%"$separator"}"
}

capture_stdout_stderr_separator=

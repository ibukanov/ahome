test "${rc_has_setup-}" && return 0

rc_is_mac=
test -d /Library && rc_is_mac=1

rc_is_linux=
test -z "$rc_is_mac" && test -z "${MSYS-}" && rc_is_linux=1

rc_is_msys() {
  test "${MSYS-}"
}

rc_is_mac() {
  test "$rc_is_mac"
}

rc_is_linux() {
  test "$rc_is_linux"
}

rc_is_bash() {
  test "${BASH_VERSION-}"
}

rc_is_zsh() {
  test "${ZSH_VERSION-}"
}

# Read Fedora-specific startup file. On Debian and Ubuntu bash is
# compiled to read system-wide files automatically.

if rc_is_bash; then
  test -f /etc/bashrc && . /etc/bashrc
fi

# Append all colon-separated components from the dirs to prefix but
# only if they not already there.
rc_merge_path() {
  local prefix dirs
  prefix="$1"
  dirs="$2"
  # Strip initial colon in prefix
  prefix="${prefix#:}"
  if test -z "$prefix"; then
    R="$dirs"
    return 0
  fi
  if test -z "$dirs"; then
    R="$prefix"
    return 0
  fi
  if test "$dirs" = ":"; then
    R="$prefix:"
    return 0
  fi
  while :; do
    local dir
    # Select the first component
    dir="${dirs%%:*}"
    if test "$prefix" != "$dir"; then
      # Check if prefix starts, ends or contains dir in the middle
      case "$prefix" in
      "$dir":*|*:"$dir"|*:"$dir":* ) ;;
      * )
        # New directory
        prefix="$prefix:$dir"
        ;;
      esac
    fi
    if test "$dir" = "$dirs"; then
      # The last components withot colon
      break
    fi
    dirs="${dirs#*:}"
    if test -z "$dirs"; then
      # $disrs ends with : indicating default like in MANPATH, keep it in the
      # result.
      prefix="$prefix:"
      break
    fi
  done
  R="$prefix"
}

rc_setup_path() {
  local p m data_dirs d R
  p="$rc_ahome/bin:$rc_ahome/local/bin:$rc_ahome/state/bin"
  m=
  data_dirs=

  d="$HOME/opt/bin"
  test -d "$d" && p="$p:$d"

  d="$HOME/opt/bin"
  test -d "$d" && p="$p:$d"

  d="$HOME/opt/node/bin"
  if test -d "$d"; then
    p="$p:$d"
    d="$HOME/opt/node/share/man"
    test -d "$d" && m="$m:$d"
  fi

  # Rust support
  d="$HOME/.cargo/bin"
  test -d "$d" && p="$p:$d"

  d="$HOME/p/git-subrepo/man"
  test -d "$d" && m="$m:$d"

  # Go install
  d="/usr/local/go/bin"
  test -d "$d" && p="$p:$d"

  # Go compiled packages
  d="$HOME/go/bin"
  test -d "$d" && p="$p:$d"

  d="$HOME/python/active/bin"
  test -d "$d" && p="$p:$(realpath "$d")"

  if test "$rc_flatpak"; then
    data_dirs="$data_dirs:$rc_flatpak:$HOME/.local/share/flatpak/exports/share"
  fi

  if test "$rc_nix_profile"; then
    p="$p:$rc_nix_profile/bin"
    m="$m:$rc_nix_profile/share/man"
    data_dirs="$data_dirs:$rc_nix_profile/share:/nix/var/nix/profiles/default/share"
  fi

  if test "$rc_homebrew"; then
    if rc_is_mac; then
      p="$p:$rc_homebrew/bin"
      p="$p:$rc_homebrew/sbin"
      d="$rc_homebrew/opt/coreutils/libexec/gnubin"
      test -d "$d" && p="$p:$d"
    else
      p="$p:$rc_homebrew/bin"
      m="$m:$rc_homebrew/share/man"
    fi
  fi

  if test -z "${AHOME_FORCED_PATH-}"; then
    rc_merge_path "$p" "$PATH"
    export PATH="$R"
  fi

  if test "$m"; then
    # empty path in MANPATH means use the default
    rc_merge_path "$m" "${MANPATH:-:}"
    export MANPATH="$R"
  fi

  if test "$data_dirs"; then
    # According to XDG spec the default is /usr/local/share:/usr/share
    rc_merge_path "$data_dirs" "${XDG_DATA_DIRS:-"/usr/local/share:/usr/share"}"
    export XDG_DATA_DIRS="$R"
  fi
}

rc_setup_env() {
  local rc_nix_profile rc_homebrew rc_flatpak
  rc_nix_profile=
  if test -x /nix; then
     rc_nix_profile="$HOME/.nix-profile"
  fi
  if rc_is_mac; then
    rc_homebrew=/opt/homebrew
  else
    rc_homebrew=/home/linuxbrew/.linuxbrew
  fi
  if ! test -x "$rc_homebrew"; then
    rc_homebrew=
  fi
  rc_flatpak=/var/lib/flatpak/exports/share
  if ! test -d "$rc_flatpak"; then
    rc_flatpak=
  fi

  if rc_is_mac; then
    export LANG=en_US.UTF-8
  else
    export LANG=C.UTF-8
  fi
  #export LC_ALL C

  rc_setup_path

  export EDITOR=u-cvs-editor

  # ninja tool
  export NINJA_STATUS="[%r processes, %f/%t @ %o/s : %es] "

  # TEX
  export TEXINPUTS="$rc_ahome/dev/tex_lib:"

  if rc_is_linux && test -z "${XDG_RUNTIME_DIR-}"; then
    local id
    id="$(id -u)"
    if test -d "/run/user/$id"; then
      export XDG_RUNTIME_DIR="/run/user/$id"
    fi
  fi

  if ! test "${SSH_AUTH_SOCK-}" && ! rc_is_msys; then
    rc_ensure_ssh_agent
  fi

  if test "$WSLENV"; then
    GPG_TTY="$(tty)"
    export GPG_TTY
    export AWS_VAULT_BACKEND=pass

    #export AWS_VAULT_PASS_PASSWORD_STORE_DIR="$HOME/.password-store/aws-vault"
    # Under WSL allow access to YubiKeys.
    #local x
    #x="/mnt/c/Program Files/OpenSSH/ssh-sk-helper.exe"
    #test -x "$x" && export SSH_SK_HELPER="$x"
  fi

  if test "$rc_nix_profile"; then
     export NIX_PROFILES="/nix/var/nix/profiles/default $rc_nix_profile"
     if test  -e /etc/ssl/certs/ca-certificates.crt; then
       export NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt
     elif test -e /etc/pki/tls/certs/ca-bundle.crt; then
       export NIX_SSL_CERT_FILE=/etc/pki/tls/certs/ca-bundle.crt
     fi
  fi

  if test "$rc_homebrew"; then
    export HOMEBREW_PREFIX="$rc_homebrew";
    export HOMEBREW_CELLAR="$rc_homebrew/Cellar";
    export HOMEBREW_REPOSITORY="$rc_homebrew/Homebrew";
  fi
}

rc_ensure_ssh_agent() {
  local u_agent working_agent
  u_agent="$HOME/.ssh/u-sockets/agent.socket"
  working_agent=
  if test -S "$u_agent"; then
    # Check if the agent does work
    local x
    x=0
    SSH_AUTH_SOCK="$u_agent" ssh-add -l > /dev/null 2>&1 || x="$?"
    if test "$x" -lt 2; then
      # ssh-add exits with 1 if agent works but has no identities
      working_agent=1
    fi
  fi
  if ! test "$working_agent"; then
    rm -f "$u_agent"
    if test "$WSLENV"; then
      u-start-wsl-ssh-agent "$u_agent" && working_agent=1
    else
      ssh-agent -a "$u_agent" >/dev/null && working_agent=1 || \
        echo "Failed to start SSH agent"
    fi
  fi
  if test "$working_agent"; then
    export SSH_AUTH_SOCK="$u_agent"
  fi
}

rc_setup_env

### Interactive-only settings when PS1 is set
if ! test "${PS1+1}"; then
  return 0
fi

rc_history() {

  # for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
  HISTFILESIZE="$((500 * 1000))"
  HISTSIZE="$HISTFILESIZE"

  if rc_is_bash; then
    HISTFILE="$HOME/.bash_history"
    # don't put duplicate lines or lines starting with space in the history.
    # See bash(1) for more options
    HISTCONTROL=ignorespace:erasedups

    # append to the history file, don't overwrite it
    shopt -s histappend

    # Automatically save history after each prompt
    PROMPT_COMMAND='history -a'
  fi

  if rc_is_zsh; then
    HISTFILE="$HOME/.zsh_history"

    # Skip dups while searching, still keep them in the history file
    setopt HIST_FIND_NO_DUPS
    setopt HIST_IGNORE_ALL_DUPS

    setopt INC_APPEND_HISTORY
    setopt EXTENDED_HISTORY
    HISTTIMEFORMAT="[%F %T] "
  fi
}

rc_print_git_branch_for_prompt() {
  local branch
  # Filter rev-parse result to avoid surprises as this goes to the terminal
  branch="$(git rev-parse --abbrev-ref HEAD 2>/dev/null | tr -dc 'a-zA-Z0-9 .:%=/_-' || :)"
  test "$branch" || return 0
  printf ' (%s)' "$branch"
}

rc_prompt() {
  local s
  s=

  if rc_is_zsh; then
    setopt PROMPT_SUBST
    s="$s"'%B%F{green}%n@%m%f%b:%B%F{blue}%~%f%b'

    # Add Git branch
    s="$s"'%B%F{green}$(rc_print_git_branch_for_prompt)%f%b'

    s="$s"$'\n%B$%b '

    if ! : rc_is_mac; then
      s="$s"$'%{\e]0;%n@%m: %~\a%}'
    fi

    PS1="$s"
    return 0
  fi

  local color_prompt
  color_prompt=
  # set a fancy prompt (non-color, unless we know we "want" color)
  case "$TERM" in
  xterm-color|*-256color) color_prompt=1;;
  * )
    if test -x /usr/bin/tput && tput setaf 1 >/dev/null 2>&1; then
      # We have color support; assume it's compliant with Ecma-48
      # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
      # a case would tend to support setf rather than setaf.)
      color_prompt=1
    fi
  esac

  if test "$color_prompt"; then
    s="$s"'\[\e[01;32m\]$USER@\h\[\e[00m\]:\[\e[01;34m\]\w\[\e[00m\]'
  else
    s="$s"'$USER@\h:\w'
  fi

  # Add Git branch
  if test "$color_prompt"; then
    s="$s"'\[\e[01;32m\]'
  fi
  s="$s"'$(rc_print_git_branch_for_prompt)'
  if test "$color_prompt"; then
    s="$s"'\[\e[00m\]'
  fi

  s="$s"'\n\$ '

  # If this is an xterm set the title to user@host:dir
  case "$TERM" in
  xterm*|rxvt*)
    s='\[\e]0;$USER@\h: \w\a\]'"$s"
    ;;
  *) ;;
  esac

  PS1="$s"
}

rc_aliases() {
  # enable color support of ls and also add handy aliases
  if test -x /usr/bin/dircolors; then
    local s
    if test -r "$HOME/.dircolors"; then
      s="$(dircolors -b "$HOME/.dircolors")"
    else
      s="$(dircolors -b)"
    fi
    # Avoid eval, just extract the value between single quotes
    s="${s#*'}"
    LS_COLORS="${s%'*}"
    export LS_COLORS
  fi

  alias ls='ls --color=auto'
  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
}

rc_completion() {
  # enable programmable completion features (you don't need to enable
  # this, if it's already enabled in /etc/bash.bashrc and /etc/profile
  # sources /etc/bash.bashrc).
  if rc_is_bash && ! shopt -oq posix; then
    if test -f /usr/share/bash-completion/bash_completion; then
      . /usr/share/bash-completion/bash_completion
    elif test -f /etc/bash_completion; then
      . /etc/bash_completion
    fi
  fi
}

rc_misc() {
  if rc_is_bash; then
    # check the window size after each command and, if necessary,
    # update the values of LINES and COLUMNS.
    shopt -s checkwinsize
  fi

  # Disable blinking cursor in linux console
  test "$TERM" = linux && printf '\033[?17;0;64c'

  # Disable blinking cursor for Windows terminal
  test "$TERM" && test "$WSLENV" && printf '\033[2 q'
}

rc_history
! rc_is_msys && rc_prompt
rc_aliases
rc_completion
rc_misc

rc_has_setup=1

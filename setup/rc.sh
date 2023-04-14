
# Read Fedora-specific startup file. On Debian and Ubuntu bash is
# compiled to read system-wide files automatically.

test "${rc_has_setup-}" && return 0

rc_is_mac() {
  test "$rc_is_mac"
}

rc_is_bash() {
  test "${BASH_VERSION-}"
}

rc_is_zsh() {
  test "${ZSH_VERSION-}"
}

if rc_is_bash; then
  test -f /etc/bashrc && . /etc/bashrc
fi

rc_setup_env() {

  if rc_is_mac; then
    export LANG=en_US.UTF-8
  else
    export LANG=C.UTF-8
  fi
  #export LC_ALL C

  test -f "$rc_ahome/state/env" && . "$rc_ahome/state/env"
  if ! test "${AHOME_FORCED_PATH-}"; then
    export PATH="$rc_ahome_path"
  fi

  if ! test "${SSH_AUTH_SOCK-}"; then
    local u_agent working_agent
    test -d "$HOME/.ssh" || mkdir -m 0700 "$HOME/.ssh"
    u_agent="$HOME/.ssh/u-ssh-agent.socket"
    working_agent=
    if test -S "$u_agent"; then
      # Check if the agent does work
      local status
      status=0
      SSH_AUTH_SOCK="$u_agent" ssh-add -l > /dev/null 2>&1 || status="$?"
      if test "$status" -lt 2; then
	# ssh-add exits with 1 if agent works but has no identities
	working_agent=1
      fi
    fi
    if ! test "$working_agent"; then
      rm -f "$u_agent"
      ssh-agent -a "$u_agent" >/dev/null && working_agent=1 || \
	echo "Failed to start SSH agent"
    fi
    if test "$working_agent"; then
      export SSH_AUTH_SOCK="$u_agent"
    fi
  fi
}

rc_setup_env

### Interactive-only settings when PS1 is set
if ! test "${PS1+1}"; then
  return 0
fi

rc_history() {

  # for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
  HISTSIZE="$((100 * 1000))"
  HISTFILESIZE="$((1000 * 1000))"

  if rc_is_bash; then
    HISTFILE="$HOME/.bash_history"
    # don't put duplicate lines or lines starting with space in the history.
    # See bash(1) for more options
    HISTCONTROL=ignoreboth

    # append to the history file, don't overwrite it
    shopt -s histappend

  fi
  if rc_is_zsh; then
    HISTFILE="$HOME/.zsh_history"

    # Skip dups while searching, still keep them in the history file
    setopt HIST_FIND_NO_DUPS
    #setopt HIST_IGNORE_ALL_DUPS

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
    s="$s"'\[\e[01;32m\]\u@\h\[\e[00m\]:\[\e[01;34m\]\w\[\e[00m\]'
  else
    s="$s"'\u@\h:\w'
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
    s='\[\e]0;\u@\h: \w\a\]'"$s"
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
}

rc_history
rc_prompt
rc_aliases
rc_completion
rc_misc

rc_has_setup=1

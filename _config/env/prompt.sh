RESET="$(tput sgr0)"
RED="$(tput setaf 1)"
GREEN="$(tput setaf 2)"
YELLOW="$(tput setaf 3)"
BLUE="$(tput setaf 4)"
MAGENTA="$(tput setaf 5)"
CYAN="$(tput setaf 6)"
BOLD="$(tput bold)"

VI_COMMAND="$(tput setaf 154)" # Light green
VI_INSERT="$(tput setaf 39)"	# Cyan-ish

KEYTIMEOUT=1

STATUS_COLOR=$GREEN
VI_COLOR=$VI_INSERT

function __local_prompt {
  print "%{$RESET%}$(
    [ -f $HOME/local_prompt.sh ] && source $HOME/local_prompt.sh
  )"
}

function __user {
  if [ -n "$ZSH_VERSION" ]; then
    print "%{$RESET%}%n@%m"
  else
    echo '\u@\h'
  fi
}

function __path {
  if [ -n "$ZSH_VERSION" ]; then
    print "%{$YELLOW%}%~%{$RESET%}"
  else
    echo '\w'
  fi
}

function __git_prompt {
  if [ -d .git ] || git rev-parse --git-dir > /dev/null 2>&1; then
    branch="($(git symbolic-ref --short HEAD 2> /dev/null))"
    if [ $? -ne 0 ]; then
      branch=$(git branch | sed -n '/\* /s///p')
    fi

    print "%{$BLUE%}$branch%{$RESET%}"
  fi
}

function status_color {
  if [ $__exit_status -ne 0 ]; then
    print "%{$RED%}"
  else
    print "%{$GREEN%}"
  fi
}

if [ -n "$ZSH_VERSION" ]; then
  setopt prompt_subst

  # To simplify setting the vi prompt, we use some bash-incompatible syntax. So,
  # we just split that syntax into its own file which is never read by bash.
  source $XDG_CONFIG_HOME/zsh/prompt.zsh

  prompt_top=$'$(status_color)┌ %{$RESET%}$(__local_prompt)$(__user) $(__path) $(__git_prompt)\n'
  prompt_bot='$(status_color)└╌╌┄┄ %{$VI_COLOR%}❯❯ %{$RESET%}'
  PROMPT="$prompt_top$prompt_bot"
else
  :
  # PROMPT_COMMAND="__prompt"
fi

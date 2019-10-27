precmd() {
  export __exit_status=$?
  if [ $__exit_status -eq 148 ]; then
    export __exit_status=0
  fi

  export __time_stamp=$(date '+%Y.%m.%d %H:%M:%S')

  vcs_info
}

function zle-keymap-select {
  case $KEYMAP in
    vicmd)
        export VI_COLOR=$VI_COMMAND
        echo -ne "\e[2 q"
        ;;
    viins|main)
        export VI_COLOR=$VI_INSERT
        echo -ne "\e[6 q"
        ;;
   esac

   zle reset-prompt
}

RPROMPT=$'%{\e[1A%}%{\e[1C%}[$__time_stamp]%{\e[1B%}'

zle -N zle-keymap-select

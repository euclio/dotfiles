precmd() {
  export __exit_status=$?
  if [ $__exit_status -eq 148 ]; then
    export __exit_status=0
  fi

  export __time_stamp=$(date '+%Y.%m.%d %H:%M:%S')
}

function zle-keymap-select {
  case $KEYMAP in
    vicmd) export VI_COLOR=$VI_COMMAND;;
    viins|main) export VI_COLOR=$VI_INSERT;;
   esac

   zle reset-prompt
}

zle -N zle-keymap-select

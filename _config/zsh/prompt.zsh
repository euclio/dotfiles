precmd() {
  export __exit_status=$?
  if [ $__exit_status -eq 148 ]; then
    export __exit_status=0
  fi
}

function zle-keymap-select {
  case $KEYMAP in
    vicmd) export VI_COLOR=$VI_COMMAND;;
    viins|main) export VI_COLOR=$VI_INSERT;;
   esac

   zle reset-prompt
}

zle -N zle-keymap-select

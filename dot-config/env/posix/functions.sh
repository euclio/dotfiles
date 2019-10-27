#!/bin/sh

# SSH into a tmux session
ssht() {
    ssh $* -t 'tmux attach || tmux || /bin/bash'
}

# Send adb commands to all Android devices connected to the PC.
multiadb() {
    adb devices | tail -n +2 | cut -sf 1 | xargs -iX adb -s X ${@:1}
}

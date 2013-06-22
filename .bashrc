# Swap the caps lock and ctrl keys
xmodmap ~/.swapcapslock

# Allows bash aliases to be stored in a different file
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Use vi-style keybindings when in the terminal
set -o vi

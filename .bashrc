# Turn the caps lock key into a left control
xmodmap -e 'clear Lock' -e 'keycode 0x42 = Control_L'

# Allows bash aliases to be stored in a different file
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Use vi-style keybindings when in the terminal
set -o vi

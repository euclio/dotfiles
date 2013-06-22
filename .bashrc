# Swap the caps lock and ctrl keys

xmodmap ~/.swapcapslock

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

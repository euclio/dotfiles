# Turn the caps lock key into a left control
xmodmap -e 'clear Lock' -e 'keycode 0x42 = Control_L'

# Make default editor into vi (usually symlinked to vim)
export EDITOR=vi

# Use vi-style keybindings when in the terminal
set -o vi

# Enable autocomplete in Python interpreter
export PYTHONSTARTUP=~/.pythonrc

# Make terminal advertise 256 color support (from Ubuntu default .bash_profile)
if [ "$TERM" = "xterm" ] ; then
    if [ -z "$COLORTERM" ] ; then
        if [ -z "$XTERM_VERSION" ] ; then
            echo "Warning: Terminal wrongly calling iteself 'xterm'."
        else
            case "$XTERM_VERSION" in
            "XTerm(256)") TERM="xterm-256color" ;;
            "XTerm(88)") TERM="xterm-88color" ;;
            "XTerm") ;;
            *)
                echo "Warning: Unrecognized XTERM_VERSION: $XTERM_VERSION"
                ;;
            esac
        fi
    else
        case "$COLORTERM" in
            gnome-terminal)
                # Those crafty Gnome folks require you to check COLORTERM,
                # but don't allow you to just *favor* the setting over TERM.
                # Instead you need to compare it and perform some guesses
                # based upon the value. This is, perhaps, too simplistic.
                TERM="gnome-256color"
                ;;
            *)
                echo "Warning: Unrecognized COLORTERM: $COLORTERM"
                ;;
        esac
    fi
fi

# Start tmux on login if it exists and is not already running
if which tmux 2>&1 >/dev/null; then
  [[ -z "$TMUX" ]] && exec tmux
fi

# Allows bash aliases to be stored in a different file
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

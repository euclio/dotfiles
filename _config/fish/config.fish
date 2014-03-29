# Editor
set -x EDITOR /usr/bin/vim

# Use vim as manpager
set -x MANPAGER "/bin/sh -c \"col -b |                                  \
    vim -R +AirlineToggle                                               \
        -c 'set ft=man ts=8 nolist noru nonu nornu noma nosc tw=0 ls=0' \
        -\""

# TODO: activate vim mode when implemented

# Enable autocomplete in Python interpreter
set -x PYTHONSTARTUP ~/.pythonrc

# Add gem executables to PATH
set PATH $PATH (ruby -e 'puts Gem.user_dir')/bin

function ssht
    ssh $argv -t "tmux attach || tmux || /bin/bash"
end

function fish_greeting
    # No greeting
end

source ~/.aliases

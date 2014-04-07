# Editor
set -x EDITOR /usr/bin/vim

# Shell
set -x SHELL /usr/bin/fish

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

set VIRTUALFISH_HOME ~/repos
# Add virtualenvwrapper-style aliases to virtualfish
set VIRTUALFISH_COMPAT_ALIASES

source ~/.config/fish/colors.fish
source ~/.config/fish/scripts/virtualfish/virtual.fish
source ~/.aliases

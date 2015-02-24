source $XDG_CONFIG_HOME/fish/colors.fish

# Virtualfish
set -g VIRTUALFISH_COMPAT_ALIASES       # Add virtualenvwrapper-like commands
set -g VIRTUALFISH_HOME $XDG_DATA_HOME/virtualenvs
source $XDG_CONFIG_HOME/fish/scripts/virtualfish/virtual.fish
source $XDG_CONFIG_HOME/fish/scripts/virtualfish/auto_activation.fish

source $XDG_CONFIG_HOME/bash/aliases.sh

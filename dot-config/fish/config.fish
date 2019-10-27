source $XDG_CONFIG_HOME/fish/colors.fish

# Virtualfish
set -g VIRTUALFISH_HOME $XDG_DATA_HOME/virtualenvs
eval (python -m virtualfish auto_activation compat_aliases)

source $XDG_CONFIG_HOME/env/aliases.sh

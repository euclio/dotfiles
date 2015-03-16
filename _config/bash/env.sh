#!/bin/sh

# Make default editor into vim
export EDITOR=/usr/bin/vim

# Use xdg to determine the default browser
export BROWSER="/usr/bin/xdg-open http://"

# Use vim as the manpager
export MANPAGER="/bin/sh -c \"col -b |
    vim -R +AirlineToggle \
        -c 'set ft=man ts=8 nolist noru nonu nornu noma nosc tw=0 ls=0' -\""

# Enable autocomplete in Python interpreter
export PYTHONSTARTUP=$XDG_CONFIG_HOME/python/pythonrc

# Add ruby gem executables to PATH
export PATH="$(ruby -e 'puts Gem.user_dir')/bin:$PATH"

# Use antialiased fonts and GTK look and feel for Swing applications.
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
# Fix apperance of Swing applications in tiling window manager
export _JAVA_AWT_WM_NONREPARENTING=1

# Set vimrc's location to $XDG_CONFIG_HOME
export VIMINIT='let $MYVIMRC=$XDG_CONFIG_HOME . "/vim/vimrc" | source $MYVIMRC'
export GVIMINIT='let $MYGVIMRC=$XDG_CONFIG_HOME . "/vim/gvimrc" | source $MYGVIMRC'

# Increase SBT's cache size
export SBT_OPTS="-XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -Xms1g -Xmx1g"

# Store npmrc in config directory
export npm_config_userconfig=$XDG_CONFIG_HOME/nodejs/npmrc

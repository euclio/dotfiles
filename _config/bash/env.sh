#!/bin/sh

# Add local bin to PATH
export PATH="$HOME/.local/bin:$PATH"

# Make default editor into vim
export EDITOR=vim

# Use xdg to determine the default browser
export BROWSER="/usr/bin/xdg-open http://"

# Use vim as the manpager
export MANPAGER="/bin/sh -c \"col -b |
    vim -R +AirlineToggle \
        -c 'set ft=man ts=8 nolist noru nonu nornu noma nosc tw=0 ls=0' -\""

# Enable autocomplete in Python interpreter
export PYTHONSTARTUP=$XDG_CONFIG_HOME/python/pythonrc

# Add ruby gem executables to PATH
if type "ruby" >/dev/null 2>&1; then
  export PATH="$(ruby -e 'if defined?(Gem) then puts Gem.user_dir + "/bin:" end')$PATH"
fi

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

# Store ccache in cache dir
export CCACHE_DIR=$XDG_CACHE_HOME/ccache

# makepkg
# Store development package sources in data dir
export SRCDEST=$XDG_DATA_HOME/makepkg

# Compress using multiple threads
export COMPRESSXZ=(xz -T 0 -c -z -)

# Set Maven configuration directory
export M2_HOME="$XDG_CONFIG_HOME/m2"

if type "ag" >/dev/null 2>&1; then
  export FZF_DEFAULT_COMMAND='ag -l -g ""'
fi

# Perforce Settings
export P4DIFF='git --no-pager diff --no-index'
export P4MERGE='vimdiff'

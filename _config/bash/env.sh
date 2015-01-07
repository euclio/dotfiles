#!/bin/sh

# Make default editor into vim
export EDITOR=/usr/bin/vim

# Use vim as the manpager
export MANPAGER="/bin/sh -c \"col -b |
    vim -R +AirlineToggle \
        -c 'set ft=man ts=8 nolist noru nonu nornu noma nosc tw=0 ls=0' -\""

# Enable autocomplete in Python interpreter
export PYTHONSTARTUP=~/.pythonrc

# Add ruby gem executables to PATH
export PATH="$(ruby -e 'puts Gem.user_dir')/bin:$PATH"

# Use antialiased fonts and GTK look and feel for Swing applications.
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
# Fix apperance of Swing applications in tiling window manager
export _JAVA_AWT_WM_NONREPARENTING=1

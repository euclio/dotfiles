# Editor
set -x EDITOR /usr/bin/vim

# Shell
set -x SHELL /usr/bin/fish

# Use vim as manpager
set -x MANPAGER "/bin/sh -c \"col -b |                                  \
    vim -R +AirlineToggle                                               \
        -c 'set ft=man ts=8 nolist noru nonu nornu noma nosc tw=0 ls=0' \
        -\""

# Enable autocomplete in Python interpreter
set -x PYTHONSTARTUP ~/.pythonrc

# Add gem executables to PATH
set PATH $PATH (ruby -e 'puts Gem.user_dir')/bin /opt/android-sdk/tools /opt/android-sdk/platform-tools

set VIRTUALFISH_HOME ~/repos
# Add virtualenvwrapper-style aliases to virtualfish
set VIRTUALFISH_COMPAT_ALIASES

# Disable flow control. Instead pass Ctrl-S and Ctrl-Q through.
stty -ixon

# Use antialiased fonts and GTK look and feel for Swing applications.
set -x _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'

source ~/.config/fish/colors.fish
source ~/.config/fish/scripts/virtualfish/virtual.fish
source ~/.aliases

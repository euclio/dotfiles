#!/bin/sh

# Add local bin to PATH
export PATH="$HOME/.local/bin:$PATH"

# Make default editor into neovim, then fallback to vim, then fallback to vi.
__editor() {
  command -v nvim >/dev/null 2>&1 && { echo 'nvim'; return; }
  command -v vim >/dev/null 2>&1 && { echo 'vim'; return; }
  command -v vi >/dev/null 2>&1 && { echo 'vi'; return; }
}
editor="$(__editor)"
export EDITOR="$editor"

export BROWSER="firefox"

# Use vim as the manpager
export MANPAGER="/bin/sh -c \"col -b |
    vim -R +AirlineToggle \
        -c 'set ft=man ts=8 nolist noru nonu nornu noma nosc tw=0 ls=0' -\""

# Enable autocomplete in Python interpreter
export PYTHONSTARTUP=$XDG_CONFIG_HOME/python/pythonrc

# Add ruby gem executables to PATH
if command -v "ruby" >/dev/null 2>&1; then
  gem_dir=$(ruby -e 'if defined?(Gem) then puts Gem.user_dir + "/bin" end')
  export PATH="$gem_dir:$PATH"
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
export AURDEST=$XDG_DATA_HOME/aur

# Compress using multiple threads
export COMPRESSXZ=(xz -T 0 -c -z -)

# Set Maven configuration directory
export M2_HOME="$XDG_CONFIG_HOME/m2"

if command -v ag >/dev/null; then
  export FZF_DEFAULT_COMMAND='ag -g ""'
fi

# Perforce Settings
export P4DIFF='git --no-pager diff --no-index'
export P4MERGE='vimdiff'

if command -v rustup >/dev/null; then
  rust_sysroot=$(rustc --print sysroot)
  export RUST_SRC_PATH="$rust_sysroot/lib/rustlib/src/rust/src"
fi

# Add cargo binaries to PATH
export PATH="$HOME/.cargo/bin:$PATH"

# Shell-independent history settings
export HISTSIZE=500000
export HISTFILESIZE=$HISTSIZE
export SAVEHIST=$HISTSIZE

# Add additional environment variables to make applications obey the XDG
# directory specification.
# shellcheck source=/dev/null
source $XDG_CONFIG_HOME/env/xdg.sh

# Source local, shell-independent variables
# shellcheck source=/dev/null
[ -f "$HOME/.local/env" ] && source $HOME/.local/env

# Set the SSH authorization socket (set by the SSH-agent systemd unit).
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

# Set the TTY used for GPG authentication
export GPG_TTY=$(tty)

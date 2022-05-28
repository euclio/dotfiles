#!/bin/sh

# Colorize ls by default
if command -v exa >/dev/null 2>&1; then
  # If present, alias `ls` to `exa`.
  alias ls=exa
elif ls --color=auto >/dev/null 2>&1; then
  # For GNU ls
  alias ls='ls --color=auto'
else
  # For BSD ls
  export CLICOLOR=1
fi

# Chiptune music streams
alias kohina='mplayer -prefer-ipv4 -playlist http://anka.org:8080/fresh.ogg.m3u'
alias rainwave='mplayer -prefer-ipv4 -playlist http://chiptune.rainwave.cc/tune_in/4.ogg'

# Quick way to show irssi nicklist
alias nicklist='cat $XDG_CONFIG_HOME/irssi/nicklistfifo'

# Give words command history
alias words='rlwrap words'

# Remove orphan packages
alias pacro='sudo pacman -Qtdq | sudo pacman -Rns -'

# Update all packages, including development packages
alias pacu=yay

# Perforce
alias pshelved='p4 changes -u $USER -s shelved'

# Pass a cargo command to a docker image set up to build static Rust binaries:
#   $ rust-musl-builder cargo run --release
alias rust-musl-builder='docker run --rm -it -v "$(pwd)":/home/rust/src ekidd/rust-musl-builder'

# Live life on the edge
if command -v nvim >/dev/null 2>&1; then
  alias vim='nvim'
fi

# Workarounds to make programs obey the XDG base directory specification
alias irssi='irssi --home $XDG_CONFIG_HOME/irssi'
alias latexmk='latexmk -r $XDG_CONFIG_HOME/latexmk/latexmkrc'
alias sbt='sbt -Dsbt.global.base=$XDG_CONFIG_HOME/sbt -Dsbt.ivy.home=$XDG_CACHE_HOME/ivy'
alias tmux='tmux -f $XDG_CONFIG_HOME/tmux/tmux.conf'

# Alias xdg-open to open for consistency with MacOS.
if command -v xdg-open >/dev/null 2>&1; then
  alias open=xdg-open
fi

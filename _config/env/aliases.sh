#!/bin/sh

# Colorize ls by default
if ls --color=auto >/dev/null 2>&1; then
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
alias nicklist="cat $XDG_CONFIG_HOME/irssi/nicklistfifo"

# Give words command history
alias words='rlwrap words'

# Workarounds to make programs obey the XDG base directory specification
alias tmux="tmux -f $XDG_CONFIG_HOME/tmux/tmux.conf"
alias irssi="irssi --home $XDG_CONFIG_HOME/irssi"

# Update all packages, including development packages
alias pacu='pacaur -Syu --devel --needed --noedit'

# Put SBT's config in the config directory, and the ivy cache in the cache
alias sbt="sbt -Dsbt.global.base=$XDG_CONFIG_HOME/sbt -Dsbt.ivy.home=$XDG_CACHE_HOME/ivy"

# Put latexmk's config in the config directory
alias latexmk="latexmk -r $XDG_CONFIG_HOME/latexmk/latexmkrc"

# Perforce
alias pshelved="p4 changes -u $USER -s shelved"

# Live life on the edge
alias vim='nvim'

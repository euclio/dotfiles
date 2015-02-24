# Colorize ls by default
alias ls='ls --color=auto'

# Chiptune music stream
alias kohina='mplayer -prefer-ipv4 -playlist http://anka.org:8080/fresh.ogg.m3u'

# Quick way to show irssi nicklist
alias nicklist='cat $HOME/.irssi/nicklistfifo'

# Give words command history
alias words='rlwrap words'

# Make tmux obey the XDG directory specification
alias tmux="tmux -f $XDG_CONFIG_HOME/tmux/tmux.conf"

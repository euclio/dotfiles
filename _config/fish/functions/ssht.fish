function ssht
    ssh $argv -t "tmux attach || tmux || /bin/bash"
end

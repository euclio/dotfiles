function fish_user_key_bindings
    fish_vi_key_bindings

    # Simulate 'r' in insert mode
    bind -m replace-one r force-repaint
    bind -M replace-one -m default '' delete-char force-repaint self-insert
end

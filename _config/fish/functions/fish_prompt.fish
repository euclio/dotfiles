function fish_prompt
    # Save the value of the last status for later
    set last_status $status

    set n (set_color normal)             # Normal color
    set ve (set_color magenta)           # Virtualenv color
    set wd (set_color yellow)            # Working directory color
    set bl (set_color blue)              # Git prompt color

    if [ $last_status -eq 0 ]
        set st (set_color green --bold)      # Status success
    else
        set st (set_color red --bold)        # Status error code
    end

    # Remember the color to use for which vim mode we're in.
    # We use the same colors that my vim config uses.
    switch $fish_bind_mode
    case default
        set vi (set_color aeee00)
        set mode 'N'
    case insert
        set vi (set_color 0a9dff)
        set mode 'I'
    case visual
        set vi (set_color ffa724)
        set mode 'V'
    end

    printf "$st┌ $n"
    printf "%s@%s $wd%s$bl%s$n" (whoami) (hostname) (prompt_pwd) (__fish_git_prompt)
    if set -q VIRTUAL_ENV
        printf " $ve<%s>$n" (basename "$VIRTUAL_ENV")
    end
    echo
    printf "$st└╌╌┄┄ $vi❯❯$n " $mode
end

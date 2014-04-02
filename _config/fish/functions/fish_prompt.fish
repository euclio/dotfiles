function fish_prompt
    # Remember the color to use for the status
    if [ $status -eq 0 ]
        set status_color green --bold
    else
        set status_color red --bold
    end
    set_color $status_color; printf '┌ '
    set_color normal; printf '%s@%s' (whoami) (hostname)
    set_color normal; printf ':';
    set_color yellow; printf '%s' (prompt_pwd)
    if set -q VIRTUAL_ENV
        set_color -b blue white
        printf ' (%s)' (basename "$VIRTUAL_ENV")
        set_color normal;
    end
    echo
    set_color $status_color; printf '└╌╌┄┄ ❯❯ '
    set_color normal
end

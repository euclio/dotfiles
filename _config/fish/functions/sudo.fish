function sudo
    if test "$argv" = !!
        eval command sudo $history[2]
    else
        command sudo $argv
    end
end

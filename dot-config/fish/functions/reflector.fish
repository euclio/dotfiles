# Replace the pacman mirrors with the first 200 HTTP mirrors in the United
# States and sort them by download rate
function reflector
    if test (whoami) -eq "root"
        command reflector --verbose --country 'United States' -l 200 -p http --sort rate --save /etc/pacman.d/mirrorlist
    else
        echo "reflector must be run as root."
    end
end

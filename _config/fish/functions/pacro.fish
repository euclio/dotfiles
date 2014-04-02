# Ported from the Arch Wiki
# http://wiki.archlinux.org/index.php/pacman_Tips#Removing_orphaned_packages
function pacro
    if pacman -Qdt
        sudo pacman -Rns (pacman -Qdtq)
    else
        echo "No orphans to remove."
    end
end

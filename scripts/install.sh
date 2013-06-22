#!/usr/bin/env bash
#
# Simple script that symlinks dotfiles in the home directory to those in the
# dotfiles directory. This script assumes that it is 

# Dotfiles that should be managed by this script
declare -a dotfiles=('.vim' '.vim/.vimrc' '.vim/.gvimrc'
                     '.bashrc' '.swapcapslock')

cd `dirname $0`
dotfile_backup=${HOME}/dotfiles.old
script_dir=`pwd -P`
dotfile_dir=$(dirname ${script_dir})

mkdir -p ${dotfile_backup}
for file in ${dotfiles[@]}; do
    name=$(basename $file)
    file_link=${HOME}/${name}
    if [ -f ${file_link} -a ! -h ${file_link} ]; then
        # Back up the current dotfile
        mv ${file_link} ${dotfile_backup}
        echo "Backed up ${file_link}"
    fi
    file_abspath=${dotfile_dir}/${file}
    # Get the relative path to the dotfile, removing the initial '../'
    file_relpath=$(python -c "import os.path; \
                              print os.path.relpath('${file_abspath}', \
                                                    '${file_link}')[3:]")

    # Just overwrite whatever is there (it's not a file)
    cd ~
    rm ${file_link}
    ln -sfv ${file_relpath} ${file_link}
    echo "Linked ~/$(basename ${file_link}) to ${file_relpath}"
done

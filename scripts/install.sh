#!/usr/bin/env bash

# Dotfiles that should be managed by this script
declare -a dotfiles=('.vim' '.vim/.vimrc' '.vim/.gvimrc')

dotfile_backup=${HOME}/dotfiles.old
script_dir=$(dirname $0)
dotfile_dir=$(dirname $script_dir)

mkdir -p dotfile_backup
for file in ${dotfiles[@]}; do
    name=$(basename $file)
    if [ -e ${HOME}/${name} ]; then
        mv ${HOME}/${name} dotfile_backup
    fi
    ln -sf ${dotfile_dir}/$file ${HOME}/$name
done

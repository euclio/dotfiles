#!/usr/bin/env bash
#
# Simple script that symlinks dotfiles in the home directory to those in the
# dotfiles directory.

# Where existing dotfiles should be backed up
dotfile_backup=${HOME}/dotfiles.old

# The directory containing this script
script_dir=$(cd $(dirname $0); pwd -P)

# The directory containing the dotfiles
dotfile_dir=$(dirname ${script_dir})

function link_file () {
    name=$(basename $1)
    if [[ ${name} != .* ]]; then
        link_name=${HOME}/${name/_/.}
    else
        link_name=${HOME}/${name}
    fi

    # If the dotfile already exists, back it up
    if [ -f ${link_name} -a ! -h ${link_name} ]; then
        mkdir -p ${dotfile_backup}
        mv ${link_name} ${dotfile_backup}
        echo "Backed up ${link_name}"
    fi

    # Get the relative path to the dotfile, removing the initial '../'
    file_relpath=$(
        python -c "import os.path; \
            print os.path.relpath('$1', '${link_name}')[3:]")

    # Overwrite whatever is there (it's not a file)
    cd ~
    if [ -e ${link_name} ]; then
        rm ${link_name}
    fi
    ln -sf ${file_relpath} ${link_name}
    echo "Linked ~/$(basename ${link_name}) to ~/${file_relpath}"
}

# Match any file that has a leading underscore
dotfiles=(${dotfile_dir}/_*)

for file in ${dotfiles[@]}; do
    link_file ${file}
    if [ "${file}" == "${dotfile_dir}/_vim" ]; then
        # We need to link the vimrc and gvimrc
        link_file "${file}/.vimrc"
        link_file "${file}/.gvimrc"
    fi
done

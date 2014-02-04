#!/usr/bin/env python
"""Symlinks files in the home directory to their corresponding files in the
dotfiles directory."""

from __future__ import print_function

from glob import glob
from os.path import expanduser
import argparse
import os
import shutil
import sys
import subprocess


def link_file(filename):
    """Creates a symbolic link in the home directory to the given dotfile.

    Places any file that already exists in the file's location in the
    dotfile_backup folder.
    """

    dotfilename = os.path.basename(filename)
    if dotfilename[0] == '_':
        dotfilename = '.' + dotfilename[1:]

    # Add the 'dot' to the dotfile if there is none (fix for vimrc and gvimrc)
    if dotfilename[0] != '.':
        dotfilename = '.' + dotfilename

    link_name = os.path.join(os.path.realpath(expanduser('~')), dotfilename)

    # Remove existing symbolic links and back up any existing file or directory
    # at the desired link location
    if os.path.islink(link_name):
        if _ARGS.verbose:
            print('removing symbolic link at {}'.format(link_name))
        os.remove(link_name)
    elif os.path.isfile(link_name) or os.path.isdir(link_name):
        backup_succeeded = backup_file(link_name)
        if not backup_succeeded:
            print('did not link {}'.format(link_name), file=sys.stderr)
            return

    # Get the relative path to the actual dotfile
    file_relpath = os.path.join(
        os.path.relpath(os.path.dirname(filename), os.path.dirname(link_name)),
        os.path.basename(filename))

    if _ARGS.verbose:
        print('linking {} to {}'.format(link_name, file_relpath))
    os.symlink(file_relpath, link_name)


def backup_file(filename):
    """Backs up a file into the dotfile backup directory.

    Returns:
        A boolean indicating whether the backup succeeded or failed.
    """
    dotfile_backup = os.path.realpath(_ARGS.backup)
    if os.path.exists(dotfile_backup):
        if not os.path.isdir(dotfile_backup):
            raise ValueError('The specified backup directory already exists '
                             'but is not a directory.')
    else:
        if _ARGS.verbose:
            print('creating backup directory {}'.format(dotfile_backup))
        os.mkdir(dotfile_backup)
    backup_location = os.path.join(dotfile_backup, os.path.basename(filename))

    if _ARGS.verbose:
        print('backing up {} into {}'.format(filename, dotfile_backup))
    if os.path.exists(backup_location):
        if _ARGS.force:
            if os.path.isdir(backup_location):
                shutil.rmtree(backup_location)
            else:
                os.remove(backup_location)
        else:
            print('did not backup {}, {} exists'.format(
                filename, backup_location), file=sys.stderr)
            return False
    shutil.move(filename, dotfile_backup)
    return True


def main():
    """The main function of execution."""
    # The directory containing this script
    script_dir = os.path.dirname(os.path.realpath(__file__))

    # The directory containing the dotfiles
    dotfile_dir = os.path.dirname(script_dir)

    dotfiles = glob(os.path.join(dotfile_dir, '_*'))
    for dotfile in dotfiles:
        link_file(dotfile)

    # If the computer doesn't have Vim 7.4, we need to link the vimrc and gvimrc
    vim_version_output = (
            subprocess.check_output(['vim', '--version']).decode('utf-8'))
    # Split the string into lines, then examine the first line:
    #   VIM - Vi IMproved <VERSION>
    vim_version = float(vim_version_output.split('\n')[0].split(' ')[4])
    if vim_version < 7.4:
        link_file(os.path.join(dotfile_dir, '_vim', 'vimrc'))
        link_file(os.path.join(dotfile_dir, '_vim', 'gvimrc'))


if __name__ == '__main__':
    _PARSER = argparse.ArgumentParser(
        description='Create symbolic links from the home directory to the '
        'files in the dotfiles directory.')
    _PARSER.add_argument('-b', '--backup',
            default=os.path.join(expanduser(os.path.join('~', 'dotfiles.old'))),
            help='where to move any existing files that will be overwritten')
    _PARSER.add_argument('-v', '--verbose',
            action='store_true',
            help='enables verbose output')
    _PARSER.add_argument('-f', '--force',
            action='store_true',
            help='overwrites existing backups when moving dotfiles')
    _ARGS = _PARSER.parse_args()
    main()

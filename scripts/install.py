#!/usr/bin/env python
"""Symlinks files in the home directory to their corresponding files in the
dotfiles directory."""

from __future__ import print_function

from glob import glob
from os.path import expanduser
import argparse
import logging
import os
import platform
import subprocess

from dotfiles.files import link_file


def main():
    """The main function of execution."""
    log_format = "%(levelname)s: %(message)s"
    if _ARGS.verbose:
        logging.basicConfig(format=log_format, level=logging.DEBUG)
        logging.info("Verbose output.")
    else:
        logging.basicConfig(format=log_format)

    # The directory containing this script
    script_dir = os.path.dirname(os.path.realpath(__file__))

    # The directory containing the dotfiles
    dotfile_dir = os.path.dirname(script_dir)

    # Windows special casing
    if platform.system() == 'Windows':
        from dotfiles import windows
        windows.install(dotfile_dir, _ARGS.backup)

    dotfiles = glob(os.path.join(dotfile_dir, '_*'))
    for dotfile in dotfiles:
        link_file(dotfile, _ARGS.backup)

    # Check if the computer has Vim 7.4. If not, then we need to link the vimrc
    # and gvimrc.
    try:
        vim_version_output = (
            subprocess.check_output(['vim', '--version']).decode('utf-8'))
        # Split the string into lines, then examine the first line:
        #   VIM - Vi IMproved <VERSION>
        vim_version = vim_version_output.split('\n')[0].split(' ')[4]
        if vim_version < '7.4':
            vimrc = os.path.join(dotfile_dir, '_vim', 'vimrc')
            gvimrc = os.path.join(dotfile_dir, '_vim', 'gvimrc')
            link_file(vimrc, _ARGS.backup)
            link_file(gvimrc, _ARGS.backup)
    except OSError as err:
        if err.errno == os.errno.ENOENT:
            pass            # Vim isn't installed
        else:
            raise


if __name__ == '__main__':
    _PARSER = argparse.ArgumentParser(
        description='Create symbolic links from the home directory to the '
        'files in the dotfiles directory.')
    _PARSER.add_argument(
        '-b', '--backup',
        default=os.path.join(expanduser(os.path.join('~', 'dotfiles.old'))),
        help='where to move any existing files that will be overwritten')
    _PARSER.add_argument(
        '-v', '--verbose',
        action='store_true',
        help='enables verbose output')
    _PARSER.add_argument(
        '-f', '--force',
        action='store_true',
        help='overwrites existing backups when moving dotfiles')
    _PARSER.add_argument(
        '-d', '--dry_run',
        action='store_true',
        help='prints out the commands to be run without executing them')
    _ARGS = _PARSER.parse_args()
    main()

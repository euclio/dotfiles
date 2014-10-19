#!/usr/bin/env python
"""Symlinks files in the home directory to their corresponding files in the
dotfiles directory."""

from __future__ import print_function

from glob import glob
import argparse
import logging
import os
import platform

from dotfiles import files


def parse_arguments():
    """Generate command line arguments."""
    parser = argparse.ArgumentParser(
        description='Create symbolic links from the home directory to the '
        'files in the dotfiles directory.')
    parser.add_argument(
        '-b', '--backup',
        default=os.path.join(files.HOME_DIRECTORY, 'dotfiles.old'),
        help='where to move any existing files that will be overwritten')
    parser.add_argument(
        '-v', '--verbose',
        action='store_true',
        help='enables verbose output')
    parser.add_argument(
        '-f', '--force',
        action='store_true',
        help='overwrites existing backups when moving dotfiles')
    parser.add_argument(
        '-d', '--dry_run',
        action='store_true',
        help='prints out the commands to be run without executing them')
    parser.add_argument(
        '-c', '--check',
        action='store_true',
        help='checks whether dotfiles are installed or not.')
    return parser.parse_args()


def main():
    """The main function of execution."""
    args = parse_arguments()
    log_format = "%(levelname)s: %(message)s"
    if args.verbose:
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
        windows.install(dotfile_dir, args.backup, args.dry_run,
                        check_only=args.check)

    dotfiles = glob(os.path.join(dotfile_dir, '_*'))
    for dotfile in dotfiles:
        # Vim special casing
        if os.path.basename(dotfile) == '_vim':
            files.link_vim(dotfile, args.backup, args.dry_run)
            continue
        files.link_file(dotfile, args.backup, args.dry_run,
                        check_only=args.check)


if __name__ == '__main__':
    main()

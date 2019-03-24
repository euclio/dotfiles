#!/usr/bin/env python

"""
Links dotfiles and other configuration files from this directory to the proper
places on the filesystem.
"""

from glob import glob
import os
from os import path
import argparse
import errno
import logging
import platform
import shutil

HOME = path.realpath(path.expanduser('~'))
INSTALL_SCRIPT = path.realpath(__file__)
SCRIPT_DIR = path.join(path.dirname(INSTALL_SCRIPT), 'scripts')
DOTFILE_DIR = path.dirname(SCRIPT_DIR)


def make_link(file, link, backup_dir, dry_run):
    """
    Creates a symbolic link named "link" to "file".

    If a link already exists at the location, it is ignored. If a file already
    exists at the location, then it is copied over to the "backup" folder and a
    link is made in its place.
    """
    # It's nicer to read dotfiles that contain a relative path, but this
    # causes an issue on Windows if the files are on different drives.
    try:
        file = path.join(
            path.relpath(
                path.dirname(file),
                path.dirname(link),
            ),
            path.basename(file),
        )
    except ValueError:
        if platform.system() != 'Windows':
            raise

    if path.islink(link):
        if path.realpath(link) != file:
            logging.info('removing existing symlink %s', link)
            if not dry_run:
                os.remove(link)
        else:
            return
    elif path.exists(link):
        logging.info('backing up %s', file)
        if not dry_run:
            try:
                shutil.copytree(file, backup_dir)
            except OSError as ex:
                if ex.errno == errno.ENOTDIR:
                    shutil.copy(file, backup_dir)
                else:
                    raise

    logging.info('linking %s to %s', link, file)
    if not dry_run:
        os.symlink(file, link)


def link_dotfiles(backup_dir, dry_run):
    for dotfile in glob(path.join(DOTFILE_DIR, '_*')):
        link_name = '.' + path.basename(dotfile)[1:]
        make_link(dotfile, path.join(HOME, link_name), backup_dir, dry_run)


def link_scripts(backup_dir, dry_run):
    for script in glob(path.join(SCRIPT_DIR, '*')):
        if path.isdir(script) or script == INSTALL_SCRIPT:
            continue

        local_bin = path.expanduser('~/.local/bin')
        link = path.join(local_bin, path.basename(script))
        make_link(script, link, backup_dir, dry_run)


def install_win():
    raise Exception('additional installation for Windows is not implemented')


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '-d', '--dry-run',
        action='store_true',
        help='does not execute any filesystem operations'
    )
    args = parser.parse_args()

    if args.dry_run:
        logging.basicConfig(
            format='%(levelname)s: %(message)s', level=logging.DEBUG)

    backup_dir = path.join(HOME, 'dotfiles.old')

    link_dotfiles(backup_dir, args.dry_run)
    link_scripts(path.join(backup_dir, 'bin'), args.dry_run)

    if platform.system() == 'Windows':
        install_win()


if __name__ == '__main__':
    main()

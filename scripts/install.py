#!/usr/bin/env python
"""Symlinks files in the home directory to their corresponding files in the
dotfiles directory."""

from __future__ import print_function

from glob import glob
from os.path import expanduser
import argparse
import ctypes
import errno
import logging
import os
import platform
import tempfile
import shutil
import sys
import subprocess


def is_admin():
    """Returns whether the current user has administrator privileges on a
    Windows system.

    Raises: OSError: The current system is not Windows.
    """
    if platform.system() != 'Windows':
        raise OSError("The current system is not Windows: " +
                platform.system())
    return ctypes.windll.shell32.IsUserAnAdmin() != 0


def _get_folder_path(csidl_value):
    import ctypes.wintypes
    SHGFP_TYPE_CURRENT = 0  # Current, not default value

    path_buffer = ctypes.create_unicode_buffer(ctypes.wintypes.MAX_PATH)
    ctypes.windll.shell32.SHGetFolderPathW(0, csidl_value, 0,
            SHGFP_TYPE_CURRENT, path_buffer)
    return os.path.abspath(path_buffer.value)


def my_documents_location():
    """Returns the location of the current user's "My Documents" folder."""
    CSIDL_PERSONAL = 5      # My Documents
    return _get_folder_path(CSIDL_PERSONAL)


def startup_location():
    """Returns the location of the current user's "Startup" folder."""
    CSIDL_STARTUP = 7       # Startup
    return _get_folder_path(CSIDL_STARTUP)


def _execute(function, *args):
    """Invokes function with *args if dry_run is False. Otherwise, logs the
    invocation."""
    if _ARGS.dry_run:
        logging.info('DRY RUN: %s %s', function.__name__, args)
    else:
        function(*args)


def delete_file_or_directory(filename):
    if os.path.isdir(filename):
        _execute(shutil.rmtree, filename)
    else:
        _execute(os.remove, filename)


def move_file_or_directory(filename):
    _execute(shutil.move, filename, dotfile_backup)


def create_symbolic_link(file_path, link_name):
    _execute(os.symlink, file_path, link_name)


def create_directory(directory):
    _execute(os.mkdir, directory)


def link_file(filename, destination=None, add_dot=True):
    """Creates a symbolic link in the home directory to the given dotfile.

    Places any file that already exists in the file's location in the
    dotfile_backup folder.

    Arguments:
        filename - The file that the link should point to.
        destination - Where the link should be located. Defaults to the user's
            home directory.
        add_dot - Whether the link should have a "dot" prepended to its name.
            This is useful for files (especially those on Windows) that are not
            "dotfiles" but are still considered part of the configuration.
    """

    # If no destination is specified, back up into the home directory.
    if destination is None:
        destination = os.path.realpath(os.path.expanduser('~'))

    dotfilename = os.path.basename(filename)
    if dotfilename[0] == '_':
        dotfilename = '.' + dotfilename[1:]

    # Add the 'dot' to the dotfile if there is none (fix for vimrc and gvimrc)
    if add_dot and dotfilename[0] != '.':
        dotfilename = '.' + dotfilename

    link_name = os.path.join(destination, dotfilename)

    # Get the relative path to the actual dotfile
    file_relpath = os.path.join(
        os.path.relpath(os.path.dirname(filename), os.path.dirname(link_name)),
        os.path.basename(filename))

    logging.info('linking %s to %s', link_name, file_relpath)

    # Remove existing symbolic links and back up any existing file or directory
    # at the desired link location
    if os.path.islink(link_name):
        logging.info('removing symbolic link at %s', link_name)
        delete_file_or_directory(link_name)
    elif os.path.isfile(link_name):
        try:
            backup_file(link_name)
        except OSError as e:
            if e.errno == errno.EEXIST:
                logging.warning('Could not backup %s, file exists in %s',
                        link_name, _ARGS.backup)
                logging.warning('Did not link %s', link_name)
            else:
                raise

        create_symbolic_link(file_relpath, link_name)
    elif os.path.isdir(link_name):
        # In this case (such as .config), we should move all the contents into
        # a temporary directory, create the link, and then move the contents
        # back in.
        tmp_dir = tempfile.mkdtemp()
        files = os.listdir(link_name)
        for filename in files:
            move_file_or_directory(filename, tmp_dir)
        delete_file_or_directory(link_name)
        create_symbolic_link(file_relpath, link_name)
        delete_file_or_directory(tmp_dir)


def backup_file(filename):
    """Backs up a file into the dotfile backup directory.

    Raises:
        FileExistsError if there is already a file with the same name in the
        backup directory.
    """
    dotfile_backup = os.path.realpath(_ARGS.backup)
    if os.path.exists(dotfile_backup):
        if not os.path.isdir(dotfile_backup):
            raise ValueError('The specified backup directory already exists '
                             'but is not a directory.')
    else:
        logging.info('creating backup directory %s', dotfile_backup)
        create_directory(dotfile_backup)

    backup_location = os.path.join(dotfile_backup, os.path.basename(filename))

    logging.info('backing up %s into %s', filename, dotfile_backup)
    if _ARGS.force and os.path.exists(backup_location):
        remove_file(backup_location)
    move_file_or_directory(filename, dotfile_backup)


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
        # Ensure the script is run as Administrator
        if not is_admin():
            logging.error('This script must be executed as an administrator.')
            sys.exit(1)

        # AutoHotKey Setup
        default_script_path = os.path.join(
                dotfile_dir, 'win', 'AutoHotkey.ahk')

        # The main autohotkey script must reside in My Documents.
        link_file(default_script_path, destination=my_documents_location(),
                add_dot=False)

        # We want the main script to start at startup, so we also link it to
        # the startup folder.
        link_file(default_script_path, destination=startup_location(),
                add_dot=False)

    dotfiles = glob(os.path.join(dotfile_dir, '_*'))
    for dotfile in dotfiles:
        link_file(dotfile)

    # Check if the computer has Vim 7.4. If not, then we need to link the vimrc
    # and gvimrc.
    try:
        vim_version_output = (
                subprocess.check_output(['vim', '--version']).decode('utf-8'))
        # Split the string into lines, then examine the first line:
        #   VIM - Vi IMproved <VERSION>
        vim_version = vim_version_output.split('\n')[0].split(' ')[4]
        if vim_version < '7.4':
            link_file(os.path.join(dotfile_dir, '_vim', 'vimrc'))
            link_file(os.path.join(dotfile_dir, '_vim', 'gvimrc'))
    except OSError as err:
        if err.errno == os.errno.ENOENT:
            pass            # Vim isn't installed
        else:
            raise


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
    _PARSER.add_argument('-d', '--dry_run',
            action='store_true',
            help='prints out the commands to be run without executing them')
    _ARGS = _PARSER.parse_args()
    main()

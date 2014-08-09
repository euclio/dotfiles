"""Windows-specific installation functions."""

import ctypes.wintypes
import logging
import os
import platform
import sys

from .files import link_file

SHGFP_TYPE_CURRENT = 0  # Current, not default value

CSIDL_PERSONAL = 5      # My Documents
CSIDL_STARTUP = 7       # Startup


def install(dotfile_dir, backup_dir):
    """Handle installation of Windows-specific dotfiles."""
    # Check for Python 3
    if not sys.version_info.major >= 3:
        logging.error('Due to how Python 2 handles symlinks on Windows, this'
                      ' script must be executed under Python 3.')
        sys.exit(1)

    # Ensure the script is run as Administrator
    if not is_admin():
        logging.error('This script must be executed as an administrator.')
        sys.exit(1)

    # AutoHotKey Setup
    ahk_path = os.path.join(dotfile_dir, 'win', 'AutoHotkey.ahk')
    default_ahk_path = os.path.join(my_documents_location(),
                                    os.path.basename(ahk_path))
    startup_ahk_path = os.path.join(startup_location(),
                                    os.path.basename(ahk_path))

    # The main autohotkey script must reside in My Documents.
    link_file(ahk_path, backup_dir, link_name=default_ahk_path)

    # We want the main script to start at startup, so we also link it to
    # the startup folder.
    link_file(ahk_path, backup_dir, link_name=startup_ahk_path)


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
    """Returns the absolute path to the folder specified by the given CSIDL
    value."""
    path_buffer = ctypes.create_unicode_buffer(ctypes.wintypes.MAX_PATH)
    ctypes.windll.shell32.SHGetFolderPathW(
        0, csidl_value, 0, SHGFP_TYPE_CURRENT, path_buffer)
    return os.path.abspath(path_buffer.value)


def my_documents_location():
    """Returns the location of the current user's "My Documents" folder."""
    return _get_folder_path(CSIDL_PERSONAL)


def startup_location():
    """Returns the location of the current user's "Startup" folder."""
    return _get_folder_path(CSIDL_STARTUP)

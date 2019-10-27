"""
Installs dotfiles on Windows.

This script has some special handling for placing the symlinks for AutoHotkey

It also cherry-picks specific dotfiles to install, since most of them aren't
very useful on a non-Unix platform.
"""

from ctypes import windll
from pathlib import Path
import argparse
import ctypes
import ctypes.wintypes
import logging
import platform
import shutil

SCRIPT_DIR = Path(__file__).parent
DOTFILE_DIR = SCRIPT_DIR


def mklink(src, dest):
    logging.info('linking %s to %s', src, dest)

    if src.is_dir() and not src.is_symlink():
        shutil.rmtree(src)
    else:
        src.unlink(missing_ok=True)

    src.symlink_to(dest, target_is_directory=dest.is_dir())


def install_ahk():
    # Use the current value
    SHGFP_TYPE_CURRENT = 0

    # "My Documents"
    CSIDL_PERSONAL = 5

    # "Startup"
    CSIDL_STARTUP = 7

    ahk_path = DOTFILE_DIR / 'win' / 'AutoHotkey.ahk'

    def get_csidl_path(csidl_value):
        path_buf = ctypes.create_unicode_buffer(ctypes.wintypes.MAX_PATH)
        windll.shell32.SHGetFolderPathW(
            0, csidl_value, 0, SHGFP_TYPE_CURRENT, path_buf)
        return Path(path_buf.value)

    default_ahk_path = get_csidl_path(CSIDL_PERSONAL) / 'AutoHotkey.ahk'
    startup_ahk_path = get_csidl_path(CSIDL_STARTUP) / 'AutoHotkey.ahk'

    mklink(default_ahk_path, ahk_path)
    mklink(startup_ahk_path, ahk_path)


def link_dotfile(dotfile):
    name = '.' + dotfile.name[4:]
    link = Path.home() / name
    mklink(link, dotfile)


def valid_log_level(value):
    if getattr(logging, value.upper(), None) is None:
        raise argparse.ArgumentTypeError(f'{value} is an invalid log level')
    return value.upper()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-l', '--log', type=valid_log_level)
    args = parser.parse_args()

    if args.log:
        logging.basicConfig(level=args.log)

    if platform.system() != 'Windows':
        raise EnvironmentError('This script must be run on Windows')

    install_ahk()
    link_dotfile(DOTFILE_DIR / 'dot-config')
    link_dotfile(DOTFILE_DIR / 'dot-ideavimrc')


if __name__ == '__main__':
    main()

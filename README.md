# dotfiles

My obligatory configuration repository.

## Installation

Execute `scripts/install.py`. This script will automatically set up symbolic
links from the home directory to the actual files in this repository. If there
are any conflicts (e.g., there is a file where the script wants to create a
symlink), it will back up the file to a directory (default dotfiles.old). For
more information, see `scripts/install.py --help`.

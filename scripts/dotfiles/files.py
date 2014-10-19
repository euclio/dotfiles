"""File operations for installing dotfiles."""
import errno
import logging
import os
import platform
import shutil
import stat
import subprocess
import tempfile

HOME_DIRECTORY = os.path.realpath(os.path.expanduser('~'))


def delete_file_or_directory(filename, dry_run):
    """Deletes a file or directory filename."""
    if os.path.isdir(filename) and not os.path.islink(filename):
        try:
            _execute(shutil.rmtree, filename, dry_run=dry_run)
        except WindowsError:
            # On Windows, files sometimes fail to delete properly if they are
            # marked readonly.
            os.chmod(filename,
                     stat.S_IRWXU | stat.S_IRWXG | stat.S_IRWXO)  # 0777
            _execute(shutil.rmtree, filename, dry_run=dry_run)

    else:
        _execute(os.remove, filename, dry_run=dry_run)


def move_file_or_directory(filename, destination, dry_run):
    """Move a file or directory filename to the destination."""
    _execute(shutil.move, filename, destination, dry_run=dry_run)


def create_symbolic_link(file_path, link_name, dry_run):
    """Create a symbolic link at link_name that points to file_path."""
    _execute(os.symlink, file_path, link_name, dry_run=dry_run)


def create_directory(directory, dry_run):
    """Create a directory at the given path."""
    _execute(os.mkdir, directory, dry_run=dry_run)


def _execute(function, *args, dry_run):
    """Invokes function with *args if dry_run is False. Otherwise, logs the
    invocation."""
    qualified_name = '{}.{}'.format(function.__module__, function.__name__)
    logging.info('Executing %s(%s)', qualified_name, ', '.join(args))
    if not dry_run:
        function(*args)


def backup_file(filename, backup_dir, dry_run, force=False):
    """Backs up a file into the dotfile backup directory.

    Raises:
        FileExistsError if there is already a file with the same name in the
        backup directory.
    """
    dotfile_backup = os.path.realpath(backup_dir)
    if os.path.exists(dotfile_backup):
        if not os.path.isdir(dotfile_backup):
            logging.error('The specified backup directory %s already exists'
                          ' and is not a directory. Aborting.')
            raise ValueError
    else:
        logging.info('Creating backup directory at "%s"', dotfile_backup)
        create_directory(dotfile_backup, dry_run)

    backup_location = os.path.join(dotfile_backup, os.path.basename(filename))

    logging.info('Backing up "%s" into "%s"...', filename, dotfile_backup)
    if force and os.path.exists(backup_location):
        delete_file_or_directory(backup_location, dry_run)
    move_file_or_directory(filename, dotfile_backup, dry_run)


def link_vim(vim_folder, backup_dir, dry_run):
    """Special casing for linking Vim's configuration."""
    if platform.system() == 'Windows':
        dotvim = os.path.join(HOME_DIRECTORY, 'vimfiles')
    else:
        dotvim = os.path.join(HOME_DIRECTORY, '.vim')

    link_file(vim_folder, backup_dir, dry_run, link_name=dotvim)

    # Check if the computer has Vim 7.4.
    # If not, then we need to link the vimrc and gvimrc.
    try:
        vim_version_output = (
            subprocess.check_output(['vim', '--version']).decode('utf-8'))
        # Split the string into lines, then examine the first line:
        #   VIM - Vi IMproved <VERSION>
        vim_version = vim_version_output.split('\n')[0].split(' ')[4]
        if vim_version < '7.4':
            link_file(os.path.join(vim_folder, 'vimrc'), dry_run, backup_dir)
            link_file(os.path.join(vim_folder, 'gvimrc'), dry_run, backup_dir)
    except OSError as err:
        if err.errno == os.errno.ENOENT:
            pass            # Vim isn't installed
        else:
            raise


def link_file(filename, backup_dir, dry_run=False, check_only=False,
              link_name=None):
    """
    Creates a symbolic link in the home directory to the given dotfile.

    Arguments:
        filename - The absolute path of the file that the link should point to.
        backup_dir - The absolute path of a folder. If a file exists where the
        symbolic link will be created, the file at that location will be moved
        to this folder to back it up.
        link_name - If specified, the symbolic link will be created with this
            name. Otherwise, the link name will be determined by the following
            process:

            1. Get the basename of the file to be linked to.
            2. If the basename has a leading underscore, it will be replaced by
               a dot. Otherwise, a dot will be prepended.
            3. Append the basename to the user's home directory.

            This is useful for files (particularly those on Windows) that are
            not "dotfiles" but are still considered part of the configuration.
    """
    # If no name is specified, create the link name by replacing the leading
    # underscore with a dot, and place the file in the home directory.
    if link_name is None:
        dotfile_name = os.path.basename(filename)
        if dotfile_name.startswith('_'):
            dotfile_name = '.' + dotfile_name[1:]
        link_name = os.path.join(HOME_DIRECTORY, dotfile_name)

    if check_only:
        if not os.path.islink(link_name):
            logging.warning('Dotfile %s not installed.',
                            os.path.basename(link_name))
        return

    # Get the path that the dotfile should point to.  We attempt to get a
    # relative path from the name of the dotfile to the actual file.  On
    # Windows, if the files are on different drives, we use the full path to
    # the file instead.
    try:
        dotfile_path = os.path.join(
            os.path.relpath(os.path.dirname(filename),
                            os.path.dirname(link_name)),
            os.path.basename(filename))
    except ValueError:
        if platform.system() == 'Windows':
            # On Windows, the paths may be on different drives. We need to use
            # the full path instead.
            dotfile_path = filename
        else:
            raise

    # Remove existing symbolic links and back up any existing file or directory
    # at the desired link location
    if os.path.islink(link_name):
        logging.info('Removing symbolic link "%s"', link_name)
        delete_file_or_directory(link_name, dry_run)
    elif os.path.isfile(link_name):
        logging.info('"%s" is a file, attempting to back up to "%s"',
                     link_name, backup_dir)
        try:
            backup_file(link_name, backup_dir, dry_run)
        except OSError as exc:
            if exc.errno == errno.EEXIST:
                logging.warning('Could not backup "%s", file exists in "%s"',
                                link_name, backup_dir)
                logging.warning('Did not link "%s"', link_name)
            else:
                raise
    elif os.path.isdir(link_name):
        logging.info('"%s" is a directory, attempting to back up to "%s"',
                     link_name, backup_dir)
        # In this case (such as .config), we should move all the contents into
        # a temporary directory, create the link, and then move the contents
        # back in.
        tmp_dir = tempfile.mkdtemp()
        files = [os.path.join(link_name, basename)
                 for basename in os.listdir(link_name)]
        for filename in files:
            move_file_or_directory(filename, tmp_dir, dry_run)
        delete_file_or_directory(link_name, dry_run)
        create_symbolic_link(dotfile_path, link_name, dry_run)
        delete_file_or_directory(tmp_dir, dry_run)
        return

    logging.info('Linking "%s" -> "%s"', link_name, dotfile_path)
    create_symbolic_link(dotfile_path, link_name, dry_run)

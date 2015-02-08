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


def _execute(function, *args, **kwargs):
    """Invokes function with *args if dry_run is False. Otherwise, logs the
    invocation."""
    qualified_name = '{}.{}'.format(function.__module__, function.__name__)
    logging.info('Executing %s(%s)', qualified_name, ', '.join(args))
    if not kwargs['dry_run']:
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


def get_link_target(link_path, dotfile_path):
    """Returns the path that should be contained in the link to the dotfile.

    In general, the path will be a relative path from the path of the link to
    the path of the dotfile. On Windows, if the files are on different drives,
    the full path to the dotfile is used instead.
    """
    try:

        return os.path.join(
            os.path.relpath(os.path.dirname(dotfile_path),
                            os.path.dirname(link_path)),
            os.path.basename(dotfile_path))
    except ValueError:
        if platform.system() == 'Windows':
            # On Windows, the paths may be on different drives. We need to use
            # the full path instead.
            return dotfile_path
        else:
            raise


def get_link_path(dotfile_path):
    """Returns the absolute path to where the dotfile should be linked.

    This is accomplished by replacing the leading underscore of the file with a
    dot, and then appending that filename to the home directory.

    If the filename does not start with an underscore, a dot is prepended.
    """
    dotfile_name = os.path.basename(dotfile_path)
    if dotfile_name.startswith('_'):
        link_name = '.' + dotfile_name[1:]
    else:
        link_name = '.' + dotfile_name
    return os.path.join(HOME_DIRECTORY, link_name)


def backup(filename, link_target, backup_dir, dry_run):
    """Backs up a file by moving it into a specified directory. If the file
    already exists in the directory, the backup is aborted.
    """
    if os.path.isfile(filename):
        logging.info('"%s" is a file, attempting to back up to "%s"',
                     filename, backup_dir)
        try:
            backup_file(filename, backup_dir, dry_run)
        except OSError as exc:
            if exc.errno == errno.EEXIST:
                logging.warning('Could not backup "%s", file exists in "%s"',
                                filename, backup_dir)
            raise
    elif os.path.isdir(filename):
        logging.info('"%s" is a directory, attempting to back up to "%s"',
                     filename, backup_dir)
        # In this case (such as .config), we should move all the contents into
        # a temporary directory, create the link, and then move the contents
        # back in.
        tmp_dir = tempfile.mkdtemp()
        files_to_move = [os.path.join(filename, basename)
                         for basename in os.listdir(filename)]
        for file_to_move in files_to_move:
            move_file_or_directory(file_to_move, tmp_dir, dry_run)
        delete_file_or_directory(filename, dry_run)
        create_symbolic_link(link_target, filename, dry_run)
        delete_file_or_directory(tmp_dir, dry_run)
        return


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
        link_name = get_link_path(filename)

    if check_only:
        if not os.path.islink(link_name):
            logging.warning('Dotfile %s not installed.',
                            os.path.basename(link_name))
        return

    link_target = get_link_target(link_name, filename)

    # Remove existing symbolic links and back up any existing file or directory
    # at the desired link location
    if os.path.islink(link_name):
        logging.info('Removing symbolic link "%s"', link_name)
        delete_file_or_directory(link_name, dry_run)
    else:
        try:
            backup(link_name, link_target, backup_dir, dry_run)
        except OSError:
            logging.info("Backup failed for %s. Skipping.", link_name)
            return

    logging.info('Linking "%s" -> "%s"', link_name, link_target)
    create_symbolic_link(link_target, link_name, dry_run)

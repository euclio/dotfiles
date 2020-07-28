# dotfiles

My obligatory configuration repository.

## Installation

### Unix

Requires [GNU stow].

```sh
$ cd
$ git clone --recurse-submodules https://github.com/euclio/dotfiles
$ ~/dotfiles/install.sh
```

### Windows

Requires [Python 3] and [Developer mode][win-dev-mode].

```powershell
> cd ~
> git clone --recurse-submodules https://github.com/euclio/dotfiles
> python3 dotfiles/install.py
```

[GNU stow]: https://www.gnu.org/software/stow/
[Python 3]: https://www.python.org
[win-dev-mode]: https://docs.microsoft.com/en-us/windows/uwp/get-started/enable-your-device-for-development#accessing-settings-for-developers

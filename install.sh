#!/bin/sh

set -eux

package=$(dirname "$0")

stow -v --dotfiles -d "$package" .
stow -v -d "$package" -t ~/.local/bin scripts

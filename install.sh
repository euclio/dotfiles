#!/bin/sh

set -eux

package=$(dirname "$0")

stow -v --dotfiles -d "$package" .
mkdir -p ~/.local/bin
stow -v -d "$package" -t ~/.local/bin scripts

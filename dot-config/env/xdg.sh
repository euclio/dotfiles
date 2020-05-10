#!/bin/sh

export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"
export LESSHISTFILE="$XDG_DATA_HOME/less/history"
export PIP_LOG_FILE="$XDG_DATA_HOME/pip/log"
export PYLINTHOME="$XDG_DATA_HOME/pylint"
export PYLINTRC="$XDG_CONFIG_HOME/pylint/pylintrc"
export PYTHON_EGG_CACHE="$XDG_CACHE_HOME/python/egg-cache"
export XMONAD_CONFIG_DIR="$XDG_CONFIG_HOME/xmonad"
export XMONAD_CACHE_HOME="$XDG_CACHE_HOME/xmonad"
export XMONAD_DATA_HOME="$XDG_DATA_HOME/xmonad"

# xmonad will not create these directories on its own
mkdir -p "$XMONAD_CACHE_HOME"
mkdir -p "$XMONAD_DATA_HOME"

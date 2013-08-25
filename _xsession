#!/bin/sh

if [ -f $HOME/.Xdefaults ]; then
    xrdb -merge $HOME/.Xdefaults
fi

if [ -e /usr/bin/openbox-session ]; then
    exec /usr/bin/openbox-session
fi

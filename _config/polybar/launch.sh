#!/usr/bin/env sh

# Terminate all bars already running
killall -q polybar
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

polybar main &

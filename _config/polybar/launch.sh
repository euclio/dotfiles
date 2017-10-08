#!/usr/bin/env bash

# Terminate all bars already running
killall -q polybar
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Start a bar for each monitor
for monitor in $(xrandr -q | grep ' connected' | cut -d ' ' -f1); do
  MONITOR=${monitor} polybar main &
done

#!/bin/bash
if type wl-copy >/dev/null; then
  wl-copy
elif type xclip >/dev/null; then
  xclip -in -selection clipboard
elif type pbcopy >/dev/null; then
  pbcopy
elif type lemonade >/dev/null; then
  lemonade copy
fi

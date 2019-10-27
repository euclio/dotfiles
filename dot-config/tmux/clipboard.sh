#!/bin/bash

if type xclip >/dev/null; then
  xclip -in -selection clipboard
elif type pbcopy >/dev/null; then
  pbcopy
elif type lemonade >/dev/null; then
  lemonade copy
fi

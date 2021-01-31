#!/bin/sh

# Wayland-specific environment configuration.

# Replace CapsLock with Ctrl.
export XKB_DEFAULT_OPTIONS="ctrl:nocaps"

# Enable Wayland support in Firefox.
export MOZ_ENABLE_WAYLAND=1

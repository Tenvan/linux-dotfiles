#!/usr/bin/env bash
set -x
export DOT_XSESSION="initialised"

# Load custom resources
file="$CUSTOMS/.Xresources"
if [ -f "$file" ]; then
    xrdb -merge "$file"
fi

# custom session
file="$HOME/.xsession-custom"
if [ -f "$file" ]; then
    . "$file"
fi

echo "do run autostarts"
$HOME/.screenlayout/screenlayout.sh
$HOME/.bin/autostart

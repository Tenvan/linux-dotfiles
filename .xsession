#!/usr/bin/env bash
. ~/.scripts/defs

export DOT_XSESSION="initialised"

# Load custom resources
file="$CUSTOMS/.Xresources"
if [ -f "$file" ]; then
    xrdb -merge "$file"
fi

echo "do run autostarts"
$HOME/.screenlayout/screenlayout.sh
$HOME/.bin/autostart

csource "$CUSTOMS/${0##*/}"

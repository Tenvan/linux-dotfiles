#!/usr/bin/env bash
export DOT_XSESSION="initialised"

#
# ~/.xsession
#
# executed from lightdm -> XSession
#
# custom resources
echo 'Running ~/.xsession'

# Load custom resources
file="$HOME/.Xresources-custom"
if [ -f "$file" ]; then
	echo "merge $file"
    xrdb -merge "$file"
fi

echo "do run autostarts"
$HOME/.screenlayout/screenlayout.sh
$HOME/.scripts/autostart-global.sh

# custom session
file="$HOME/.xsession-custom"
if [ -f "$file" ]; then
    . "$file"
fi

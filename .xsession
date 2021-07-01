#!/usr/bin/env bash
set -x

#
# ~/.xsession
#
# executed from lightdm -> XSession
#
# custom resources
echo 'Running ~/.xsession'

# Load resources
file="$HOME/.Xresources-custom"
if [ -f "$file" ]; then
	echo "merge $file"
    xrdb -merge "$file"
fi

# custom resources
echo "do run autostarts"
$HOME/.scripts/autostart-global.sh

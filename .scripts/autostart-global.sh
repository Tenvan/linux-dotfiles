#!/usr/bin/env bash

. ~/.scripts/defs.zsh

~/.scripts/set-wallpaper.sh

#starting utility applications at boot time
run nm-applet

#run pamac-tray
run radiotray-ng
run kteatime

# Sound Tray Icons
# restart start-pulseaudio-x11
run pasystray

run xbindkeys
run copyq

run albert

CUSTOM_AUTOSTART="$HOME/.autostart-custom"
if [ -f $CUSTOM_AUTOSTART ]; then 
	sh $CUSTOM_AUTOSTART
fi

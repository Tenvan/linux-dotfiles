#!/usr/bin/env bash

. ~/.scripts/defs.zsh

sh ~/.scripts/set-wallpaper.sh

#starting utility applications at boot time
run nm-applet
#run pamac-tray
run radiotray-ng
run kteatime

# killall xbindkeys
restart xbindkeys

run copyq

CUSTOM_AUTOSTART="$HOME/.autostart-custom"
if [ -f $CUSTOM_AUTOSTART ]; then 
	sh $CUSTOM_AUTOSTART
fi

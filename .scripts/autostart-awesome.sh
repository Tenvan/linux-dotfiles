#!/usr/bin/env bash

. ~/.scripts/defs.zsh

~/.screenlayout/screenlayout.sh
~/.scripts/set-wallpaper.sh

#==============
# System tools
#==============
killall xfce-polkit
sleep 1
run /usr/lib/xfce-polkit/xfce-polkit
# run lxqt-policykit-agent

run ibus-daemon -d -x
run unclutter --root --timeout 3
run xscreensaver

killall xbindkeys
# run xbindkeys

#cursor active at boot
run xsetroot -cursor_name left_ptr -bg "#2a2a2a" -fg "#d6d6d6"

#---------------
# Desktop Tools
#---------------
pkill -f volume-osd
sleep 1
~/.bin/volume-osd &

restart alttab -n 1
run pamac-tray

restart blueman-tray
restart blueman-applet
run nm-applet

# Sound Tray Icons
# restart start-pulseaudio-x11
run pasystray

run radiotray-ng
run kteatime
run copyq

run system-config-printer-applet

killall -q picom
sleep 1
sh $SCRIPTS/picom-toggle-awesome.sh &

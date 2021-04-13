#!/usr/bin/env bash

. ~/.scripts/defs.zsh

run ibus-daemon -d -x

#change your keyboard if you need it
run setxkbmap -layout de
run numlockx on

run unclutter --root --timeout 3

#cursor active at boot
run xsetroot -cursor_name left_ptr -bg "#2a2a2a" -fg "#d6d6d6"

sh ~/.scripts/set-wallpaper.sh

#starting utility applications at boot time
run nm-applet
run pamac-tray
run radiotray-ng
run kteatime

# killall xbindkeys
restart xbindkeys

# Sound Tray Icons
run start-pulseaudio-x11
run pasystray

run blueman-tray
run blueman-applet

run xscreensaver

run copyq
restart alttab -n 1

run system-config-printer-applet

killall -q picom
sleep 1
sh $SCRIPTS/picom-toggle-awesome.sh &

killall xfce-polkit
sleep 1
run /usr/lib/xfce-polkit/xfce-polkit
# run /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
# run lxqt-policykit-agent

if [ $IS_MANJARO = true -o $IS_GARUDA = true ]; then
	run msm_notifier
fi

CUSTOM_AUTOSTART="$HOME/.autostart-custom"
if [ -f $CUSTOM_AUTOSTART ]; then 
	sh $CUSTOM_AUTOSTART
fi

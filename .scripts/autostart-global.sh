#!/usr/bin/env bash

function run() {
    if ! pgrep $1; then
        $@ &
    fi
}

function restart() {
    killall $1
}


#change your keyboard if you need it
setxkbmap -layout de

#cursor active at boot
xsetroot -cursor_name left_ptr &

sh ~/.scripts/set-wallpaper.sh

#starting utility applications at boot time
run nm-applet &
run pamac-tray &
run radiotray &
run kteatime &

killall xbindkeys
run xbindkeys &

# Sound Tray Icons
killall pasystray
run pasystray &

numlockx on &
blueman-tray &
run xscreensaver &
# /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

run copyq &
run teams &

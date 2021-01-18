#!/usr/bin/env bash

function run() {
    if command -v $1 &>/dev/null; then
        echo "autostart: $@"
        #pgrep -u "$USER" -fx "$1" >/dev/null || ($@) &
        if ! pgrep "$1"; then
            echo "execute: $@"
            $@ &
        fi
    else
        echo "error Autostart: $@"
        notify-send -t 10000 -u critical "Error Autostart" "Kommando '$1' nicht gefunden"
    fi
}

function restart() {
    killall "$1"
    run $@
}

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
run radiotray
run kteatime

# killall xbindkeys
restart xbindkeys

# Sound Tray Icons
run pasystray

run blueman-tray
run xscreensaver
run xsettingsd
restart /usr/lib/xfce-polkit/xfce-polkit
# restart /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
# restart lxqt-policykit-agent

run copyq
run xfce4-notes
run teams

killall -q picom
sh $SCRIPTS/picom-toggle-awesome.sh &

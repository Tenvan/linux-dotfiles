#!/usr/bin/env bash

function run() {
    if command -v $1 &>/dev/null; then
        #pgrep -u "$USER" -fx "$1" >/dev/null || ($@) &
        if ! pgrep "$1"; then
            $@ &
        fi
    else
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
run msm_notifier
run radiotray
run kteatime

# killall xbindkeys
restart xbindkeys

# Sound Tray Icons
run start-pulseaudio-x11
run pasystray

run redshift-gtk

run blueman-tray
run blueberry-tray
run blueman-applet

run xscreensaver
# run /usr/lib/xfce-polkit/xfce-polkit
# run /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
run lxqt-policykit-agent

run copyq
run xfce4-notes
run teams
run paleofetch --recache

run $(which gesettings-data-convert)

run /usr/lib/xapps/sn-watcher/xapp-sn-watcher

run system-config-printer-applet

killall -q picom
sh $SCRIPTS/picom-toggle-awesome.sh &

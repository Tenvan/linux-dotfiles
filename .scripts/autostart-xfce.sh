#!/usr/bin/env bash

function run() {
    if ! pgrep $1; then
        $@ &
    fi
}

function restart() {
    killall $1
}

awesome -r -c $(eval echo $HOME/.config/awesome/rc-xfc.lua)

######################
# Settings wallpaper #
######################

# nitrogen --restore
nitrogen --set-zoom-fill ~/.local/share/wallpapers/shared/blue-earth-2880x1800.jpg --head=1
nitrogen --set-zoom-fill ~/.local/share/wallpapers/shared/floating-world-in-space-3000x2357.jpg --head=0

#starting utility applications at boot time
#run nm-applet &
#run pamac-tray &
#run xfce4-power-manager &
run radiotray &
run kteatime &

killall xbindkeys
run xbindkeys &

# Sound Tray Icons
#killall pasystray
#run pasystray &

#numlockx on &
#blueman-tray &
#run xscreensaver &
#/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

#run copyq &
#run teams &

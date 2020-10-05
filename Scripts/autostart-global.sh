#!/bin/bash

function run() {
    if ! pgrep $1; then
        $@ &
    fi
}

function restart() {
    killall $1
}

xrdb -merge $HOME/.Xresources
xrdb -merge $HOME/.Xresources.custom
xrdb -merge $HOME/.Xresources.monitor
xrdb -merge $HOME/.Xresources.personal

#change your keyboard if you need it
#setxkbmap -layout be

#cursor active at boot
xsetroot -cursor_name left_ptr &

#start ArcoLinux Welcome App
#run dex $HOME/.config/autostart/arcolinux-welcome-app.desktop

######################
# Settings wallpaper #
######################

# nitrogen --restore
nitrogen --set-zoom-fill ~/.local/share/wallpapers/shared/blue-earth-2880x1800.jpg --head=1
nitrogen --set-zoom-fill ~/.local/share/wallpapers/shared/floating-world-in-space-3000x2357.jpg --head=0
# run variety &

# Some ways to set your wallpaper besides variety or nitrogen
# feh --bg-fill /usr/share/backgrounds/arcolinux/arco-wallpaper.jpg &

#start the conky to learn the shortcuts
# killall conky
# (conky -c $HOME/.xmonad/scripts/system-overview) &

#starting utility applications at boot time
run nm-applet &
run pamac-tray &
run xfce4-power-manager &
killall radiotray
run radiotray &
killall xbindkeys
run xbindkeys &
killall alttab
run alttab &
killall volumeicon
run volumeicon &
killall pasystray
run pasystray &
numlockx on &
# blueberry-tray &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

run copyq &
run teams &

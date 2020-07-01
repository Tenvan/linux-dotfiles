#!/bin/bash

function run() {
  if ! pgrep $1; then
    $@ &
  fi
}

function restart() {
  while pgrep -u $UID -x $1 >/dev/null; do sleep 1; done
  if ! pgrep $1; then
    $@ &
  fi
}

xrdb -merge $HOME/.Xresources
xrdb -merge $HOME/.Xresources.custom
xrdb -merge $HOME/.Xresources.monitor
xrdb -merge $HOME/.Xresources.personal

(
  sleep 2
  # sh $HOME/.config/polybar/launch.sh
) &

#change your keyboard if you need it
#setxkbmap -layout be

#cursor active at boot
xsetroot -cursor_name left_ptr &

#start ArcoLinux Welcome App
#run dex $HOME/.config/autostart/arcolinux-welcome-app.desktop

######################
# Settings wallpaper #
######################

nitrogen --restore
# run variety &
# Some ways to set your wallpaper besides variety or nitrogen
# feh --bg-fill /usr/share/backgrounds/arcolinux/arco-wallpaper.jpg &

#start the conky to learn the shortcuts
# killall conky
# (conky -c $HOME/.xmonad/scripts/system-overview) &

#starting utility applications at boot time
run emacs --daemon &

run nm-applet &
run pamac-tray &
run xfce4-power-manager &
# run volumeicon &
numlockx on &
# blueberry-tray &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
/usr/lib/xfce4/notifyd/xfce4-notifyd &

#starting utility applications at boot time
run pasystray &
run copyq &
run teams &

restart alttab &
# restart plank &

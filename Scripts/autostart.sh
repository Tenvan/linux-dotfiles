#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

xrdb -merge ~/.Xresources

(sleep 2; sh $HOME/.config/polybar/launch.sh) &

#change your keyboard if you need it
#setxkbmap -layout be

#cursor active at boot
xsetroot -cursor_name left_ptr &

#start the conky to learn the shortcuts
# (conky -c $HOME/.xmonad/scripts/system-overview) &

# Xfce4 Panel
# (killall xfce4-panel; xfce4-panel) &

killall pasystray

#starting utility applications at boot time
run nm-applet &
run pamac-tray &
run xfce4-power-manager &
run pasystray &
run copyq &
run alttab &
run nitrogen --restore &

numlockx on &
blueberry-tray &

killall picom -s 9;sleep 1

if [ -f $HOME/.config/picom/picom-private.conf ] ; then
    picom --config $HOME/.config/picom/picom-private.conf &
elif [ -f $HOME/.config/picom/picom.conf ] ; then
        picom --config $HOME/.config/picom/picom.conf &
fi

/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
/usr/lib/xfce4/notifyd/xfce4-notifyd &

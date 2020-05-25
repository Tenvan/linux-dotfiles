#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

(sleep 2; sh $HOME/.config/polybar/launch.sh) &

#change your keyboard if you need it
#setxkbmap -layout be

#cursor active at boot
xsetroot -cursor_name left_ptr &

#start the conky to learn the shortcuts
# (conky -c $HOME/.xmonad/scripts/system-overview) &

# Xfce4 Panel
(killall xfce4-panel; xfce4-panel) &

#starting utility applications at boot time
run nm-applet &
run pamac-tray &
run xfce4-power-manager &
# run pa-applet &
run volumeicon &
run copyq &

numlockx on &
blueberry-tray &

# picom --config $HOME/.config/picom/picom.conf &

/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
/usr/lib/xfce4/notifyd/xfce4-notifyd &

#starting user applications at boot time
nitrogen --restore &
#run caffeine &
#run vivaldi-stable &
#run firefox &
#run thunar &
#run spotify &
#run atom &

#run telegram-desktop &
#run discord &
#run dropbox &
#run insync start &
#run ckb-next -b &

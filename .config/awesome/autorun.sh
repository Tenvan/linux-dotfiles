#!/usr/bin/env bash

function run {
  if ! pgrep -f $1 ;
  then
    $@&
  fi
}

##########################
# Autostart applications #
##########################

## system trays
# run volumeicon
run pa-applet
run nm-applet
run pamac-tray
run system-config-printer-applet
run xfce4-power-manager
# run blueman-applet
# run sbxkb

## TODO noch nötig ?
run xset -b

## polkits
run /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
run /usr/lib/polkit-kde-authentication-agent-1

## Desktop
run xautolock -time 10 -locker blurlock
run nitrogen --restore

killall picom
run picom --config ~/.config/compton-awesome.conf -b

## Tools
run copyq

## conky
killall conky
sleep 1
run conky -c ~/.config/conky/conky-main.conf
# run conky -c ~/.config/conky/conky-help.conf

notify-send ' conky started!'

## Services
run ungit --no-launchBrowser
notify-send ' ungit started!'

## utils
sh /opt/screenlayout.sh

## Apps
run teams

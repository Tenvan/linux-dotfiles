#!/usr/bin/env bash

wm=awesome

if pgrep -x "picom" >/dev/null; then
    notify-send "Picom disabled !"
    killall -q picom
    while pgrep -u $UID -x "picom" >/dev/null; do sleep 1; done
else
    if [ -f "$HOME/.config/picom/picom-$wm-private.conf" ]; then
        notify-send "Picom loaded:<br>$HOME/.config/picom/picom-$wm-private.conf"
        picom --config "$HOME/.config/picom/picom-$wm-private.conf" &
    elif [ -f "$HOME/.config/picom/picom-custom.conf" ]; then
        notify-send "Picom loaded:<br>$HOME/.config/picom/picom-$wm-custom.conf"
        picom --config "$HOME/.config/picom/picom-$wm-custom.conf" &
    else
        notify-send "Picom loaded:<br>$HOME/.config/picom/picom-$wm.conf"
        picom --config "$HOME/.config/picom/picom-$wm.conf" &
    fi
fi

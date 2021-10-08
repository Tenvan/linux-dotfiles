#!/usr/bin/env bash
echo "# --> $BASH_SOURCE"

if pgrep -x "picom" >/dev/null; then
    notify-send "Picom disabled !"
    killall -q picom
    while pgrep -u $UID -x "picom" >/dev/null; do sleep 1; done
else
    if [ -f "$HOME/.config/picom/picom-awesome-private.conf" ]; then
        notify-send "Picom loaded:<br>$HOME/.config/picom/picom-awesome-private.conf"
        picom --config "$HOME/.config/picom/picom-awesome-private.conf" &
    elif [ -f "$HOME/.config/picom/picom-awesome-custom.conf" ]; then
        notify-send "Picom loaded:<br>$HOME/.config/picom/picom-awesome-custom.conf"
        picom --config "$HOME/.config/picom/picom-awesome-custom.conf" &
    else
        notify-send "Picom loaded:<br>$HOME/.config/picom/picom-awesome.conf"
        picom --config "$HOME/.config/picom/picom-awesome.conf" &
    fi
fi

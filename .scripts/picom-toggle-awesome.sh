#!/usr/bin/env bash
if pgrep -x "picom" >/dev/null; then
    notify-send "Picom disabled !"
    killall -q picom
    while pgrep -u $UID -x "picom" >/dev/null; do sleep 1; done
else
    for path_candidate in \
    "$HOME/.config/picom/picom.conf" \
    "$HOME/.config/awesome/configuration/picom.conf"
    do
        if [[ -f "${path_candidate}" ]]; then
            export PICOM_CONF="${path_candidate}"
        fi
    done
    
    notify-send.sh "Picom loaded" "$PICOM_CONF"
    picom -b --experimental-backends --dbus --config "$PICOM_CONF" &
fi

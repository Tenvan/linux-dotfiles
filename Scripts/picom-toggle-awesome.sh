#!/bin/bash

wm=awesome

if pgrep -x "picom" >/dev/null; then
    killall -q picom
    while pgrep -u $UID -x "picom" >/dev/null; do sleep 1; done
else
    if [ -f $HOME/.config/picom/picom-$wm-private.conf ]; then
        picom --config $HOME/.config/picom/picom-$wm-private.conf &
    elif [ -f $HOME/.config/picom/picom-custom.conf ]; then
        picom --config $HOME/.config/picom/picom-$wm-custom.conf &
    else
        picom --config $HOME/.config/picom/picom-$wm.conf &
    fi
fi

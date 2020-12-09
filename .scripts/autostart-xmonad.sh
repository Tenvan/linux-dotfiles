#!/usr/bin/env bash

function run() {
    if ! pgrep $1; then
        $@ &
    fi
}

while pgrep -u $UID -x "picom" >/dev/null; do sleep 1; done
sh $SCRIPTS/picom-toggle-xmonad.sh
sh $HOME/.config/polybar/launch-xmonad.sh

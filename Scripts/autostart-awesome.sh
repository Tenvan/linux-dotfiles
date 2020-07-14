#!/bin/bash

function run() {
    if ! pgrep $1; then
        $@ &
    fi
}

killall -q picom

while pgrep -u $UID -x "picom" >/dev/null; do sleep 1; done

sh $HOME/Scripts/picom-toggle-awesome.sh &
sh $HOME/.config/polybar/launch-awesome.sh &

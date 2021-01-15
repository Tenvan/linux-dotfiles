#!/usr/bin/env bash

function run() {
    if ! pgrep $1; then
        $@ &
    fi
}

function restart() {
    killall $1
}


while pgrep -u $UID -x "picom" >/dev/null; do sleep 1; done

killall -q picom
sh $SCRIPTS/picom-toggle-awesome.sh &

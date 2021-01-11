#!/usr/bin/env bash

function run() {
    if ! pgrep $1; then
        $@ &
    fi
}

function restart() {
    killall $1
}

killall -q picom

while pgrep -u $UID -x "picom" >/dev/null; do sleep 1; done

sh $SCRIPTS/picom-toggle-awesome.sh &

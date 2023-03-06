#!/usr/bin/env bash
. $SCRIPTS/defs

notify-send -t 3000 -u critical System "Bidschirm sperre wird aktiviert..."
sound lock-screen &
betterlockscreen -l

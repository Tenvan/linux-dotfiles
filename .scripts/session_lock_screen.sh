#!/usr/bin/env bash

. $SCRIPTS/defs

notify-send -t 3000 -u critical System "Bidschirm sperre wird aktiviert..."
playsound lock-screen &
betterlockscreen -l

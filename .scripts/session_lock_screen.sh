#!/usr/bin/env bash

. ~/.scripts/sounds

notify-send.sh -t 3000 -u critical System "Bidschirm sperre wird aktiviert..."
sound lock-screen &
xscreensaver-command -lock

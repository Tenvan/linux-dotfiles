#!/usr/bin/env bash
notify-send.sh -t 3000 -u critical System "Bidschirm sperre wird aktiviert..."
paplay /usr/share/sounds/Smooth/stereo/desktop-login.oga &
xscreensaver-command -lock

#!/usr/bin/env bash
echo "# --> $BASH_SOURCE"

. $SCRIPTS/defs

notify-send.sh -t 3000 -u critical System "Bidschirm sperre wird aktiviert..."
sound lock-screen &
xscreensaver-command -lock

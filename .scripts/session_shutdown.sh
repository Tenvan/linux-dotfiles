#!/usr/bin/env bash
. $SCRIPTS/defs

notify-send -t 3000 -u critical System "Rechner wird herunter gefahren..."
sound shutdown &

prepare_sessiondown

sudo systemctl poweroff
# shutdown --poweroff 0

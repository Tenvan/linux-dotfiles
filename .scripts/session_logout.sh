#!/usr/bin/env zsh
. $SCRIPTS/defs

notify-send -t 3000 -u critical System "Sitzung wird beendet..."
playsound session-logout &
pkill awesome

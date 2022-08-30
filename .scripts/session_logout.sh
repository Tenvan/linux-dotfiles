#!/usr/bin/env zsh
. $SCRIPTS/defs

notify-send.sh -t 3000 -u critical System "Sitzung wird beendet..."
sound session-logout &

pkill awesome

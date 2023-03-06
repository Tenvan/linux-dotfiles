#!/usr/bin/env bash
. $SCRIPTS/defs

notify-send -t 3000 -u critical System "Rechner in den Ruhezustand fahren ..."
sound session-hibernate &
sudo pm-hibernate
# sudo pm-hibernate

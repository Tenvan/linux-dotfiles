#!/usr/bin/env bash
. $SCRIPTS/defs

notify-send.sh  -t 3000 -u critical System "Rechner in den Ruhezustand fahren ..."
sound session-hibernate &
sudo systemctl hibernate
# sudo pm-hibernate

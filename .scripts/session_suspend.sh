#!/usr/bin/env bash

. $SCRIPTS/defs

notify-send -t 3000 -u critical System "Rechner in Eneriesparmodus setzen..."
playsound suspend &
sudo systemctl suspend
# sudo pm-suspend

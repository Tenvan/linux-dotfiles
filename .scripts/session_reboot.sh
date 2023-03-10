#!/usr/bin/env bash

. $SCRIPTS/defs

notify-send -t 3000 -u critical System "Rechner wird neu gestartet..."
playsound reboot &
prepare_sessiondown
sudo systemctl reboot
# shutdown --reboot 0

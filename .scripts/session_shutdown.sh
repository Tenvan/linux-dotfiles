#!/usr/bin/env bash
. $SCRIPTS/defs

notify-send -t 3000 -u critical System "Rechner wird herunter gefahren..."
playsound shutdown &
sudo podman stop -a
sudo systemctl poweroff

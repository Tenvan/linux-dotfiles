#!/usr/bin/env bash
notify-send.sh -t 3000 -u critical System "Rechner wird gesperrt..."
paplay /usr/share/sounds/Smooth/stereo/count-down.oga &
dm-tool lock

#!/usr/bin/env bash

. ~/.scripts/sounds

sound session-switch-user &
notify-send.sh -t 3000 -u critical System "Benutzerwechsel..."
dm-tool switch-to-greeter

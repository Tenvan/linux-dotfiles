#!/usr/bin/env bash
. $SCRIPTS/defs

sound session-switch-user &
notify-send -t 3000 -u critical System "Benutzerwechsel..."
dm-tool switch-to-greeter

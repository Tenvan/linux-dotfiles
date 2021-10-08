#!/usr/bin/env bash
. $SCRIPTS/defs

notify-send.sh -t 3000 -u critical System "Rechner wird gesperrt..."
sound session-lock &
dm-tool lock

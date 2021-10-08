#!/usr/bin/env zsh
. $SCRIPTS/defs

# notify-send -t 3000 -u critical "Play Soundfile" $@
sound $@ &

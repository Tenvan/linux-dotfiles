#!/usr/bin/env bash
. ~/.scripts/sounds

# notify-send -t 3000 -u critical "Play Soundfile" $@
sound $@ &

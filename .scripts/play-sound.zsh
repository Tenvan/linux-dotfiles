#!/usr/bin/env zsh
echo "# --> $BASH_SOURCE"

. $SCRIPTS/defs

# notify-send -t 3000 -u critical "Play Soundfile" $@
sound $@ &

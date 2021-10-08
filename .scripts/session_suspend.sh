#!/usr/bin/env bash
echo "# --> $BASH_SOURCE"

. $SCRIPTS/defs

notify-send.sh -t 3000 -u critical System "Rechner in Eneriesparmodus setzen..."
sound suspend &
# sudo systemctl suspend
sudo pm-suspend

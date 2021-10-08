#!/usr/bin/env bash
echo "# --> $BASH_SOURCE"

. $SCRIPTS/defs

notify-send.sh -t 3000 -u critical System "Rechner wird neu gestartet..."
sound reboot &
sudo systemctl stop mssql-server
# sudo systemctl reboot
shutdown --reboot 0

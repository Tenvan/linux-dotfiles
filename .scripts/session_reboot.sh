#!/usr/bin/env bash
. $SCRIPTS/defs

notify-send.sh -t 3000 -u critical System "Rechner wird neu gestartet..."
sound reboot &
$SCRIPTS/helper/mssql/stop
sudo systemctl reboot
# shutdown --reboot 0

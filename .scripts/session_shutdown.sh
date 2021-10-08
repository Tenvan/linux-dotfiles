#!/usr/bin/env bash
. $SCRIPTS/defs

notify-send.sh -t 3000 -u critical System "Rechner wird herunter gefahren..."
sound shutdown &
sudo systemctl stop mssql-server
# sudo systemctl poweroff
shutdown --poweroff 0

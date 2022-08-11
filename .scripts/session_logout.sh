#!/usr/bin/env bash
. $SCRIPTS/defs

notify-send.sh -t 3000 -u critical System "Sitzung wird beendet..."
sound session-logout &
$SCRIPTS/helper/mssql/stop
# killall awesome
pkill awesome

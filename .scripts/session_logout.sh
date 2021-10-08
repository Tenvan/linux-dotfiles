#!/usr/bin/env bash
. $SCRIPTS/defs

notify-send.sh -t 3000 -u critical System "Sitzung wird beendet..."
sound session-logout &
sudo systemctl restart lightdm
# sudo systemctl restart lightdm-plymouth
# pkill awesome

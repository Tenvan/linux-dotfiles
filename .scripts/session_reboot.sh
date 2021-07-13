#!/usr/bin/env bash
notify-send.sh -t 3000 -u critical System "Rechner wird neu gestartet..."
paplay /usr/share/sounds/LinuxMint/stereo/desktop-logout.ogg
sudo systemctl stop mssql-server
# sudo systemctl reboot
shutdown --reboot 0

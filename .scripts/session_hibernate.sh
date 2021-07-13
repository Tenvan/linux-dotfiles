#!/usr/bin/env bash
notify-send.sh  -t 3000 -u critical System "Rechner in den Ruhezustand fahren ..."
paplay /usr/share/sounds/LinuxMint/stereo/desktop-logout.ogg &
# sudo systemctl hibernate
sudo pm-hibernate

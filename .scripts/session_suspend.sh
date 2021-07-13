#!/usr/bin/env bash
notify-send.sh -t 3000 -u critical System "Rechner in Eneriesparmodus setzen..."
paplay /usr/share/sounds/LinuxMint/stereo/desktop-logout.ogg
# sudo systemctl suspend
sudo pm-suspend

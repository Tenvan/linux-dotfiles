#!/usr/bin/env bash
notify-send.sh -t 3000 -u critical System "Sitzung wird beendet..."
paplay /usr/share/sounds/LinuxMint/stereo/desktop-logout.ogg
sudo systemctl restart lightdm
# sudo systemctl restart lightdm-plymouth
# pkill awesome

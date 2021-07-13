
#!/usr/bin/env bash
notify-send.sh -t 3000 -u critical System "Rechner wird herunter gefahren..."
paplay /usr/share/sounds/LinuxMint/stereo/desktop-logout.ogg
sudo systemctl stop mssql-server
# sudo systemctl poweroff
shutdown --poweroff 0

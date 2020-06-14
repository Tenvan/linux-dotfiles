#!/usr/bin/env bash

#yay -S picom
yay -S picom-ibhagwan-git
#yay -S picom-tryone-git

yay -S --noconfirm --needed \
  rofi zenity \
  polybar broot broom pm-utils \
  polkit-gnome polkit-kde-agent \
  pamac-tray-appindicator checkupdates-aur \
  xautolock
python python-psutil python-pygit2 python-xkbgroup python-taskw python-requests pygtk python2-distutils-extra \
  xclip xsel xdotool xorg-xfd xcwd-git progress \
  bitwarden-bin bitwarden-cli-bin bitwarden-rofi

# Manjaro
yay -S --noconfirm --needed bmenu dmenu-manjaro conky alttab-git

# some tools
yay -S --noconfirm --needed \
  xfce4-taskmanager xfce4-appfinder lxappearance

chmod +x ~/Scripts/100-user-monitors.sh
sudo cp ~/Scripts/100-user-monitors.sh /etc/X11/xinit/xinitrc.d

# Default Browser setzen (vorher $BROWSER Variable entfernen)
xdg-settings set default-web-browser firefox.desktop

#!/usr/bin/env bash

#####################
# init distro check #
#####################
LINUX_VERSION_NAME=$(lsb_release -sc)
if [[ ${LINUX_VERSION_NAME} == "Manjaro" ]]; then
  IS_MANJARO=true
else
  IS_MANJARO=false
fi

if [[ ${LINUX_VERSION_NAME} == "ArcoLinux" ]]; then
  IS_ARCO=true
else
  IS_ARCO=false
fi

yay -S --noconfirm --needed \
  rofi zenity \
  polybar broot broom pm-utils \
  polkit-gnome polkit-kde-agent \
  pamac-tray-appindicator checkupdates-aur \
  xautolock trayer \
  python python-psutil python-pygit2 python-xkbgroup python-taskw python-requests pygtk python2-distutils-extra \
  xclip xsel xdotool xorg-xfd xcwd-git progress \
  bitwarden-bin bitwarden-cli-bin bitwarden-rofi

# some tools
yay -S --noconfirm --needed \
  xfce4-taskmanager xfce4-appfinder lxappearance alttab-git

if $IS_MANJARO == true; then
  #yay -S picom
  yay -S picom-ibhagwan-git
  #yay -S picom-tryone-git

  yay -S --noconfirm --needed bmenu dmenu-manjaro conky
fi

chmod +x ~/Scripts/100-user-monitors.sh
sudo cp ~/Scripts/100-user-monitors.sh /etc/X11/xinit/xinitrc.d

# Default Browser setzen (vorher $BROWSER Variable entfernen)
xdg-settings set default-web-browser firefox.desktop

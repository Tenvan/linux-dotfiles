#!/usr/bin/env bash

#####################
# init distro check #
#####################
LINUX_VERSION_NAME=$(lsb_release -si)

IS_MANJARO=false
IS_ARCO=false
if [[ "$LINUX_VERSION_NAME" == *"Manjaro"* ]]; then
    IS_MANJARO=true
fi

if [[ "$LINUX_VERSION_NAME" == *"ArcoLinux"* ]]; then
    IS_ARCO=true
fi

errorCheck() {
    retVal=$?
    if [ $retVal -ne 0 ]; then
        echo "abort installation script 'install_desktop': " $1
        exit $retVal
    fi
}

#####################################
# installation desktop packages     #
# for tiling windows managers (all) #
#####################################

# system packages
yay -S --needed \
    time rofi zenity xclip xsel xdotool xorg-xfd xcwd-git \
    polybar broot pm-utils \
    polkit-gnome polkit-kde-agent \
    pamac-tray-appindicator checkupdates-aur \
    xfce4-meta xfce4-taskmanager xfce4-appfinder lxappearance alttab-git xautolock \
    python python-psutil python-pygit2 python-xkbgroup python-taskw python-requests pygtk python2-distutils-extra

errorCheck "system packages"

# application packages
yay -S --noconfirm --needed \
    evolution bitwarden-bin bitwarden-cli-bin bitwarden-rofi

errorCheck "application packages"

yay -S picom-ibhagwan-git

errorCheck "picom"

if $IS_MANJARO == true; then
    yay -S --noconfirm --needed bmenu dmenu-manjaro conky
    errorCheck "Manjaro: menus"
fi

chmod +x ~/Scripts/100-user-monitors.sh
sudo cp ~/Scripts/100-user-monitors.sh /etc/X11/xinit/xinitrc.d

# Default Browser setzen (vorher $BROWSER Variable entfernen)
xdg-settings set default-web-browser firefox.desktop

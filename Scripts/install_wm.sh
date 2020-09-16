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
        echo "abort installation script 'install_wm': " $1
        exit $retVal
    fi
}

########################################
# Installing all used Window Managers  #
# Unter ArcoLinux TweakTool verwenden. #
########################################

if $IS_MANJARO == true; then
    yay -S --noconfirm --needed dmenu-manjaro
    errorCheck "Manjaro: menus"

    # current used settings
    yay -S --needed manjaro-awesome-settings
    errorCheck "Manjaro: settings"
fi


# awesome packages
yay -S --needed --noconfirm \
    awesome vicious xorg-server-xephyr luacheck luarocks lua-luajson lua-socket awmtt bmenu
    
errorCheck "all awesome"

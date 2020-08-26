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

#######################################
# Installing all used Window Managers #
# Nur mit Manajaro verwenden !!!      #
# Unter ArcoLinux besser TweakTool !! #
#######################################

if $IS_MANJARO == true; then
    # awesome
    yay -S --needed --noconfirm \
        awesome xorg-server-xephyr luacheck luarocks lua-luajson lua-socket awmtt
    errorCheck "Manajaro: awesome"

    # qtile
    yay -S --noconfirm --needed qtile
    errorCheck "Manajaro: qtile"

    # current used settings
    yay -S --needed manjaro-awesome-settings
    errorCheck "Manajaro: settings"
fi

if $IS_ARCO == true; then
    yay -S --noconfirm --needed blingbling
    errorCheck "ArcoLinux: AWESOME Stuff"

    # yay -S --noconfirm --needed \
    #     instantassist \
    #     instantconf \
    #     instantcursors \
    #     instantdotfiles \
    #     instantfonts \
    #     instantlock \
    #     instantmenu \
    #     instantsettings \
    #     instantshell \
    #     instantthemes \
    #     instanttools \
    #     instantutils \
    #     instantwallpaper \
    #     instantwelcome \
    #     instantwidgets \
    #     instantwm

    errorCheck "ArcoLinux: InstantOS"
fi

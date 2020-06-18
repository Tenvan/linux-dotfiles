#!/usr/bin/env bash

##################################################
# Installation Base Development System (OneTime) #
##################################################
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

if $IS_ARCO == true; then
  errorCheck "ArcoLinux: haskell"
fi

if $IS_MANJARO == true; then
  # xmonad
  yay -S --noconfirm --needed \
    xmonad xmonad-contrib xmonad-log xmonad-utils haskell-dbus stack
  errorCheck "Manajaro: xmonad"

  # awesome
  yay -S --needed --noconfirm \
    awesome xorg-server-xephyr luacheck luarocks lua-luajson lua-socket awmtt
  errorCheck "Manajaro: awesome"

  # i3
  yay -S --noconfirm --needed \
    perl-anyevent-i3 dunst i3exit conky-i3 python-i3ipc xkb-switch-i3
  errorCheck "Manajaro: i3"

  # qtile
  yay -S --noconfirm --needed qtile
  errorCheck "Manajaro: qtile"

  # dwm
  yay -S --noconfirm --needed dwm
  errorCheck "Manajaro: dwm"

  # bspwm
  yay -S --noconfirm --needed bspwm-manjaro
  errorCheck "Manajaro: bspwm"

  # current used settings
  yay -S --noconfirm --needed \
    manjaro-xfce-settings
  # manjaro-awesome-settings
  errorCheck "Manajaro: settings"

fi

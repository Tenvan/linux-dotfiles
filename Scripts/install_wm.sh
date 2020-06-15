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

#######################################
# Installing all used Window Managers #
# Nur mit Manajaro verwenden !!!      #
# Unter ArcoLinux besser TweakTool !! #
#######################################

if $IS_ARCO == true; then
  yay -S --noconfirm --needed hoogle haskell-language-server-git haskell-ide-engine
  hoogle generate
fi

if $IS_MANJARO == true; then
  # xmonad
  yay -S --noconfirm --needed \
    xmonad xmonad-contrib xmonad-log xmonad-utils haskell-dbus stack hoogle

  hoogle generate

  # awesome
  yay -S --needed --noconfirm \
    awesome xorg-server-xephyr luacheck luarocks lua-luajson lua-socket awmtt

  # i3
  yay -S --noconfirm --needed \
    perl-anyevent-i3 dunst i3exit conky-i3 python-i3ipc xkb-switch-i3

  # qtile
  yay -S --noconfirm --needed qtile

  # dwm
  yay -S --noconfirm --needed dwm

  # bspwm
  yay -S --noconfirm --needed bspwm-manjaro

  # current used settings
  yay -S --noconfirm --needed \
    manjaro-xfce-settings
  # manjaro-awesome-settings

fi

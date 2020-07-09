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

# awesome
yay -S --needed --noconfirm \
  awesome xorg-server-xephyr luacheck luarocks lua-luajson lua-socket awmtt
errorCheck "Manajaro: awesome"


if $IS_ARCO == true; then
    #~ yay -S --noconfirm --needed \
        #~ instantmenu instantconf instantassist instantwm instantshell instantsettings \
        #~ instantlock instantwallpaper instantthemes instanttools instantdotfiles instantwelcome instantfonts \
        #~ instantwidgets instantutils

    #~ yay -Rsnu --noconfirm \
        #~ instantmenu instantconf instantassist instantwm instantshell instantsettings \
        #~ instantlock instantwallpaper instantthemes instanttools instantdotfiles instantwelcome instantfonts \
        #~ instantwidgets instantutils

##    inantcursors instantos instantdepend rangerplugins sideload irox islide grub-instantos xdragon pa-applet paperbash inantcursor

    errorCheck "ArcoLinux: InstantOS"
fi

if $IS_MANJARO == true; then
  # i3
  yay -S --noconfirm --needed \
    perl-anyevent-i3 dunst i3exit conky-i3 python-i3ipc xkb-switch-i3
  errorCheck "Manajaro: i3"

  # qtile
  yay -S --noconfirm --needed qtile
  errorCheck "Manajaro: qtile"

  # dwm
  yay -S --noconfirm --needed dwm
  errorCheck "Manajaro: bspwm"

  # dwm
  yay -S --needed bspwm-manjaro
  errorCheck "Manajaro: bspwm"

  # current used settings
  yay -S --noconfirm --needed \
    manjaro-xfce-settings
  # manjaro-awesome-settings
  errorCheck "Manajaro: settings"

fi

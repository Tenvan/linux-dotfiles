#!/usr/bin/env bash

#############################
# install packages (needed) #
#############################

# Manjaro
if $IS_MANJARO == true; then
  echo Manjaro entdeckt
fi

# ArcoLinux
if $IS_ARCO == true; then
  echo ArcoLinux entdeckt
fi

sh ~/Scripts/install_system.sh
sh ~/Scripts/install_wm.sh
sh ~/Scripts/install_desktop.sh
sh ~/Scripts/install_editors.sh
sh ~/Scripts/install_arts.sh
sh ~/Scripts/install-vs-extensions.sh

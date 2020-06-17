#!/usr/bin/env bash

# Manjaro
if $IS_MANJARO == true; then
  echo Manjaro entdeckt
fi

# ArcoLinux
if $IS_ARCO == true; then
  echo ArcoLinux entdeckt
fi

errorCheck() {

  retVal=$?
  if [ $retVal -ne 0 ]; then
    echo "abort complete installation: " $1
    exit $retVal
  fi
}

#################################
# install all (needed) packages #
#################################

sh ~/Scripts/install_system.sh
errorCheck "install_system"

sh ~/Scripts/install_wm.sh
errorCheck "install_wm"

sh ~/Scripts/install_desktop.sh
errorCheck "install_desktop"

sh ~/Scripts/install_editors.sh
errorCheck "install_editors"

sh ~/Scripts/install_arts.sh
errorCheck "install_arts"

sh ~/Scripts/install-vs-extensions.sh
errorCheck "install-vs-extensions"

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

#################################
# install all (needed) packages #
#################################

sh ~/Scripts/install_system.sh
errorCheck "install_system"

sh ~/Scripts/install_develop.sh
errorCheck "install_develop"

sh ~/Scripts/install_wm.sh
errorCheck "install_wm"

sh ~/Scripts/install_desktop.sh
errorCheck "install_desktop"

sh ~/Scripts/install_editors.sh
errorCheck "install_editors"

sh ~/Scripts/install_arts.sh
errorCheck "install_arts"


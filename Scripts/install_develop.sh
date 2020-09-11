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
        echo "abort installation script 'install_develop': " $1
        exit $retVal
    fi
}

#######################################
# Installation Base Development Tools #
#######################################

yay -S --noconfirm --needed \
    meld git nsis \
    nodejs nodejs-emojione npm yarn \
    dotnet-sdk \
    mono mono-msbuild \
    jdk8-openjdk openjdk8-src jdk-openjdk

errorCheck "development tools"

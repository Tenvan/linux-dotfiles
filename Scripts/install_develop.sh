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
    git bazel \
    nodejs nodejs-emojione npm yarn \
    dotnet-sdk \
    mono mono-msbuild \
    jdk8-openjdk openjdk8-src jdk-openjdk \
    docker docker-compose

errorCheck "development tools"

# ArcoLinux
if $IS_ARCO == true; then
    # arco only packages
    yay -S --noconfirm --needed nsis2

    errorCheck "ArcoLinux: nsis"
fi

# Manjaro
if $IS_MANJARO == true; then
    yay -S --noconfirm --needed \
        nsis

    errorCheck "Manjaro: nsis"
fi

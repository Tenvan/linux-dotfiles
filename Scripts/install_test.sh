#!/usr/bin/env bash

#####################
# init distro check #
#####################
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
    echo "abort installation script 'install_system': " $1
    exit $retVal
  fi
}

###################
# Test DitroCheck #
###################

echo $LINUX_VERSION_NAME
echo $IS_MANJARO
echo $IS_ARCO

# Manjaro
if $IS_MANJARO == true; then
  echo Manjaro entdeckt
fi

# ArcoLinux
if $IS_ARCO == true; then
  echo ArcoLinux entdeckt
fi

# optional system packages
yay -S --noconfirm --needed \
  exa blub

errorCheck "test: optional system packages"

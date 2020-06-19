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

#######################################
# Installation Base Development Tools #
#######################################

# ArcoLinux
if $IS_ARCO == true; then
  # arco only packages
  yay -S --noconfirm --needed \
    mingw-w64-crt-bin mingw-w64-binutils-bin mingw-w64-winpthreads-bin mingw-w64-headers-bin mingw-w64-gcc-bin mingw-w64-zlib nsis

  errorCheck "ArcoLinux: nsis"
fi

# Manjaro
if $IS_MANJARO == true; then
  yay -S --noconfirm --needed \
    nsis

  errorCheck "Manjaro: nsis"
fi

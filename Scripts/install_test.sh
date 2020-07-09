#!/usr/bin bash

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

###################
# Test DitroCheck #
###################

echo Distro......: $LINUX_VERSION_NAME
echo Is Manjaro..: $IS_MANJARO
echo Is ArcoLinux: $IS_ARCO


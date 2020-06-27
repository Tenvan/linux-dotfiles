#!/usr/bin/env bash

#####################
# init distro check #
#####################
LINUX_VERSION_NAME=$(lsb_release -si)
if [[ ${LINUX_VERSION_NAME} == "ManjaroLinux" ]]; then
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
              echo "abort installation script 'install_editors': " $1
              exit $retVal
       fi
}

#####################################
# installation of important editors #
#####################################

# geany
yay -S --noconfirm --needed geany geany-plugins geany-themes
errorCheck "geany installation"

# vs code
yay -S --noconfirm --needed code bash-completion lua-format
errorCheck "vs code installation"

# micro
yay -S --noconfirm --needed micro-bin
errorCheck "micro installation"

# spacemacs
yay -S --noconfirm --needed emacs

if ! [ -f ~/.emacs.de ]; then
       echo emacs nicht gefunden, spacemacs wird installiert;
       git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
fi

yarn global add tern js-beautify eslint

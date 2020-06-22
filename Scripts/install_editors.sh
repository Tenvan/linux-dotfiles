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
              echo "abort installation script 'install_editors': " $1
              exit $retVal
       fi
}

#####################################
# installation of important editors #
#####################################

# vs code
yay -S --noconfirm --needed code bash-completion lua-format
errorCheck "vs code installation"

# micro
yay -S --noconfirm --needed micro-bin
errorCheck "micro installation"

# emacs
yay -S --noconfirm --needed emacs

if ! [ -f ~/.emacs.de ]; then 
	echo emacs nicht gefunden, spacemacs wird installiert;
	git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
fi

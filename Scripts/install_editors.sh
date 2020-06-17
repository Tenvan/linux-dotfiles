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

# vim/neovim
yay -S --noconfirm --needed neovim vim-plugins python-pip
errorCheck "vim/neovom installation"

sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

pip install --user neovim
errorCheck "vim/neovom pip"

# vs code
yay -S --noconfirm --needed code bash-completion
errorCheck "vs code installation"

# micro
yay -S --noconfirm --needed micro
errorCheck "micro installation"

# atom
yay -S --noconfirm --needed atom
errorCheck "atom installation"

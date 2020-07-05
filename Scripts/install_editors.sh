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

# vim/nvim
yay -S --noconfirm --needed gvim neovim vim-plug python-pynvim nodejs neovim-remote

errorCheck "vim installation"
yarn global add neovim

rm -fr ~/.vim_runtime
git clone --depth=1 https://github.com/amix/vimrc.git ~/.vim_runtime
errorCheck "awesome vim clone local"
sh ~/.vim_runtime/install_awesome_vimrc.sh
errorCheck "awesome vim install"

# emacs / doom
yay -S --noconfirm --needed emacs
errorCheck "emacs installation"

rm -fr ~/.emacs.d
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
errorCheck "doom clone local"

~/.emacs.d/bin/doom install

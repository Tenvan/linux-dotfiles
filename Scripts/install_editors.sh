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

# neovim
yay -S --noconfirm --needed neovim python-pynvim nodejs neovim-remote

# vim
yay -S --noconfirm --needed vim vim-plug
errorCheck "vim installation"

rm -fr ~/.config/nvim
git clone git://github.com/rafi/vim-config.git ~/.config/nvim
errorCheck "rafi vim clone local"
cd ~/.config/nvim
ln -s ~/.config/nvim ~/.vim  # For "regular" Vim
make
cd ~

# veonim, oni
yay -S --noconfirm --needed onivim2 gnvim goneovim

# emacs / doom
yay -S --noconfirm --needed emacs
errorCheck "emacs installation"

rm -fr ~/.emacs.d
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
errorCheck "doom clone local"

~/.emacs.d/bin/doom install

# nodejs tools for editors
yarn global add neovim eslint jshint jsxhint stylelint sass-lint markdownlint-cli raml-cop


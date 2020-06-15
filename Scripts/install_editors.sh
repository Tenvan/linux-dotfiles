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

###############################
# installation of all editors #
###############################

# vim/neovim
yay -S --noconfirm --needed neovim vim-plugins

sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

pip install --user neovim

# emacs doom
yay -S --noconfirm --needed emms emacs

git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install

# vs
yay -S --noconfirm --needed code bash-completion

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

# vim / spacevim
yay -S --noconfirm --needed vim vim-plug
errorCheck "vim installation"

rm -fr ~/.config/nvim
rm -fr ~/.SpaceVim
curl -sLf https://spacevim.org/install.sh | bash
errorCheck "spacevim install"

# emacs / spacemacs
yay -S --noconfirm --needed emacs

if [ -f ~/.emacs.d/.git/config ]; then
    echo Emacs Git gefunden. aktualisiere...
    cd  ~/.emacs.d
    git pull
    errorCheck "pull emacs.d"
    cd ~
else
    echo Emacs Git NICHT gefunden. klone...
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
    errorCheck "clone emacs.de"
fi

errorCheck "spacemacs install"

# nodejs tools for editors
yarn global add neovim eslint jshint jsxhint stylelint sass-lint markdownlint-cli raml-cop typescript tern js-beautify
errorCheck "install required nodejs-tools"


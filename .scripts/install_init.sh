#!/usr/bin/env bash

export SCRIPTS=~/.scripts

. $SCRIPTS/defs.sh

#####################
# init distro check #
#####################

errorCheck() {
    retVal=$?
    if [ $retVal -ne 0 ]; then
        echo "abort installation script 'install_all': $1"
        exit $retVal
    fi
}

sudo pacman -S --noconfirm --needed git base-devel colorgcc
errorCheck "installation base-devel"

git submodule update --init --recursive

# Config pacman
sed 's/^#Color$/Color/g' </etc/pacman.conf >pacman.conf
sudo mv pacman.conf /etc/
sed 's/^.*ILoveCandy$/ILoveCandy/g' </etc/pamac.conf >pamac.conf
sudo mv pamac.conf /etc/
sed 's/^.*CheckAURUpdates$/CheckAURUpdates/g' </etc/pamac.conf >pamac.conf
sudo mv pamac.conf /etc/
sed 's/^.*EnableFlatpak$/EnableFlatpak/g' </etc/pamac.conf >pamac.conf
sudo mv pamac.conf /etc/
sed 's/^.*EnableAUR$/EnableAUR/g' </etc/pamac.conf >pamac.conf
sudo mv pamac.conf /etc/
sed 's/^.*KeepBuiltPkgs$/KeepBuiltPkgs/g' </etc/pamac.conf >pamac.conf
sudo mv pamac.conf /etc/

# install yay and pakku
pamac install --no-confirm yay pakku-git pacman-mirrorup
errorCheck "installation yay"

if [ $IS_GARUDA != true ]; then
	pacman-mirrorup -v
fi

# Prompt installieren
pakku -S $PAKKU_ALL ttf-meslo-nerd-font-powerlevel10k zsh-theme-powerlevel10k
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# disable sudo password
echo "Cmnd_Alias INSTALL = /usr/bin/pacman, /usr/share/pacman
Cmnd_Alias POWER = /usr/bin/pm-hibernate, /usr/bin/pm-powersave, /usr/bin/pm-suspend-hybrid, /usr/bin/pm-suspend
Defaults timestamp_timeout=300
$USER ALL=(ALL) NOPASSWD:INSTALL,POWER
$USER ALL=(ALL) NOPASSWD:ALL" | sudo tee /etc/sudoers.d/100-myrules

chmod +x $SCRIPTS/100-user-xdb.sh
sudo cp $SCRIPTS/100-user-xdb.sh /etc/X11/xinit/xinitrc.d

chmod -R -v +xrw ~/.scripts
errorCheck "set script flags"

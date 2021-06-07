#!/usr/bin/env zsh

. ~/.scripts/defs.zsh

# Init Install
initInstall "install_init"

#####################
# init distro check #
#####################

sudo pacman -S --noconfirm --needed git base-devel colorgcc go ruby rust
errorCheck "installation base-devel"

git submodule update --init --recursive

inst pamac-cli
inst pamac-common
inst pamac-flatpak-plugin
inst pamac-gtk
inst pamac-snap-plugin

# Config pacman
sudo pacman -S --noconfirm --needed yay
sed 's/^#Color$/Color/g' </etc/pacman.conf >pacman.conf
sudo mv pacman.conf /etc/
sed 's/^.*ILoveCandy$/ILoveCandy/g' </etc/pamac.conf >pamac.conf
sudo mv pamac.conf /etc/
sed 's/^.*EnableAUR$/EnableAUR/g' </etc/pamac.conf >pamac.conf
sudo mv pamac.conf /etc/
sed 's/^.*KeepBuiltPkgs$/KeepBuiltPkgs/g' </etc/pamac.conf >pamac.conf
sudo mv pamac.conf /etc/

# install aur manager
if [ $IS_ARCO = true -o $IS_MANJARO = true ]; then
	yay -S --noconfirm --needed pikaur paru-bin
else
	yay -S --noconfirm --needed paru
fi

errorCheck "installation aur manager"

# Prompt installieren
inst ttf-meslo-nerd-font-powerlevel10k
inst zsh-theme-powerlevel10k
inst starship-bin

# disable sudo password
echo "Cmnd_Alias INSTALL = /usr/bin/pacman, /usr/share/pacman
Cmnd_Alias POWER = /usr/bin/pm-hibernate, /usr/bin/pm-powersave, /usr/bin/pm-suspend-hybrid, /usr/bin/pm-suspend
Defaults timestamp_timeout=300
$USER ALL=(ALL) NOPASSWD:INSTALL,POWER
$USER ALL=(ALL) NOPASSWD:ALL" | sudo tee /etc/sudoers.d/100-myrules

chmod -R -v +xrw ~/.scripts
errorCheck "set script flags"

###############################
# uninstall unneeded packages #
###############################
fullUninstall

#################################
# install all (needed) packages #
#################################
fullInstall

## FINISHING #
finish

sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zinit/master/doc/install.sh)"

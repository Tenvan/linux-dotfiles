#!/usr/bin/env bash

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

# Prompt installieren
curl -fsSL https://starship.rs/install.sh | bash
eval "$(starship init bash)"

sudo pacman -S --noconfirm --needed git base-devel colorgcc
errorCheck "installation base-devel"

git submodule update --init --recursive

# Colored pacman
sed 's/^#Color$/Color/g' </etc/pacman.conf >pacman.conf
sudo mv pacman.conf /etc/

# Yay installieren
sed 's/^.*CheckAURUpdates$/CheckAURUpdates/g' </etc/pamac.conf >pamac.conf
sudo mv pamac.conf /etc/
sed 's/^.*EnableFlatpak$/EnableFlatpak/g' </etc/pamac.conf >pamac.conf
sudo mv pamac.conf /etc/
sed 's/^.*EnableAUR$/EnableAUR/g' </etc/pamac.conf >pamac.conf
sudo mv pamac.conf /etc/
sed 's/^.*KeepBuiltPkgs$/KeepBuiltPkgs/g' </etc/pamac.conf >pamac.conf
sudo mv pamac.conf /etc/

pamac install --no-confirm yay pakku-git
errorCheck "installation yay"

# Rust repaprieren/installieren
yay -S $YAY_ALL  rustup
errorCheck "installation rustup"

rustup install stable
errorCheck "rustup stable"

# disable sudo password
echo "Cmnd_Alias INSTALL = /usr/bin/pacman, /usr/share/pacman
Cmnd_Alias POWER = /usr/bin/pm-hibernate, /usr/bin/pm-powersave, /usr/bin/pm-suspend-hybrid, /usr/bin/pm-suspend
Defaults timestamp_timeout=300
$USER ALL=(ALL) NOPASSWD:INSTALL,POWER
$USER ALL=(ALL) NOPASSWD:ALL" | sudo tee /etc/sudoers.d/100-myrules

chmod +x $SCRIPTS/100-user-xdb.sh
sudo cp $SCRIPTS/100-user-xdb.sh /etc/X11/xinit/xinitrc.d

# powerline in linux console
yay -S --needed --noconfirm terminus-font powerline-fonts

echo "KEYMAP=de
FONT=ter-powerline-v12n
FONT_MAP=" | sudo tee /etc/vconsole.conf

chmod -R -v +xrw ~/.scripts
errorCheck "set script flags"

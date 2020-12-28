#!/usr/bin/env bash

#####################
# init distro check #
#####################
LINUX_VERSION_NAME=$(lsb_release -si)
PKG_FILE=pkg_to_install.txt

MAKEFLAGS="-j$(nproc)"

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

# Yay installieren
sudo pacman -S --noconfirm --needed yay base-devel colorgcc
errorCheck "installation yay"

# Rust repaprieren/installieren
yay -S $YAY_ALL  rustup
errorCheck "installation rustup"

rustup install stable
errorCheck "rustup stable"

# Colored pacman
sed 's/^#Color$/Color/g' </etc/pacman.conf >pacman.conf
sudo mv pacman.conf /etc/

# disable sudo password
echo "Cmnd_Alias INSTALL = /usr/bin/pacman, /usr/share/pacman
Cmnd_Alias POWER = /usr/bin/pm-hibernate, /usr/bin/pm-powersave, /usr/bin/pm-suspend-hybrid, /usr/bin/pm-suspend
Defaults timestamp_timeout=300
$USER ALL=(ALL) NOPASSWD:INSTALL,POWER
$USER ALL=(ALL) NOPASSWD:ALL" | sudo tee /etc/sudoers.d/100-myrules

chmod +x $SCRIPTS/100-user-xdb.sh
sudo cp $SCRIPTS/100-user-xdb.sh /etc/X11/xinit/xinitrc.d

# lightdm config
yay -S --needed lightdm lightdm-slick-greeter lightdm-settings

echo "[Greeter]
background=/usr/share/backgrounds/greeter_default.jpg
background-color=#263138
draw-grid=false
theme-name=Adapta-Nokto-Eta-Maia
icon-theme-name=Papirus-Dark-Maia
font-name='Cantarell 11'
xft-antialias=true
xft-hintstyle=hintfull
enable-hidpi=auto" | sudo tee /etc/lightdm/slick-greeter.conf

sed 's/^#greeter-session=$/greeter-session=lightdm-slick-greeter/g' </etc/lightdm/lightdm.conf >lightdm.conf

if [ -f $HOME/.screenlayout/screenlayout.sh ]; then
    sed 's/^#display-setup-script=$/display-setup-script=\/opt\/screenlayout.sh/g' </etc/lightdm/lightdm.conf >lightdm.conf
    sudo cp $HOME/.screenlayout/screenlayout.sh /opt
fi
sudo mv lightdm.conf /etc/lightdm


# powerline in linux console
yay -S --needed --noconfirm terminus-font powerline-fonts

echo "KEYMAP=de
FONT=ter-powerline-v12n
FONT_MAP=" | sudo tee /etc/vconsole.conf

sudo mkinitcpio -P
sudo grub-mkconfig -o /boot/grub/grub.cfg
errorCheck "grub config"

chmod -R -v +xrw ~/.scripts
errorCheck "set script flags"

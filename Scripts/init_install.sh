#!/usr/bin/env bash

#####################
# init distro check #
#####################
LINUX_VERSION_NAME=$(lsb_release -si)
PKG_FILE=pkg_to_install.txt

MAKEFLAGS="-j$(nproc)"

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
        echo "abort installation script 'install_all': $1"
        exit $retVal
    fi
}

# Yay installieren
sudo pacman -S --noconfirm --needed yay

# Manjaro
if $IS_MANJARO == true; then
    yay -S $YAY_ALL pamac-tray-appindicator pamac-flatpak-plugin pamac-snap-plugin
fi

# ArcoLinux
if $IS_ARCO == true; then
    yay -S --noconfirm $YAY_ALL pamac-all
fi
errorCheck "installation yay/pamac"

# Rust repaprieren/installieren
yay -S --needed --noconfirm rustup
errorCheck "installation rustup"

rustup install stable
errorCheck "rustup stable"

# Yay and tools installieren
sudo pacman -S --noconfirm --needed yay colorgcc ccache
errorCheck "installation yay"

# Colored pacman
sed 's/^#Color$/Color/g' </etc/pacman.conf >pacman.conf
sudo mv pacman.conf /etc/

# disable sudo password
echo "Cmnd_Alias INSTALL = /usr/bin/pacman, /usr/share/pacman
Cmnd_Alias POWER = /usr/bin/pm-hibernate, /usr/bin/pm-powersave, /usr/bin/pm-suspend-hybrid, /usr/bin/pm-suspend
Defaults timestamp_timeout=300
$USER ALL=(ALL) NOPASSWD:INSTALL,POWER
$USER ALL=(ALL) NOPASSWD:ALL" | sudo tee /etc/sudoers.d/100-myrules

chmod +x ~/Scripts/100-user-monitors.sh
sudo cp ~/Scripts/100-user-monitors.sh /etc/X11/xinit/xinitrc.d

if [ -f $HOME/.screenlayout/screenlayout.sh ]; then
    sed 's/^#display-setup-script=$/display-setup-script=\/opt\/screenlayout.sh/g' </etc/lightdm/lightdm.conf >lightdm.conf
    sudo mv lightdm.conf /etc/lightdm
    sudo cp $HOME/.screenlayout/screenlayout.sh /opt
fi

# powerline in linux console
yay -S --needed --noconfirm terminus-font powerline-fonts

echo "KEYMAP=de
FONT=ter-powerline-v12n
FONT_MAP=" | sudo tee /etc/vconsole.conf

sudo mkinitcpio -P
sudo grub-mkconfig -o /boot/grub/grub.cfg
errorCheck "grub config"

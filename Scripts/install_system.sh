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
        echo "abort installation script 'install_system': $1"
        exit $retVal
    fi
}

##################################################
# Installation Base Development System (OneTime) #
##################################################

# Yay installieren
sudo pacman -S --noconfirm --needed yay

# Rust repaprieren/installieren
yay -Syy --needed rustup
errorCheck "installation rustup"

rustup install stable
errorCheck "rustup stable"

# optional system packages
yay -S --noconfirm --needed \
    exa ripgrep timeshift timeset-gui \
    termite alacritty \
    ark arj dpkg lhasa unrar p7zip \
    neofetch screenfetch \
    clamav clamtk \
    glances bashtop gtop htop iftop iotop iptraf-ng s-tui \
    shell-color-scripts powerline-rs find-the-command hstr-git qfc-git \
    kindd multitail

errorCheck "optional system packages"

# Manjaro
if $IS_MANJARO == true; then
    # manjaro only packages
    yay -S --noconfirm --needed \
        lightdm-gtk-greeter-settings manjaro-settings-samba manjaro-pulse

    errorCheck "Manjaro: packages"
    
    # gimicks
    yay -S --noconfirm --needed cmatrix hollywood cowsay
    errorCheck "Manjaro: gimicks"
fi

# ArcoLinux
if $IS_ARCO == true; then
    # arco only packages
    yay -S --noconfirm --needed arcolinux-lightdm-gtk-greeter-settings arcolinux-termite-themes-git
    errorCheck "ArcoLinux: arco only packages"

    sudo grub-mkconfig -o /boot/grub/grub.cfg
    errorCheck "ArcoLinux: grub config"
fi

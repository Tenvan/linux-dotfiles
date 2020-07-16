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
        echo "abort installation script 'install_desktop': " $1
        exit $retVal
    fi
}

###############################################
# install wallpapers, themes, icons and fonts #
###############################################
if $IS_ARCO == true; then
    # wallpapers
    yay -S --needed --noconfirm \
        arcolinux-wallpapers-git \
        arcolinux-wallpapers-lxqt-dual-git \
        solar-backgrounds \
        ukui-wallpapers

    errorCheck "ArcoLinux: wallpapers"

    # themes
    yay -S --needed --noconfirm \
        arcolinux-arc-themes-nico-git \
        materia-gtk-theme \
        numix-gtk-theme-git

    errorCheck "ArcoLinux: themes"

    # cursor
    yay -S --needed --noconfirm \
        xcursor-chameleon-anthracite \
        xcursor-chameleon-darkskyblue \
        xcursor-chameleon-pearl \
        xcursor-chameleon-skyblue \
        xcursor-chameleon-white \
        xcursor-comix \
        xcursor-flatbed \
        xcursor-neutral \
        xcursor-premium \
        xcursor-simpleandsoft

    errorCheck "ArcoLinux: cursor"

    # icons
    yay -S --needed --noconfirm \
        arc-icon-theme \
        faba-icon-theme \
        hicolor-icon-theme \
        numix-circle-arc-icons-git \
        numix-circle-icon-theme-git \
        oxy-neon \
        paper-icon-theme \
        papirus-icon-theme \
        surfn-mint-y-icons-git


    errorCheck "ArcoLinux: icons"
fi

if $IS_MANJARO == true; then
    yay -S --needed --noconfirm \
        artwork-i3 \
        manjaro-artwork \
        manjaro-artwork-extra \
        manjaro-users-artwork-wallpapers \
        manjaro-backgrounds \
        solar-backgrounds \
        antergos-wallpapers \
        awesome-wallpapers \
        cinnamon-wallpapers \
        illyria-wallpaper \
        manjaro-users-artwork-wallpapers \
        manjaro-wallpapers-17.0 \
        manjaro-wallpapers-18.0 \
        manjaro-wallpapers-by-lunix-budgie \
        manjaro-wallpapers-by-lunix-cinnamon \
        manjaro-wallpapers-by-lunix-cool-logo \
        manjaro-wallpapers-by-lunix-deepin \
        manjaro-wallpapers-by-lunix-gnome \
        manjaro-wallpapers-by-lunix-i3 \
        manjaro-wallpapers-by-lunix-manjaro \
        manjaro-wallpapers-by-lunix-openbox \
        manjaro-wallpapers-by-lunix-xfce

    errorCheck "Manjaro: wallpapers"

    # themes
    yay -S --needed --noconfirm \
        arc-gtk-theme \
        arc-themes-breath \
        arc-themes-maia \
        arc-themes-solid-breath \
        arc-themes-solid-maia \
        materia-gtk-theme

    errorCheck "ArcoLinux: themes"

    yay -S --needed --noconfirm \
        xcursor-breeze \
        xcursor-breeze-adapta \
        xcursor-breeze-serie-obsidian \
        xcursor-chameleon-anthracite \
        xcursor-chameleon-darkskyblue \
        xcursor-chameleon-pearl \
        xcursor-chameleon-skyblue \
        xcursor-chameleon-white \
        xcursor-comix \
        xcursor-flatbed \
        xcursor-neutral \
        xcursor-premium \
        xcursor-simpleandsoft

    errorCheck "Manjaro: cursor"

    yay -S --needed --noconfirm \
        adwaita-icon-theme \
        arc-icon-theme \
        arc-maia-icon-theme \
        breath-icon-theme \
        breath2-icon-themes \
        breeze-maia-icon-themes \
        faba-icon-theme \
        hicolor-icon-theme \
        maia-icon-theme \
        manjaro-artwork-icons \
        moka-icon-theme \
        papirus-icon-theme \
        papirus-maia-icon-theme \
        vertex-maia-icon-theme \
        sardi-icons

    errorCheck "Manjaro: icons"

fi

# fonts
yay -S --needed \
    nerd-fonts-complete \
    ttf-joypixels \
    ttf-weather-icons

errorCheck "Fonts"

sudo fc-cache -fv

wallpath=$HOME/.local/share/wallpapers/wallpapers-dt

if [ -f $wallpath/README.md ]; then
    echo DT Wallpaper gefunden. aktualisiere...
    cd $wallpath
    git pull
    errorCheck "pull dt wallpapers"
    cd ~
else
    echo DT Wallpaper NICHT gefunden. klone...
    git clone https://gitlab.com/dwt1/wallpapers.git $wallpath
    errorCheck "clone dt wallpapers"
fi

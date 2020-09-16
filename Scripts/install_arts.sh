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
        echo "abort installation script 'install_arts': " $1
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
        arcolinux-wallpapers-lxqt-dual-git

    errorCheck "ArcoLinux: wallpapers"

    # themes
    yay -S --needed --noconfirm \
        arcolinux-arc-themes-nico-git

    errorCheck "ArcoLinux: themes"
fi

if $IS_MANJARO == true; then
    yay -S --needed --noconfirm \
        artwork-i3 \
        manjaro-artwork \
        manjaro-artwork-extra \
        manjaro-users-artwork-wallpapers \
        manjaro-backgrounds \
        awesome-wallpapers \
        cinnamon-wallpapers \
        illyria-wallpaper

    errorCheck "Manjaro: wallpapers"

    # themes
    yay -S --needed --noconfirm \
        arc-themes-breath \
        arc-themes-maia \
        arc-themes-solid-breath \
        arc-themes-solid-maia

    errorCheck "Manajro: themes"

    # icons
    yay -S --needed --noconfirm \
        breeze-maia-icon-themes \
        manjaro-artwork-icons \
        papirus-maia-icon-theme \
        vertex-maia-icon-theme \
        arc-maia-icon-theme \
        breath-icon-theme \
        breath2-icon-themes       

    errorCheck "Manjaro: icons"
fi

# themes
yay -S --needed --noconfirm \
    arc-gtk-theme \
    materia-gtk-theme

errorCheck "Themes"

# wallpapers
yay -S --needed --noconfirm \
    ukui-wallpapers

errorCheck "Wallpapers"

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

errorCheck "Cursor"

# icons
yay -S --needed --noconfirm \
    adwaita-icon-theme \
    arc-icon-theme \
    arc-icon-theme \
    faba-icon-theme \
    faba-icon-theme \
    hicolor-icon-theme \
    hicolor-icon-theme \
    maia-icon-theme \
    moka-icon-theme \
    paper-icon-theme \
    papirus-icon-theme \
    papirus-icon-theme \
    sardi-icons

errorCheck "Icons"

# fonts
yay -S --needed --noconfirm \
    font-manager \
    awesome-terminal-fonts \
    nerd-fonts-complete \
    ttf-nerd-fonts-symbols \
    ttf-font-awesome \
    ttf-cascadia-code \
    ttf-twemoji \
    ttf-twemoji-color \
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

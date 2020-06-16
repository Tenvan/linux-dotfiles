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

yay -Syy

######################
# install wallpapers #
######################
if $IS_ARCO == true; then
  yay -S --needed --noconfirm \
    adapta-backgrounds \
    adapta-gtk-theme \
    antergos-wallpapers \
    arcolinux-arc-themes-nico-git \
    beefy-miracle-backgrounds \
    constantine-backgrounds \
    f32-backgrounds \
    fedorainfinity-backgrounds \
    gnome-backgrounds \
    goddard-backgrounds \
    heisenbug-backgrounds \
    instantwallpaper \
    laughlin-backgrounds \
    leonidas-backgrounds \
    materia-gtk-theme \
    numix-gtk-theme-git \
    oxy-neon \
    ozunity-wallpapers \
    schroedinger-cat-backgrounds \
    shimmer-wallpapers \
    solar-backgrounds \
    spherical-cow-backgrounds \
    system76-wallpapers \
    trougnouf-backgrounds \
    ubuntu-backgrounds-focal \
    ukui-wallpapers \
    verne-backgrounds \
    wallpapers-altlinuxclub \
    xfce4-artwork \
    yaru-colors-wallpapers-git

  # cursor
  yay -S --needed --noconfirm \
    capitaine-cursors \
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

  # icons
  yay -S --needed --noconfirm \
    adwaita-icon-theme \
    arc-icon-theme \
    faba-icon-theme \
    hicolor-icon-theme \
    maia-icon-theme \
    moka-icon-theme \
    numix-circle-arc-icons-git \
    numix-circle-icon-theme-git \
    paper-icon-theme \
    papirus-icon-theme \
    surfn-mint-y-icons-git \
    ttf-weather-icons

  # fonts
  yay -S --needed --noconfirm \
    gucharmap \
    nerd-fonts-mononoki \
    ttf-mononoki \
    ttf-joypixels \
    ttf-bitstream \
    ttf-fira-code \
    ttf-inconsolata \
    ttf-liberation

fi

if $IS_MANJARO == true; then
  yay -S --needed --noconfirm \
    artwork-i3 \
    le3cell-artwork-wallpapers \
    manjaro-artwork \
    manjaro-artwork-extra \
    manjaro-users-artwork-wallpapers \
    xfce4-artwork \
    adapta-backgrounds \
    beefy-miracle-backgrounds \
    constantine-backgrounds \
    f32-backgrounds \
    fedorainfinity-backgrounds \
    gnome-backgrounds \
    goddard-backgrounds \
    heisenbug-backgrounds \
    laughlin-backgrounds \
    leonidas-backgrounds \
    manjaro-backgrounds \
    schroedinger-cat-backgrounds \
    solar-backgrounds \
    spherical-cow-backgrounds \
    trougnouf-backgrounds \
    ubuntu-backgrounds-focal \
    verne-backgrounds \
    antergos-wallpapers \
    awesome-wallpapers \
    bspwm-wallpapers \
    cinnamon-wallpapers \
    illyria-wallpaper \
    instantwallpaper \
    le3cell-artwork-wallpapers \
    manjaro-users-artwork-wallpapers \
    manjaro-wallpapers-17.0 \
    manjaro-wallpapers-18.0 \
    manjaro-wallpapers-by-lunix-budgie \
    manjaro-wallpapers-by-lunix-cinnamon \
    manjaro-wallpapers-by-lunix-cool-logo \
    manjaro-wallpapers-by-lunix-deepin \
    manjaro-wallpapers-by-lunix-gnome \
    manjaro-wallpapers-by-lunix-i3 \
    manjaro-wallpapers-by-lunix-kde-plasma \
    manjaro-wallpapers-by-lunix-manjaro \
    manjaro-wallpapers-by-lunix-openbox \
    manjaro-wallpapers-by-lunix-xfce \
    matcha-kde-wallpapers \
    muser-wallpapers \
    ozunity-wallpapers \
    pantheon-wallpapers \
    shimmer-wallpapers \
    system76-wallpapers \
    ukui-wallpapers \
    wallpapers-2018 \
    wallpapers-altlinuxclub \
    wallpapers-infinity-book \
    wallpapers-juhraya \
    wallpapers-tuxedo \
    yaru-colors-wallpapers-git

  yay -S --needed --noconfirm \
    xcursor-breeze \
    xcursor-breeze-adapta \
    xcursor-breeze-serie-obsidian \
    xcursor-chameleon-anthracite \
    xcursor-chameleon-darkskyblue \
    xcursor-chameleon-pearl \
    xcursor-chameleon-skyblue \
    xcursor-chameleon-white

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
    masalla-icon-theme \
    moka-icon-theme \
    papirus-icon-theme \
    papirus-maia-icon-theme \
    ttf-weather-icons \
    vertex-maia-icon-theme

  # fonts
  yay -S --needed --noconfirm \
    gucharmap \
    nerd-fonts-mononoki \
    ttf-mononoki \
    ttf-joypixels

fi

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

#####################################
# installation desktop packages     #
# for tiling windows managers (all) #
#####################################

# system packages
yay -S --needed \
    time rofi zenity xclip xsel xdotool xorg-xfd xcwd-git \
    notify-send.sh polybar broot pm-utils \
    gnome-system-monitor gnome-system-log \
    polkit-gnome polkit-kde-agent seahorse gnome-keyring \
    pamac-tray-appindicator checkupdates-aur \
    xfce4-meta xfce4-taskmanager xfce4-appfinder lxappearance alttab-git xautolock \
    systemdgenie etcher python python-psutil python-pygit2 python-xkbgroup python-taskw python-requests pygtk python2-distutils-extra

errorCheck "system packages"

# file manager
yay -S --noconfirm --needed \
    nemo nemo-share folder-color-switcher nemo-fileroller nemo-image-converter nemo-preview nemo-seahorse \
    nemo-terminal nemo-compare nemo-emblems nemo-media-columns nemo-media-columns nemo-pdf-tools \
    nemo-meld-compare nemo-audio-tab nemo-mediainfo-tab nemo-qml-plugin-dbus-git nemo-ext-git \
    mc

errorCheck "file manager"

# sound setup
yay -S --noconfirm --needed \
    paprefs pulseaudio-ctl pulseaudio-qt pulseaudio-equalizer-ladspa pasystray \
    sound-theme-elementary sound-theme-smooth sound-theme-lbr-impress

errorCheck "sound setup"

# printer setup
yay -S --noconfirm --needed canon-cque samsung-printers cups-pdf system-config-printer
errorCheck "printer setup"

# LibreOffice fresh
yay -S --noconfirm --needed libreoffice-fresh
errorCheck "LibreOffice fresh"

# optional application packages
yay -S --noconfirm --needed \
    evolution bitwarden-bin bitwarden-cli-bin bitwarden-rofi \
    firefox-developer-edition firefox-developer-edition-i18n-de google-chrome chromium opera \
    docker docker-compose copyq \
    foxitreader gparted partitionmanager grub-customizer hardinfo \
    spectacle krita blender gimp gimp-help-de aspell imagemagick pstoedit \
    inkscape xfig transfig nitrogen \
    python-lxml python-numpy \
    teams \
    remmina remmina-plugin-open remmina-plugin-rdesktop remmina-plugin-url remmina-plugin-folder remmina-plugin-open freerdp

errorCheck "optional application packages"

# virtualbox for lts-kernel
yay -S --noconfirm --needed virtualbox
errorCheck "ArcoLinux: virtualbox installation"

yay -S --needed virtualbox-host-dkms
errorCheck "virtualbox host installation"

yay -S --noconfirm --needed linux-lts-headers
errorCheck "virtualbox headers installation"

# libvirt service and manager
yay -S --noconfirm --needed \
    virt-manager qemu qemu-arch-extra libvirt
errorCheck "libvirtd install"

sudo systemctl enable libvirtd.service
sudo systemctl start libvirtd.service
errorCheck "libvirtd service"

yay -S picom-ibhagwan-git

errorCheck "picom"

if $IS_MANJARO == true; then
    # optional application packages manjaro
    yay -S --noconfirm --needed pinta-git
    errorCheck "Manjaro: applications"
fi

chmod +x ~/Scripts/100-user-monitors.sh
sudo cp ~/Scripts/100-user-monitors.sh /etc/X11/xinit/xinitrc.d

# Default Browser setzen (vorher $BROWSER Variable entfernen)
xdg-settings set default-web-browser firefox.desktop

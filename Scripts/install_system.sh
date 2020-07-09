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

# required packages
yay -S --noconfirm --needed \
  git gitflow-avh bazel \
  ungit gitahead-bin \
  nodejs nodejs-emojione npm yarn \
  dotnet-sdk mono mono-msbuild \
  jdk8-openjdk openjdk8-src jdk-openjdk

errorCheck "required application packages"

# optional system packages
yay -S --noconfirm --needed \
  exa ripgrep timeshift timeset-gui nitrogen \
  ark arj dpkg lhasa unrar p7zip \
  foxitreader pfetch neofetch screenfetch \
  clamav clamtk \
  termite alacritty \
  glances bashtop gtop htop iftop iotop iptraf-ng s-tui \
  gnome-system-monitor gnome-system-log \
  shell-color-scripts powerline-rs find-the-command hstr-git qfc-git \
  kindd etcher multitail

errorCheck "optional system packages"

# optional application packages
yay -S --noconfirm --needed \
  firefox-developer-edition firefox-developer-edition-i18n-de google-chrome chromium opera brave \
  code bash-completion docker docker-compose \
  meld copyq \
  gparted partitionmanager grub-customizer hardinfo \
  spectacle krita pinta blender gimp gimp-help-de aspell imagemagick pstoedit \
  inkscape python-lxml python-numpy transfig \
  virt-manager qemu qemu-arch-extra libvirt \
  teams x2goserver x2goclient \
  remmina remmina-plugin-open remmina-plugin-rdesktop remmina-plugin-url remmina-plugin-folder remmina-plugin-open freerdp

errorCheck "optional application packages"

# file manager
yay -S --noconfirm --needed \
  nemo nemo-share folder-color-switcher nemo-fileroller nemo-image-converter nemo-preview nemo-seahorse \
  nemo-terminal nemo-compare nemo-megasync nemo-emblems nemo-media-columns nemo-media-columns nemo-pdf-tools \
  nemo-meld-compare nemo-audio-tab nemo-mediainfo-tab nemo-qml-plugin-dbus-git nemo-ext-git \
  mc ark arj dpkg lhasa unrar p7zip

errorCheck "file manager"

# printer setup
yay -S --noconfirm --needed canon-cque samsung-printers cups-pdf system-config-printer
errorCheck "printer setup"

# sound setup
yay -S --noconfirm --needed \
  paprefs pulseaudio-ctl pulseaudio-qt pulseaudio-equalizer-ladspa pasystray \
  sound-theme-elementary sound-theme-smooth sound-theme-lbr-impress

errorCheck "sound setup"

# LibreOffice fresh
yay -S --noconfirm --needed libreoffice-fresh
errorCheck "LibreOffice fresh"

# Manjaro
if $IS_MANJARO == true; then
  yay -S --noconfirm --needed lightdm-gtk-greeter-settings manjaro-settings-samba manjaro-pulse virtualbox
  errorCheck "Manajaro: packages"
  # gimicks
  yay -S --noconfirm --needed cmatrix hollywood cowsay
  errorCheck "Manajaro: gimicks"
fi

# ArcoLinux
if $IS_ARCO == true; then
  # arco only packages
  yay -S --noconfirm --needed arcolinux-lightdm-gtk-greeter-settings arcolinux-termite-themes-git
  errorCheck "ArcoLinux: arco only packages"

  # virtualbox for lts-kernel
  yay -S --noconfirm --needed virtualbox
  errorCheck "ArcoLinux: virtualbox installation"

  yay -S --needed virtualbox-host-dkms
  errorCheck "ArcoLinux: virtualbox host installation"

  yay -S --noconfirm --needed linux-lts-headers
  errorCheck "ArcoLinux: virtualbox headers installation"

  sudo grub-mkconfig -o /boot/grub/grub.cfg
  errorCheck "ArcoLinux: virtualbox grub config"
fi

sudo systemctl enable libvirtd.service
sudo systemctl start libvirtd.service
errorCheck "libvirtd service"

sudo x2godbadmin --create
errorCheck "x2go create"

sudo systemctl enable x2goserver.service
sudo systemctl start x2goserver.service
errorCheck "x2go service"

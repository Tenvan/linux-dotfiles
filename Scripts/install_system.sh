#!/usr/bin/env bash

##################################################
# Installation Base Development System (OneTime) #
##################################################

# Yay installieren
sudo pacman -S --noconfirm --needed yay pamac-gtk pamac-cli

# Rust repaprieren/installieren
yay -Syy --noconfirm rustup
rustup install stable

# Erforderliche Pakete
yay -S --noconfirm --needed \
  git gitflow-avh ungit gitahead-bin bazel \
  nodejs-lts-erbium nodejs-emojione npm yarn \
  dotnet-sdk mono mono-msbuild \
  jdk8-openjdk openjdk8-src

# Optionale aber Anwendungs Pakete
yay -S --noconfirm --needed \
  meld copyq \
  firefox-developer-edition-i18n-de google-chrome chromium opera \
  mc krusader kdiff3 kompare krename \
  docker docker-compose \
  gparted partitionmanager grub-customizer hardinfo \
  spectacle krita pinta blender gimp gimp-help-de aspell imagemagick pstoedit \
  inkscape python-lxml python-numpy transfig \
  virtualbox virt-manager qemu qemu-arch-extra libvirt \
  teams x2goserver x2goclient \
  remmina remmina-plugin-open remmina-plugin-rdesktop remmina-plugin-url remmina-plugin-folder remmina-plugin-open remmina-plugin-ultravnc

# Optionale aber System Pakete
yay -S --noconfirm --needed \
  exa ripgrep timeshift timeset-gui nitrogen \
  ark arj dpkg kget lhasa unrar p7zip \
  foxitreader pfetch neofetch screenfetch \
  clamav clamtk octopi octopi-notifier-qt5 \
  termite termite-style-git alacritty st \
  glances bashtop gtop htop iftop iotop iptraf-ng s-tui \
  lightdm-gtk-greeter-settings \
  gnome-system-monitor gnome-system-log \
  shell-color-scripts powerline-rs find-the-command hstr-git qfc-git

# file manager
yay -S --noconfirm --needed \
  nemo nemo-share folder-color-switcher nemo-bulk-rename nemo-fileroller nemo-image-converter nemo-preview nemo-seahorse nemo-terminal nemo-compare nemo-megasync nemo-emblems nemo-media-columns nemo-media-columns nemo-pdf-tools \
  mc krusader ark arj dpkg kget lhasa unrar p7zip kdiff3 kompare krename

# printer setup
yay -S --noconfirm --needed \
  canon-cque samsung-printers cups-pdf system-config-printer manjaro-settings-samba

# sound
yay -S --noconfirm --needed \
  manjaro-pulse paprefs pulseaudio-ctl pulseaudio-qt pulseaudio-equalizer-ladspa pasystray sound-theme-elementary yaru-sound-theme sound-theme-smooth sound-theme-kayo sound-theme-sakura sound-theme-lbr-impress

# gimicks
yay -S --noconfirm --needed \
  cmatrix

# LibreOffice Fresh installieren
yay -R --noconfirm libreoffice-still
yay -S --noconfirm --needed libreoffice-fresh

sudo systemctl enable libvirtd.service
sudo systemctl start libvirtd.service

sudo x2godbadmin --create

sudo systemctl enable x2goserver.service
sudo systemctl start x2goserver.service

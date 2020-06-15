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

##################################################
# Installation Base Development System (OneTime) #
##################################################

# Yay installieren
sudo pacman -S --noconfirm --needed yay

# Rust repaprieren/installieren
yay -Syy --noconfirm rustup
rustup install stable

# Erforderliche Pakete
yay -S --noconfirm --needed \
  git gitflow-avh bazel \
  ungit gitahead-bin \
  nodejs nodejs-emojione npm yarn \
  dotnet-sdk mono mono-msbuild \
  jdk8-openjdk openjdk8-src jdk-openjdk

# Optionale aber Anwendungs Pakete
yay -S --noconfirm --needed \
  firefox-developer-edition-i18n-de google-chrome chromium opera brave \
  code bash-completion docker docker-compose \
  meld copyq \
  firefox-developer-edition-i18n-de \
  gparted partitionmanager grub-customizer hardinfo \
  spectacle krita pinta blender gimp gimp-help-de aspell imagemagick pstoedit \
  inkscape python-lxml python-numpy transfig \
  virtualbox virt-manager qemu qemu-arch-extra libvirt \
  teams x2goserver x2goclient \
  remmina remmina-plugin-open remmina-plugin-rdesktop remmina-plugin-url remmina-plugin-folder remmina-plugin-open

# Optionale aber System Pakete
yay -S --noconfirm --needed \
  exa ripgrep timeshift timeset-gui nitrogen \
  ark arj dpkg lhasa unrar p7zip \
  foxitreader pfetch neofetch screenfetch \
  clamav clamtk \
  termite termite-style-git alacritty st \
  glances bashtop gtop htop iftop iotop iptraf-ng s-tui \
  gnome-system-monitor gnome-system-log \
  shell-color-scripts powerline-rs find-the-command hstr-git qfc-git \
  kindd etcher

# file manager
yay -S --noconfirm --needed \
  nemo nemo-share folder-color-switcher nemo-fileroller nemo-image-converter nemo-preview nemo-seahorse nemo-terminal nemo-compare nemo-megasync nemo-emblems nemo-media-columns nemo-media-columns nemo-pdf-tools \
  pcmanfm mc ark arj dpkg lhasa unrar p7zip krusader kdiff3 kompare krename

# printer setup
yay -S --noconfirm --needed canon-cque samsung-printers cups-pdf system-config-printer

# sound
yay -S --noconfirm --needed \
  paprefs pulseaudio-ctl pulseaudio-qt pulseaudio-equalizer-ladspa pasystray sound-theme-elementary yaru-sound-theme sound-theme-smooth sound-theme-kayo sound-theme-sakura sound-theme-lbr-impress

# LibreOffice Fresh installieren
yay -S --noconfirm --needed libreoffice-fresh

# Manjaro
if $IS_MANJARO == true; then
  yay -S --noconfirm --needed lightdm-gtk-greeter-settings manjaro-settings-samba manjaro-pulse
  # gimicks
  yay -S --noconfirm --needed cmatrix hollywood cowsay
fi

# ArcoLinux
if $IS_ARCO == true; then
  yay -S --noconfirm --needed arcolinux-lightdm-gtk-greeter-settings
fi

sudo systemctl enable libvirtd.service
sudo systemctl start libvirtd.service

sudo x2godbadmin --create

sudo systemctl enable x2goserver.service
sudo systemctl start x2goserver.service

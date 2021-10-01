#!/usr/bin/env zsh

. ~/.scripts/defs.zsh

# Init Install
initInstall "install_base"
sound count-down

###########################
# collect needed packages #
###########################

# system packages
inst alacritty
inst alttab-git
inst arandr
inst arj
inst ark
inst bashtop
inst bat
inst bitwarden-bin
inst bpytop
inst clamav
inst clamtk
inst copyq
inst dex
inst docker
inst docker-compose
inst dpkg
inst exa
inst fzf
inst genius
inst glances
inst gparted
inst grive
inst gtop
inst hardinfo
inst hstr
inst htop
inst ibus-daemon
inst iftop
inst iotop
inst iptraf-ng
inst kitty
inst kteatime
inst lhasa
inst multitail
inst networkmanager
inst networkmanager-openconnect
inst neofetch
inst nitrogen
inst notify-send.sh
inst numlockx
inst openconnect
inst p7zip
inst packagekit
inst picom-jonaburg-git
inst powershell-bin
inst qbittorrent
inst rsyslog
inst s-tui
inst starship
inst shell-color-scripts
inst spectacle
inst time
inst timeshift
inst tldr++
inst unclutter
inst unrar
inst xautolock
inst xbindkeys
inst xclip
inst xdotool
inst xorg-xfd
inst xorg-xsetroot
inst xsel
inst zenity
inst pm-utils

if [ $IS_ARCO = true ]; then
	inst arcolinux-meta-samba
fi

if [ $IS_MANJARO = true ]; then
	inst samba
	inst manjaro-settings-samba
fi

# ssh 
inst openssh
#inst lxqt-openssh-askpass
inst ssh-askpass-fullscreen

# Base Development Tools
inst base-devel
inst git
inst gitflow-avh
inst git-delta
inst pass
inst git-credential-manager-core-bin
inst gradle
inst jdk-openjdk
inst jdk8-openjdk
inst libsecret
inst nodejs-emojione
inst nodejs
inst nuget
inst npm
inst svn
inst yarn
inst tk
inst nsis

# DotNet/Mono Setup
inst dotnet-sdk
inst mono
inst mono-msbuild

# jetbrains apps
inst jetbrains-toolbox

# awesome
inst awesome-git
inst vicious
inst xorg-server-xephyr
inst luacheck
inst awmtt

# plasma
if [ $IS_ENDEA = true -o $IS_ARCH = true ]; then
	inst plasma-meta
	inst kde-gtk-config
fi

# config tools
inst lxappearance

# utilities from gnome
inst gnome-disk-utility
inst gnome-system-monitor
inst gnome-system-log
inst gnome-logs
inst gnome-calculator

# polkits
#inst polkit-gnome
#inst polkit-kde-agent
inst xfce-polkit

inst xscreensaver
inst qt5-styleplugins
inst qt5ct

# xfce developer
inst xfce4-meta

# file manager
inst mc
inst ranger
inst ufraw-thumbnailer

# Thunar
inst thunar-archive-plugin 
inst thunar-media-tags-plugin

if [ $IS_MANJARO = true ]; then
	inst thunar-shares-plugin-manjaro 
fi

if [ $IS_ARCO = true ]; then
	inst thunar-shares-plugin
fi

inst folder-color-switcher
inst gtkhash-thunar
inst thunar-vcs-plugin 
inst thunar-volman

# Audio Radio
inst radiotray-ng
inst shortwave

# Nemo
inst cinnamon-translations
inst gtkhash-nemo
inst nemo
inst nemo-audio-tab
inst nemo-compare
inst nemo-emblems
inst nemo-fileroller
inst nemo-image-converter
inst nemo-media-columns
inst nemo-mediainfo-tab
inst nemo-preview
inst nemo-qml-plugin-notifications
inst nemo-repairer
inst nemo-seahorse
inst nemo-share

# Rofi Configs
if [ $IS_ARCO = true ]; then
	inst arcolinux-rofi-git
	inst arcolinux-rofi-themes-git
fi
inst rofi
inst rofimoji
inst rofi-emoji

# sound setup
inst alsa-utils
inst alsa-oss
inst paprefs
inst pasystray
inst sp
inst sound-theme-smooth
inst gst123
inst livecd-sounds
inst mint-sounds
inst tvolnoti
inst ponymix

# Python libs
inst python

# other
inst bleachbit

# bluetooth setup
if [ $IS_MANJARO = true ]; then
	inst manjaro-bluetooth
elif [ $IS_ENDEA = true ]; then
	inst bluez 
	inst bluez-utils 
	inst pulseaudio-bluetooth
	inst blueman
elif [ $IS_ARCO = true ]; then
	#inst bluetooth-autoconnect
	inst pulseaudio-bluetooth
	inst blueman
	#inst bluez
	#inst bluez-hid2hci
	#inst bluez-libs
	#inst bluez-plugins
	#inst bluez-tools
	#inst bluez-utils

fi

# printer setup
if [ $IS_MANJARO = true ]; then
	inst manjaro-printer
fi
inst brother-mfc-j4420dw
inst canon-cque
inst cups-pdf
inst samsung-printers
inst hplip
inst system-config-printer
inst print-manager

inst ghostscript 
inst gsfonts 
inst foomatic-db-engine 
inst foomatic-db 
inst foomatic-db-ppds 
inst foomatic-db-nonfree 
inst foomatic-db-nonfree-ppds 
inst foomatic-db-gutenprint-ppds
inst gutenprint 

# scanner setup
#inst xsane
#inst simple-scan
#inst skanlite
#inst colord-sane
#inst sane

# Browser
inst firefox
inst firefox-i18n-de
inst chromium

# remmina
inst remmina
inst remmina-plugin-open
inst remmina-plugin-rdesktop
inst remmina-plugin-url
inst remmina-plugin-folder
inst remmina-plugin-open
inst freerdp
inst remote-desktop-manager-free

# installation of important editors
inst micro
inst neovim
inst visual-studio-code-bin
inst typora

# utils for editors
inst bash-completion
inst clang
inst fd
inst gdb
inst graphviz
inst prettier
inst ripgrep
inst shellcheck
inst tar

# wallpapers, themes, icons and fonts

# icons
inst paper-icon-theme
inst papirus-icon-theme

# cursor
if [ $IS_ARCO = true ]; then
	inst bibata-cursor-theme
else
	inst bibata-cursor-theme-bin
fi
inst bibata-cursor-translucent

# fonts
inst font-manager

# powerline in linux console
inst awesome-terminal-fonts
inst terminus-font
inst powerline-fonts

inst nerd-fonts-fira-code
inst nerd-fonts-noto
inst nerd-fonts-roboto-mono
inst nerd-fonts-terminus
inst ttf-meslo-nerd-font-powerlevel10k
inst ttf-devicons
inst ttf-twemoji
inst ttf-twemoji-color
inst ttf-weather-icons


# lightdm config
inst lightdm
inst lightdm-settings
inst lightdm-gtk-greeter-settings
inst lightdm-gtk-greeter
inst lightdm-slick-greeter
inst lightdm-webkit2-greeter
inst lightdm-webkit2-theme-glorious
inst lightdm-webkit2-clean
inst lightdm-webkit2-theme-obsidian

if [ $IS_MANJARO = true ]; then
  inst manjaro-slick-greeter-theme-light
fi

# grub
inst grub-customizer
inst update-grub

if [ $IS_MANJARO != true -a $IS_ARCO != true ]; then
	inst grub2-theme-archlinux
fi

# btrfs tools
inst grub-btrfs
inst snapper
inst snapper-gui
inst timeshift-autosnap

#########################
# collect optional apps #
#########################
#inst epdfview

# gimicks
inst cmatrix
inst hollywood

# Office
inst libreoffice-fresh
inst libreoffice-fresh-de

# language files
inst man-pages-de
inst aspell-de
inst mythes-de
inst libmythes
inst languagetool

# Browser
if [ $IS_ARCO != true ]; then
	inst google-chrome
else
	inst vivaldi
fi
inst microsoft-edge-dev-bin

# optional application packages
inst aspell
inst baobab
inst gimp
inst gimp-help-de
inst gwenview
inst imagemagick-full
inst libmagick-full
inst inkscape
inst partitionmanager
inst pinta
inst playerctl
inst teams
inst teams-insiders

inst glfw-x11
inst phonon-qt5-vlc

inst themix-full
inst themix-icons-numix

# rust apps
inst bat
inst fd
inst ripgrep
inst tokei
inst procs

###############################
# uninstall unneeded packages #
###############################
fullUninstall

#################################
# install all (needed) packages #
#################################
fullInstall

## FINISHING #
finish

# Fix Teams Black Screen
# sudo ln -f -s /bin/true /usr/share/teams/resources/app.asar.unpacked/node_modules/slimcore/bin/rect-overlay
# sudo ln -f -s /bin/true /usr/share/teams-insiders/resources/app.asar.unpacked/node_modules/slimcore/bin/rect-overlay

sound complete

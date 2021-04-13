#!/usr/bin/env zsh

. ~/.scripts/defs.zsh

# Init Install
initInstall "install_base"

###########################
# collect needed packages #
###########################

uninst tlp

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
uninst checkupdates-aur
inst clamav
inst clamtk
inst cockpit
inst cockpit-machines
inst cockpit-pcp
inst copyq
inst dex
inst docker
inst docker-compose
inst dpkg
inst exa
inst find-the-command
inst flatpak
inst fzf
inst genius
inst glances
inst gparted
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
inst lxqt-openssh-askpass
inst multitail
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
inst radiotray-ng
uninst redshift
uninst rofi
inst rsyslog
inst s-tui
inst shell-color-scripts
inst spectacle
inst time
inst timeshift
inst tldr++
inst ulauncher
inst unclutter
inst unrar
inst usermin
inst webmin
inst xautolock
inst xbindkeys
inst xclip
inst xdotool
inst xorg-xfd
inst xorg-xsetroot
inst xsel
inst zenity
inst pm-utils


if [ $IS_GARUDA = true ]; then
	inst samba-support
fi

if [ $IS_MANJARO = true ]; then
	inst samba
	inst manjaro-settings-samba
fi

# Base Development Tools
inst dotnet-sdk-bin
inst git
uninst gitahead-bin
inst gitflow-avh
uninst github-cli
inst git-delta-bin
inst gradle
inst jdk-openjdk
inst jdk8-openjdk
inst libsecret
inst mono
inst mono-msbuild
inst nodejs-emojione
inst nodejs
inst nuget
inst npm
inst svn
inst yarn
inst tk
inst nsis

# jetbrains apps
inst jetbrains-toolbox

# awesome packages
inst awesome
inst vicious
inst xorg-server-xephyr
inst luacheck
inst awmtt

# config tools

if [ $IS_ARCO = true ]; then
	inst lxappearance
else
	inst lxappearance-gtk3
fi
#inst lxqt-config
#inst lxqt-admin
#inst lxqt-policykit
#inst lxqt-qtplugin
#inst lxqt-sudo
#inst xsettingsd
#inst xsettings-client

# utilities from gnome
inst gnome-disk-utility
inst gnome-system-monitor
inst gnome-system-log
inst gnome-calculator

# polkits
#inst polkit-gnome
#inst polkit-kde-agent
inst xfce-polkit

inst xscreensaver
inst qt-logout
inst qt5-styleplugins
inst qt5ct

# file manager
inst thunar 
inst thunar-archive-plugin 
inst thunar-shares-plugin-manjaro 
inst thunar-vcs-plugin 
inst thunar-volman    
inst gtkhash-thunar
inst mc
inst ranger

# sound setup
inst paprefs
inst pasystray
inst sp

# Python libs
inst python
#inst python-psutil
uninst python-pygit2
#inst python-requests
uninst python-taskw
uninst python-xkbgroup
uninst python2-distutils-extra
uninst python-black
uninst python-nose
uninst python-pipenv
uninst python-pyflakes
uninst python-pytest
#inst python-lxml
uninst python-numpy

# other
inst bleachbit

# bluetooth setup
if [ $IS_GARUDA = true ]; then
	inst bluetooth-support
elif [ $IS_MANJARO = true ]; then
	inst manjaro-bluetooth
elif [ $IS_ARCO = true ]; then
	print 'install bluetooth packages'
	inst bluetooth-autoconnect
	#inst pulseaudio-bluetooth
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
inst system-config-printer
inst print-manager
inst xsane
inst simple-scan
inst skanlite

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

# installation of important editors
inst micro
inst neovim
inst visual-studio-code-bin

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

# themes
inst materia-gtk-theme
inst kvantum-theme-materia
inst adwaita-dark

# icons
inst paper-icon-theme
inst papirus-icon-theme

# wallpapers
inst ukui-wallpapers

# cursor
if [ $IS_ARCO = true ]; then
	inst bibata-cursor-theme
else
	inst bibata-cursor-theme-bin
fi
inst bibata-cursor-translucent

# fonts
inst font-manager
inst awesome-terminal-fonts

inst nerd-fonts-complete
# inst nerd-fonts-cascadia-code
# inst nerd-fonts-iosevka 
# inst ttf-nerd-fonts-symbols

inst ttf-devicons
inst ttf-twemoji
inst ttf-twemoji-color
inst ttf-weather-icons
inst ttf-ms-win10-auto

# lightdm config
if [ $IS_ARCO != true ]; then
	inst lightdm
	inst lightdm-settings
	inst lightdm-gtk-greeter-settings
	inst lightdm-slick-greeter
	inst lightdm-gtk-greeter
fi

if [ $IS_MANJARO = true ]; then
  inst manjaro-slick-greeter-theme-light
fi

# grub
if [ $IS_GARUDA = true ]; then
	inst grub-theme-garuda
fi
if [ $IS_MANJARO != true -a $IS_ARCO != true ]; then
	inst grub2-theme-archlinux
	inst grub-theme-stylish-git
fi

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

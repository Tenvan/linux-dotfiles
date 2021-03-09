#!/usr/bin/env zsh

. ~/.scripts/defs.zsh

# Init Install
initInstall "install_base"

###########################
# collect needed packages #
###########################

# system packages
inst alacritty
inst alttab
inst arandr
inst arj
inst ark
inst bashtop
inst bat
inst bitwarden-bin
inst bitwarden-cli-bin
inst bitwarden-rofi
inst bootsplash-systemd
inst bootsplash-theme-manjaro
inst bpytop
inst checkupdates-aur
inst clamav
inst clamtk
inst cockpit
inst cockpit-machines
inst copyq
inst dex
inst docker
inst docker-compose
inst dpkg
inst exa
inst find-the-command
inst flatpak
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
inst kindd
inst kitty
inst kteatime
inst lhasa
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
inst python
inst python-psutil
inst python-pygit2
inst python-requests
inst python-taskw
inst python-xkbgroup
inst python2-distutils-extra
inst qbittorrent
inst radiotray-ng
inst redshift
inst rofi
inst rsyslog
inst s-tui
inst shell-color-scripts
inst spectacle
inst time
inst timeshift
inst tldr++
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

uninst tlp

if [ $IS_GARUDA = true ]; then
	inst samba-support
fi

# Base Development Tools
inst dotnet-sdk-bin
inst git
inst gitahead-bin
inst gitflow-avh
inst github-cli
inst git-delta
inst gradle
inst jdk-openjdk
inst jdk8-openjdk
inst libsecret
inst mono
inst mono-msbuild
inst nodejs-emojione
inst nodejs-lts-fermium
inst nuget
inst npm
inst svn
inst yarn
inst tk
inst xsp

# jetbrains apps
inst jetbrains-toolbox

# awesome packages
inst awesome
inst vicious
inst xorg-server-xephyr
inst luacheck
inst awmtt

# config tools
# inst lxappearance
inst lxappearance-gtk3
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
inst pcmanfm
inst mc
inst ranger

# sound setup
inst paprefs
inst pasystray
inst sp

# other
inst dockbarx
inst bleachbit

if [ $IS_GARUDA != true ]; then
	print 'install sound packages'
	#inst pulseaudio-ctl
	#inst pulseaudio-qt
	#inst pulseaudio-equalizer-ladspa
fi

# bluetooth setup
if [ $IS_GARUDA = true ]; then
	inst bluetooth-support
else
	print 'install bluetooth packages'
	#inst blueberry
	#inst bluetooth-autoconnect
	#inst pulseaudio-bluetooth
	#inst bluez
	#inst bluez-hid2hci
	#inst bluez-libs
	#inst bluez-plugins
	#inst bluez-tools
	#inst bluez-utils
	# inst gnome-bluetooth
	#inst sbc
fi

# printer setup
inst brother-mfc-j4420dw
inst canon-cque
inst cups-pdf
inst samsung-printers
inst system-config-printer

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
inst python-black
inst python-nose
inst python-pipenv
inst python-pyflakes
inst python-pytest
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
inst bibata-cursor-theme-bin
inst bibata-cursor-translucent

# fonts
inst font-manager
inst awesome-terminal-fonts
inst nerd-fonts-cascadia-code
inst nerd-fonts-iosevka
inst nerd-fonts-mononoki
inst noto-fonts-emoji
inst ttf-twemoji
inst ttf-twemoji-color
inst ttf-weather-icons

# lightdm config
inst lightdm
inst lightdm-settings
inst lightdm-gtk-greeter-settings
inst lightdm-slick-greeter
inst lightdm-gtk-greeter
if [ $IS_MANJARO != true ]; then
  inst manjaro-slick-greeter-theme-light
fi

# grub
if [ $IS_GARUDA = true ]; then
	inst grub-theme-garuda
fi
if [ $IS_MANJARO != true ]; then
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

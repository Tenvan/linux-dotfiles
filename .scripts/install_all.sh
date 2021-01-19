#!/usr/bin/env bash

. $SCRIPTS/defs.sh

#####################
# init distro check #
#####################
DEBUG=FALSE
#DEBUG=TRUE

errorCheck() {
    retVal=$?
    if [ $retVal -ne 0 ]; then
        echo "abort installation script 'install_all': $1"
        exit $retVal
    fi
}

inst() {
    PAKAGE_INST="${PAKAGE_INST} $1"
    
    if [ $DEBUG = "TRUE" ]; then
		pakku -S $PAKKU_ALL $1
		
	    retVal=$?
	    if [ $retVal -ne 0 ]; then
	        echo "error on install: $1"
			ERROR_PAKAGE_INST="${ERROR_PAKAGE_INST}
$1"        
	    fi
    fi
}

uninst() {
    PAKAGE_UNINST="${PAKAGE_UNINST} $1"

    if [ $DEBUG = "TRUE" ]; then
	    pakku -R --noconfirm $1
	    
	    retVal=$?
	    if [ $retVal -ne 0 ]; then
	        echo "error on uninstall: $1"
			ERROR_PAKAGE_UNINST="${ERROR_PAKAGE_UNINST}
$1"        
	    fi
	fi
}

###########################
# collect needed packages #
###########################

# system packages
inst alacritty
inst arj
inst ark
inst bashtop
inst bpytop
inst clamav
inst clamtk
inst dpkg
inst exa
inst find-the-command
inst fish
inst glances
inst gtop
inst hstr
inst htop
inst iftop
inst iotop
inst iptraf-ng
inst kindd
inst kitty
inst lhasa
inst multitail
inst neofetch
inst openconnect
inst p7zip
uninst qfc-git
inst s-tui
inst shell-color-scripts
uninst starship
inst timeshift
inst unrar

# Base Development Tools
inst dotnet-sdk
inst git
inst gitahead-bin
inst gitflow-avh
inst github-cli
inst jdk-openjdk
inst jdk8-openjdk
inst libsecret
inst mono
inst mono-msbuild
inst nodejs-emojione
inst nodejs-lts-fermium
inst npm
inst openjdk8-src
inst svn
inst yarn
uninst meld

# jetbrains apps
inst jetbrains-toolbox

# awesome packages
inst awesome
inst vicious
inst xorg-server-xephyr
inst luacheck
inst awmtt

# config tools
#inst lxqt-config
#inst lxqt-admin
#inst lxqt-policykit
#inst lxqt-qtplugin
#inst lxqt-sudo
#inst lxappearance
#inst xsettingsd
#inst xsettings-client

# system packages
inst arandr
inst bitwarden-bin
inst bitwarden-cli-bin
inst bitwarden-rofi
inst checkupdates-aur
inst copyq
inst dex
inst docker
inst docker-compose
inst etcher-bin
inst genius
inst gparted
inst grub-customizer
inst hardinfo
inst i3-sensible-browser-git
inst ibus-daemon
inst kteatime
inst notify-send.sh
inst numlockx
inst picom-jonaburg-git
inst pm-utils
inst pygtk
inst python
inst python-psutil
inst python-pygit2
inst python-requests
inst python-taskw
inst python-xkbgroup
inst python2-distutils-extra
inst rofi
inst systemdgenie
inst time
inst unclutter
inst webmin
inst xautolock
inst xbindkeys
inst xclip
inst xcwd-git
inst xdotool
inst xorg-xfd
inst xorg-xsetroot
inst xsel
inst zenity
inst ktorrent
inst nitrogen
inst qbittorrent
inst spectacle

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
inst pulseaudio-ctl
inst pulseaudio-qt
inst pulseaudio-equalizer-ladspa
inst pasystray
inst sp

# bluetooth setup
inst blueberry
inst bluetooth-autoconnect
#inst bluetooth-support
inst bluez
inst bluez-hid2hci
inst bluez-libs
inst bluez-plugins
inst bluez-tools
inst bluez-utils
inst gnome-bluetooth
inst pulseaudio-bluetooth
inst sbc

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

# inst zoom
inst zoom-system-qt
inst zoom-firefox

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
inst sublime-text-3

# vs code
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
inst arc-gtk-theme
inst arc-solid-gtk-theme
inst materia-gtk-theme
inst kvantum-theme-materia


# icons
inst arc-icon-theme
inst paper-icon-theme
inst papirus-icon-theme
inst papirus-icon-theme
inst papirus-icon-theme
inst sardi-icons

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
inst ttf-ms-fonts
inst ttf-twemoji
inst ttf-twemoji-color
inst ttf-weather-icons

# lightdm config
inst lightdm
inst lightdm-settings
inst lightdm-slick-greeter
inst lightdm-webkit2-greeter

# grub
uninst grub2-theme-archlinux
inst grub-theme-stylish-git
#inst arch-matrix-grub-theme-git
inst arch-silence-grub-theme-git

sudo rm /var/lib/pacman/db.lck

###############################
# uninstall unneeded packages #
###############################
if [ $DEBUG != "TRUE" ]; then
	echo "UNINST: $PAKKU_PAKAGE_U"
	pakku -R --noconfirm $PAKAGE_UNINST
	#errorCheck "uninstall packages"
fi

#################################
# install all (needed) packages #
#################################
if [ $DEBUG != "TRUE" ]; then
	echo "INST: $PAKKU_PAKAGE"
	pakku -S $PAKKU_ALL $PAKAGE_INST
	errorCheck "install packages"
fi

## FINISHING #

#echo "Error in Uninst: ${ERROR_PAKAGE_UNINST}"
echo "Error in Inst: ${ERROR_PAKAGE_INST}"

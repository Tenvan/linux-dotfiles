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

###################
# system packages #
###################
inst alacritty
inst arj
inst ark
inst bashtop
inst bpytop
inst clamav
inst clamtk
inst cronie
inst dpkg
inst exa
inst find-the-command
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
uninst powerline-rs
inst qfc-git
uninst ripgrep
inst s-tui
inst shell-color-scripts
inst starship
inst timeshift
inst unrar

# gimicks
inst cmatrix
inst hollywood

##########################
# Base Development Tools #
##########################
inst meld
inst git
inst gitflow-avh
inst github-cli
inst gitahead-bin
inst libsecret
inst nodejs-lts-fermium
inst nodejs-emojione
inst npm
inst yarn
inst dotnet-sdk
inst mono
inst mono-msbuild
inst jdk8-openjdk
inst openjdk8-src
inst jdk-openjdk

# jetbrains apps
inst jetbrains-toolbox

########################################
# Installing all used Window Managers  #
# Unter ArcoLinux TweakTool verwenden. #
########################################

# awesome packages
inst awesome
inst vicious
inst xorg-server-xephyr
inst luacheck
inst luarocks
inst lua-luajson
inst lua-socket
inst awmtt

#####################################
# installation desktop packages     #
# for tiling windows managers (all) #
#####################################
# system packages
inst appeditor-git
inst arandr
inst blueman
inst checkupdates-aur
inst copyq
inst genius
inst lxappearance
inst notify-send.sh
inst pm-utils
inst pygtk
inst python
inst python-psutil
inst python-pygit2
inst python-requests
inst python-taskw
inst python-xkbgroup
inst python2-distutils-extra
inst radiotray
inst rofi
inst seahorse
inst systemdgenie
inst time
inst xautolock
inst xbindkeys
inst xclip
inst xcwd-git
inst xdotool
inst xorg-xfd
inst xsel
inst zenity
inst skippy-xd-git

# utilities from gnome
inst gnome-disk-utility
inst gnome-system-monitor
inst gnome-system-log
inst gnome-keyring
inst gnome-calculator
inst gnome-menu-editor-qt

# polkits
inst polkit-gnome
inst polkit-kde-agent

inst xscreensaver
inst qt-logout
inst qt5-styleplugins
inst qt5ct
#inst phonon-qt5-gstreamer

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
inst spotify
inst sp

# printer setup
inst brother-mfc-j4420dw
inst canon-cque
inst cups-pdf
inst samsung-printers
inst system-config-printer

# Office
inst libreoffice-fresh
inst libreoffice-fresh-de

# Browser
inst firefox-developer-edition
inst firefox-developer-edition-i18n-de
inst firefox
inst firefox-i18n-de
inst google-chrome
inst chromium
inst microsoft-edge-dev-bin
uninst vivaldi

# optional application packages
inst aspell
inst bitwarden-bin
inst bitwarden-cli-bin
inst bitwarden-rofi
inst docker
inst docker-compose
inst evolution
inst gimp
inst gimp-help-de
inst gparted
inst grub-customizer
inst hardinfo
uninst imagemagick
inst inkscape
inst ktorrent
inst nitrogen
inst partitionmanager
inst playerctl
inst pinta
inst pstoedit
inst python-lxml
inst python-numpy
inst qbittorrent
inst spectacle
inst teams
uninst transfig
uninst transmission-gtk
uninst xfig
inst etcher-bin

# remmina
inst remmina
inst remmina-plugin-open
inst remmina-plugin-rdesktop
inst remmina-plugin-url
inst remmina-plugin-folder
inst remmina-plugin-open
inst freerdp

#inst picom-ibhagwan-git
inst picom-jonaburg-git

#####################################
# installation of important editors #
#####################################
# vs code
inst visual-studio-code-bin
inst bash-completion
uninst lua-format

# neovim
inst neovim

# Atom
uninst atom-editor-bin
uninst atom-editor-git

# utils for editors
inst shellcheck
inst prettier
inst ripgrep
inst clang
inst tar
inst fd
inst gdb
inst graphviz
inst python-black
inst python-pyflakes
inst python-pipenv
inst python-nose
inst python-pytest

inst micro

###############################################
# install wallpapers, themes, icons and fonts #
###############################################

# themes
uninst arc-gtk-theme-colorpack
inst arc-gtk-theme
inst arc-solid-gtk-theme
uninst deepin-gtk-theme
inst materia-gtk-theme
inst kvantum-theme-materia
uninst numix-gtk-theme
uninst zuki-themes

#inst oomox
inst themix-full-git

# icons
inst arc-icon-theme
inst papirus-icon-theme
uninst numix-icon-theme-pack-git

# inst bibata-extra-cursor-git
uninst bibata-extra-cursor-theme
inst bibata-cursor-theme-bin
inst bibata-cursor-translucent

# wallpapers
inst ukui-wallpapers

# cursor
uninst xcursor-breeze
uninst xcursor-breeze-adapta
uninst xcursor-breeze-serie-obsidian
uninst xcursor-chameleon-anthracite
uninst xcursor-chameleon-darkskyblue
uninst xcursor-chameleon-pearl
uninst xcursor-chameleon-skyblue
uninst xcursor-chameleon-white

# icons
uninst adwaita-icon-theme
# inst arc-icon-theme
inst paper-icon-theme
inst papirus-icon-theme
inst papirus-icon-theme
# inst sardi-icons

# fonts
inst font-manager
inst fontmatrix
inst awesome-terminal-fonts

uninst nerd-fonts-complete
inst nerd-fonts-mononoki
inst nerd-fonts-iosevka

inst noto-fonts-emoji

inst ttf-twemoji
inst ttf-twemoji-color
inst ttf-weather-icons
inst ttf-ms-fonts

# lightdm config
inst lightdm
inst lightdm-settings
inst lightdm-slick-greeter
inst lightdm-webkit2-greeter

# webkit themes
inst lightdm-webkit2-clean
uninst lightdm-webkit2-theme-alter
inst lightdm-webkit2-theme-glorious
inst lightdm-webkit2-theme-material2
uninst lightdm-webkit2-theme-obsidian
uninst lightdm-webkit2-theme-sapphire
inst lightdm-webkit-theme-osmos
inst lightdm-webkit-theme-aether
uninst lightdm-webkit-theme-contemporary
uninst lightdm-webkit-theme-luminos
uninst lightdm-webkit-theme-wisp
uninst lightdm-webkit-theme-osmos
uninst lightdm-webkit-theme-petrichor-git
uninst lightdm-webkit-theme-sequoia-git

# grub
inst grub2-theme-archlinux
inst grub-theme-stylish-git
inst arch-matrix-grub-theme-git
inst arch-silence-grub-theme-git

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

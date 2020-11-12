#!/usr/bin/env bash

source $HOME/Scripts/defs.sh

#####################
# init distro check #
#####################
PKG_FILE=pkg_to_install.txt
PKG_UNINST_FILE=pkg_to_uninstall.txt

YAY_ALL="--needed --batchinstall --topdown \
    --nocleanmenu --nodiffmenu --noeditmenu --noupgrademenu \
    --norebuild --noredownload --noprovides --useask --noremovemake \
    --combinedupgrade"

errorCheck() {
    retVal=$?
    if [ $retVal -ne 0 ]; then
        echo "abort installation script 'install_all': $1"
        exit $retVal
    fi
}

inst() {
    echo $1 >> $PKG_FILE
}

uninst() {
    echo $1 >> $PKG_UNINST_FILE
}

rm -f $PKG_FILE
rm -f $PKG_UNINST_FILE

# Yay installieren
sudo pacman -S --noconfirm --needed yay

# Manjaro
if $IS_MANJARO == true; then
    yay -S $YAY_ALL pamac-tray-appindicator pamac-flatpak-plugin pamac-snap-plugin
fi

# ArcoLinux
if $IS_ARCO == true; then
    yay -S --noconfirm $YAY_ALL pamac-all
fi
errorCheck "installation yay/pamac"

# Rust repaprieren/installieren
yay -S --needed --noconfirm rustup
errorCheck "installation rustup"

rustup install stable
errorCheck "rustup stable"

###########################
# collect needed packages #
###########################

###################
# system packages #
###################
inst exa
inst ripgrep
inst timeshift
uninst timeset-gui
uninst termite
inst alacritty
inst ark
inst arj
inst dpkg
inst lhasa
inst unrar
inst p7zip
inst neofetch
uninst screenfetch
inst clamav
inst clamtk
inst glances
inst bashtop
inst bpytop
inst gtop
inst htop
inst iftop
inst iotop
inst iptraf-ng
inst s-tui
inst shell-color-scripts
inst powerline-rs
inst find-the-command
inst hstr
inst qfc-git
inst kindd
inst multitail
inst openconnect

# Manjaro
if $IS_MANJARO == true; then
    # manjaro only packages
    inst lightdm-gtk-greeter-settings
    inst manjaro-settings-samba
    inst manjaro-pulse

    # gimicks
    inst cmatrix
    inst hollywood
    inst cowsay
fi

# ArcoLinux
if $IS_ARCO == true; then
    # arco only packages
    inst arcolinux-lightdm-gtk-greeter-settings
    inst arcolinux-termite-themes-git
fi

##########################
# Base Development Tools #
##########################
inst meld
inst git
inst gitflow-avh
inst github-cli
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

########################################
# Installing all used Window Managers  #
# Unter ArcoLinux TweakTool verwenden. #
########################################

if $IS_MANJARO == true; then
    inst dmenu-manjaro
    # current used settings
    inst manjaro-awesome-settings
    # inst manjaro-cinnamon-settings
fi

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
inst copyq
inst time
inst rofi
inst zenity
inst xclip
inst xsel
inst xdotool
inst xorg-xfd
inst xcwd-git
inst notify-send.sh
uninst polybar
uninst broot
inst pm-utils
inst seahorse
inst checkupdates-aur
inst lxappearance
inst alttab-git
inst xautolock
inst systemdgenie
inst python
inst python-psutil
inst python-pygit2
inst python-xkbgroup
inst python-taskw
inst python-requests
inst pygtk
inst python2-distutils-extra
inst xbindkeys
inst genius
inst radiotray

# utilities from gnome
inst gnome-disk-utility
inst gnome-system-monitor
inst gnome-system-log
inst gnome-keyring
inst gnome-calculator
inst gnome-menu-editor-qt

# polkits
inst polkit-gnome
uninst polkit-kde-agent

# Multi Monitor Lock and QT-Logout
if $IS_ARCO == true; then
  inst arcolinux-betterlockscreen-git arcolinux-logout-git arcolinux-logout-themes-git
  uninst arcolinux-openbox-git arcolinux-i3wm-git
fi

inst xscreensaver
inst qt-logout

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
uninst sound-theme-elementary
uninst sound-theme-smooth
uninst sound-theme-lbr-impress

# printer setup
inst canon-cque
inst samsung-printers
inst cups-pdf
inst system-config-printer

# Office
inst libreoffice-fresh
inst libreoffice-fresh-de

# optional application packages
inst evolution
inst bitwarden-bin
inst bitwarden-cli-bin
inst bitwarden-rofi
inst firefox
inst firefox-i18n-de
uninst firefox-developer-edition
uninst firefox-developer-edition-i18n-de
inst google-chrome
inst chromium
inst opera
inst microsoft-edge-dev-bin
inst docker
inst docker-compose
uninst foxitreader
inst gparted
inst partitionmanager
inst grub-customizer
inst hardinfo
inst spectacle
uninst krita
uninst blender
inst gimp
inst gimp-help-de
inst pinta
inst aspell
inst imagemagick
inst pstoedit
inst inkscape
inst xfig
inst transfig
inst nitrogen
inst python-lxml
inst python-numpy
inst teams
inst qbittorrent

# remmina
inst remmina
inst remmina-plugin-open
inst remmina-plugin-rdesktop
inst remmina-plugin-url
inst remmina-plugin-folder
inst remmina-plugin-open
inst freerdp

# virtualbox
inst virtualbox
inst virtualbox-ext-oracle

if $IS_ARCO == true; then
    inst virtualbox-host-modules-arch
    inst linux-headers
fi

if $IS_MANJARO == true; then
    inst linux$(uname -r | sed -E 's/(.){1}\.(.){1}(.*)/\1\2/g')-headers
    inst linux$(uname -r | sed -E 's/(.){1}\.(.){1}(.*)/\1\2/g')-virtualbox-host-modules
fi

# libvirt service and manager
inst virt-manager
inst qemu
inst qemu-arch-extra
inst libvirt

inst picom-ibhagwan-git

#####################################
# installation of important editors #
#####################################
# vs code
inst visual-studio-code-bin
inst bash-completion
inst lua-format

# neovim
inst neovim

# Atom
inst atom-editor-bin

# utils for editors
inst shellcheck
inst prettier
inst ripgrep
inst clang
inst tar
inst fd
inst gdb
inst bashdb
inst graphviz
inst python-black
inst python-pyflakes
inst python-pipenv
inst python-nose
inst python-pytest

###############################################
# install wallpapers, themes, icons and fonts #
###############################################
if $IS_ARCO == true; then
    # wallpapers
    inst arcolinux-wallpapers-git
    inst arcolinux-wallpapers-lxqt-dual-git

    # themes
    inst arcolinux-arc-themes-nico-git
fi

if $IS_MANJARO == true; then
    inst artwork-i3
    inst manjaro-artwork
    inst manjaro-artwork-extra
    inst manjaro-users-artwork-wallpapers
    inst manjaro-backgrounds
    inst awesome-wallpapers
    inst cinnamon-wallpapers
    inst illyria-wallpaper

    # themes
    inst arc-themes-breath
    inst arc-themes-maia
    inst arc-themes-solid-breath
    inst arc-themes-solid-maia

    # icons
    inst breeze-maia-icon-themes
    inst manjaro-artwork-icons
    inst papirus-maia-icon-theme
    inst vertex-maia-icon-theme
    inst arc-maia-icon-theme
    inst breath-icon-theme
    inst breath2-icon-themes
    # cursor

    # inst bibata-extra-cursor-git
    inst bibata-extra-cursor-theme
    inst bibata-cursor-theme-bin
    inst bibata-cursor-translucent
fi

# themes
inst arc-gtk-theme
inst materia-gtk-theme

# wallpapers
inst ukui-wallpapers

# cursor
inst xcursor-breeze
inst xcursor-breeze-adapta
inst xcursor-breeze-serie-obsidian
inst xcursor-chameleon-anthracite
inst xcursor-chameleon-darkskyblue
inst xcursor-chameleon-pearl
inst xcursor-chameleon-skyblue
inst xcursor-chameleon-white
uninst xcursor-comix
uninst xcursor-flatbed
uninst xcursor-neutral
uninst xcursor-premium
uninst xcursor-simpleandsoft

# icons
inst adwaita-icon-theme
inst arc-icon-theme
inst arc-icon-theme
uninst faba-icon-theme
uninst faba-icon-theme
uninst hicolor-icon-theme
uninst hicolor-icon-theme
uninst maia-icon-theme
uninst moka-icon-theme
inst paper-icon-theme
inst papirus-icon-theme
inst papirus-icon-theme
inst sardi-icons

# fonts
inst font-manager
inst awesome-terminal-fonts

uninst nerd-fonts-complete
inst nerd-fonts-mononoki
inst nerd-fonts-iosevka

inst noto-fonts-emoji
uninst noto-fonts-extra

uninst ttf-droid
uninst ttf-nerd-fonts-symbols
uninst ttf-font-awesome
uninst ttf-cascadia-code
inst ttf-twemoji
inst ttf-twemoji-color
inst ttf-weather-icons

#################################
# install all (needed) packages #
#################################
yay -S $YAY_ALL - < $PKG_FILE
errorCheck "install packages"

###############################
# uninstall unneeded packages #
###############################
yay -R --noconfirm - < $PKG_UNINST_FILE

## FINISHING #
# Atom Plugins
apm install autocomplete-lua busy-signal intentions language-lua \
  linter linter-lua linter-ui-default
  
# Git config for meld
git config --global diff.tool code
git config --global difftool.code.cmd "$(which code) --wait --diff \"\$LOCAL\" \"\$BASE\" \"\$REMOTE\""
git config --global difftool.prompt false

git config --global merge.tool code
git config --global mergetool.code.cmd "$(which code) --wait \"\$MERGED\""
git config --global mergetool.prompt false

git config --global core.editor $(which code)

# nodejs tools for editors
sudo npm install -g neovim eslint jshint jsxhint stylelint sass-lint markdownlint-cli raml-cop typescript tern js-beautify
errorCheck "install required nodejs-tools"

sudo systemctl enable libvirtd.service
sudo systemctl start libvirtd.service
errorCheck "libvirtd service"

# Default Browser setzen (vorher $BROWSER Variable entfernen)
xdg-settings set default-web-browser firefox-developer-edition.desktop

sudo fc-cache -fv
errorCheck "fontcache"

rm -f $PKG_FILE
rm -f $PKG_UNINST_FILE

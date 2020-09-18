#!/usr/bin/env bash

#####################
# init distro check #
#####################
LINUX_VERSION_NAME=$(lsb_release -si)
PKG_FILE=pkg_to_install.txt

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
        echo "abort installation script 'install_all': $1"
        exit $retVal
    fi
}

inst() {
    echo $1 >> $PKG_FILE
}

rm -f $PKG_FILE

# Yay installieren
sudo pacman -S --noconfirm --needed yay
errorCheck "installation yay"

# Rust repaprieren/installieren
yay -Syy --needed --noconfirm rustup
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
inst timeset-gui
inst termite 
inst alacritty
inst ark
inst arj
inst dpkg
inst lhasa
inst unrar
inst p7zip
inst neofetch
inst screenfetch
inst clamav
inst clamtk
inst glances
inst bashtop
inst gtop
inst htop
inst iftop
inst iotop
inst iptraf-ng
inst s-tui
inst shell-color-scripts 
inst powerline-rs
inst find-the-command
inst hstr-git 
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
inst nodejs
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
    errorCheck "Manjaro: menus"

    # current used settings
    inst manjaro-awesome-settings
    # inst manjaro-cinnamon-settings
    errorCheck "Manjaro: settings"
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
inst polybar
inst broot
inst pm-utils
inst qt-logout
inst multimonitorlock
inst multimonitorlock-gui
inst gnome-system-monitor
inst gnome-system-log
inst polkit-gnome
inst polkit-kde-agent
inst seahorse
inst gnome-keyring
inst pamac-tray-appindicator
inst checkupdates-aur
inst lxappearance
inst alttab-git
inst xautolock
inst systemdgenie 
inst etcher
inst python
inst python-psutil
inst python-pygit2
inst python-xkbgroup
inst python-taskw
inst python-requests
inst pygtk
inst python2-distutils-extra

# file manager
inst nemo
inst nemo-share
inst folder-color-switcher
inst nemo-fileroller
inst nemo-image-converter
inst nemo-preview
inst nemo-seahorse
inst nemo-compare
inst nemo-emblems
inst nemo-media-columns
inst nemo-pdf-tools
inst nemo-meld-compare
inst nemo-audio-tab
inst nemo-mediainfo-tab
inst nemo-ext-git
inst mc

# sound setup
inst paprefs
inst pulseaudio-ctl
inst pulseaudio-qt
inst pulseaudio-equalizer-ladspa
inst pasystray
inst sound-theme-elementary
inst sound-theme-smooth
inst sound-theme-lbr-impress

# printer setup
inst canon-cque
inst samsung-printers
inst cups-pdf
inst system-config-printer

# LibreOffice fresh
inst libreoffice-fresh

# optional application packages
inst evolution
inst bitwarden-bin
inst bitwarden-cli-bin
inst bitwarden-rofi
inst firefox
inst firefox-i18n-de
inst firefox-developer-edition
inst firefox-developer-edition-i18n-de
inst google-chrome
inst chromium
inst opera
inst docker
inst docker-compose
inst foxitreader
inst gparted
inst partitionmanager
inst grub-customizer
inst hardinfo \
inst spectacle
inst krita
inst blender
inst gimp
inst gimp-help-de
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

# remmina
inst remmina
inst remmina-plugin-open
inst remmina-plugin-rdesktop
inst remmina-plugin-url
inst remmina-plugin-folder
inst remmina-plugin-open
inst freerdp

# virtualbox for lts-kernel
inst virtualbox
inst virtualbox-host-dkms
# inst linux-lts-headers
# inst linux-lts-virtualbox-host-modules
inst linux$(uname -r | sed -E 's/(.){1}\.(.){1}(.*)/\1\2/g')-headers
inst linux$(uname -r | sed -E 's/(.){1}\.(.){1}(.*)/\1\2/g')-virtualbox-host-modules

# libvirt service and manager
inst virt-manager
inst qemu
inst qemu-arch-extra
inst libvirt

yay -R --noconfirm picom
inst picom-ibhagwan-git

if $IS_MANJARO == true; then
    # optional application packages manjaro
    inst pinta-git
fi

#####################################
# installation of important editors #
#####################################
# geany
inst geany
inst geany-plugins
inst geany-themes

# vs code
inst code
inst bash-completion
inst lua-format

# neovim
inst neovim
inst python-pynvim
inst neovim-remote
inst vim
inst vim-plug

# utils for editors
inst nodejs
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
inst xcursor-comix
inst xcursor-flatbed
inst xcursor-neutral
inst xcursor-premium
inst xcursor-simpleandsoft

# icons
inst adwaita-icon-theme
inst arc-icon-theme
inst arc-icon-theme
inst faba-icon-theme
inst faba-icon-theme
inst hicolor-icon-theme
inst hicolor-icon-theme
inst maia-icon-theme
inst moka-icon-theme
inst paper-icon-theme
inst papirus-icon-theme
inst papirus-icon-theme
inst sardi-icons

# fonts
inst font-manager
inst awesome-terminal-fonts
inst nerd-fonts-complete
inst ttf-nerd-fonts-symbols
inst ttf-font-awesome
inst ttf-cascadia-code
inst ttf-twemoji
inst ttf-twemoji-color
inst ttf-weather-icons

#################################
# install all (needed) packages #
#################################
yay -S --needed --batchinstall - < $PKG_FILE
errorCheck "install packages"

# FINISHING #

# Git config for meld
git config --global diff.tool meld
git config --global difftool.meld.path "/usr/bin/meld \"\$LOCAL\" \"\$REMOTE\""
git config --global difftool.prompt false

git config --global merge.tool meld
git config --global mergetool.meld.path "/usr/bin/meld \"\$LOCAL\" \"\$BASE\" \"\$REMOTE\" --output \"\$MERGED\""
git config --global mergetool.prompt false

# spacevim
curl -sLf https://spacevim.org/install.sh | bash
errorCheck "spacevim install"

# nodejs tools for editors
yarn global add neovim eslint jshint jsxhint stylelint sass-lint markdownlint-cli raml-cop typescript tern js-beautify
errorCheck "install required nodejs-tools"

wallpath=$HOME/.local/share/wallpapers/wallpapers-dt

if [ -f $wallpath/README.md ]; then
    echo DT Wallpaper gefunden. aktualisiere...
    cd $wallpath
    git pull
    errorCheck "pull dt wallpapers"
    cd ~
else
    echo DT Wallpaper NICHT gefunden. klone...
    git clone https://gitlab.com/dwt1/wallpapers.git $wallpath
    errorCheck "clone dt wallpapers"
fi

sudo systemctl enable libvirtd.service
sudo systemctl start libvirtd.service
errorCheck "libvirtd service"

chmod +x ~/Scripts/100-user-monitors.sh
sudo cp ~/Scripts/100-user-monitors.sh /etc/X11/xinit/xinitrc.d

if [ -f $HOME/.screenlayout/screenlayout.sh ]; then
    sed 's/^#display-setup-script=$/display-setup-script=\/opt\/screenlayout.sh/g' < /etc/lightdm/lightdm.conf > lightdm.conf
    sudo mv lightdm.conf /etc/lightdm
    sudo cp $HOME/.screenlayout/screenlayout.sh /opt
fi

# Default Browser setzen (vorher $BROWSER Variable entfernen)
xdg-settings set default-web-browser firefox.desktop

# ArcoLinux
if $IS_ARCO == true; then
    sudo grub-mkconfig -o /boot/grub/grub.cfg
    errorCheck "ArcoLinux: grub config"
fi

sudo fc-cache -fv
errorCheck "fontcache"

rm -f $PKG_FILE

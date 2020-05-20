#!/usr/bin/env bash
# base i3 with bumblebee-status
yay -S picom-ibhagwan-git

yay -S nitrogen dmenu bmenu rofi zenity spectacle lightdm-gtk-greeter-settings pm-utils \
polkit-gnome polkit-kde-agent \
pamac-gtk pamac-cli pamac-tray-appindicator checkupdates-aur \
copyq xautolock oblogout-blurlock conky pavucontrol pa-applet \
python-psutil python-pygit2 python-xkbgroup python-taskw python-requests octopi octopi-notifier-qt5 \
xfce4-taskmanager gnome-system-monitor gnome-system-log xclip xsel xdotool xorg-xfd xcwd-git progress \
bitwarden-bin bitwarden-cli-bin bitwarden-rofi foxitreader pfetch

# from kde (replace if possible)
yay -S kate krusader kde-cli-tools ksystemlog ksysguard kruler

# applications
yay -S ungit gitahead-bin timeset-gui python \
neovim vim-plugins vundle vim-vimwiki vifm vim-nerdtree vim-instant-markdown

pip install --user neovim


# arts
yay -S manjaro-artwork manjaro-artwork-extra \
manjaro-wallpapers-17.0 manjaro-wallpapers-18.0 wallpapers-juhraya \
cinnamon-wallpapers manjaro-wallpapers-by-lunix-cinnamon yaru-colors-wallpapers-git \
manjaro-users-artwork-wallpapers muser-wallpapers illyria-wallpaper \
awesome-wallpapers awesome-themes-git \
artwork-i3 i3-default-artwork manjaro-wallpapers-by-lunix-i3

# cursor
yay -S xcursor-chameleon-pearl xcursor-chameleon-darkskyblue xcursor-chameleon-skyblue xcursor-chameleon-anthracite xcursor-chameleon-white

# icons
yay -S manjaro-artwork-icons papirus-icon-theme arc-icon-theme ttf-weather-icons \
breath-icon-theme breath2-icon-themes \
maia-icon-theme papirus-maia-icon-theme vertex-maia-icon-theme \
ultraflatorange-icon-theme masalla-icon-theme nitrux-icon-theme nouvegnomegray-icon-theme

# printer setup
yay -S canon-cque samsung-printers cups-pdf system-config-printer manjaro-settings-samba

chmod +x ~/Scripts/100-user-monitors.sh
sudo cp ~/Scripts/100-user-monitors.sh /etc/X11/xinit/xinitrc.d

# Default Browser setzen (vorher $BROWSER Variable entfernen)
xdg-settings set default-web-browser firefox.desktop

#!/usr/bin/env bash

yay -Syy

#yay -S picom
yay -S picom-ibhagwan-git
#yay -S picom-tryone-git

yay -S --noconfirm termite termite-style-git nitrogen dmenu bmenu rofi zenity ripgrep \
exa glances bashtop gtop htop iftop iotop iptraf-ng s-tui \
polybar broot broom spectacle lightdm-gtk-greeter-settings pm-utils \
polkit-gnome polkit-kde-agent \
pamac-gtk pamac-cli pamac-tray-appindicator checkupdates-aur \
copyq oblogout-manjaro xautolock conky pavucontrol pa-applet \
python-psutil python-pygit2 python-xkbgroup python-taskw python-requests pygtk python2-distutils-extra \
octopi octopi-notifier-qt5 gnome-system-monitor gnome-system-log xclip xsel xdotool xorg-xfd xcwd-git progress \
bitwarden-bin bitwarden-cli-bin bitwarden-rofi foxitreader pfetch wedder-git alttab-git

# main editors
yay -S --noconfirm kate geany geany-plugins geany-themes editorconfig-geany neovim vim-plugins

sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

# from XFCE4 (Tools, Panels, etc.)
yay -S --noconfirm xfce4-taskmanager

# from kde (replace if possible)
yay -S --noconfirm krusader kde-cli-tools ksystemlog ksysguard kruler

# applications
yay -S --noconfirm ungit gitahead-bin timeset-gui python

pip install --user neovim

# printer setup
yay -S --noconfirm canon-cque samsung-printers cups-pdf system-config-printer manjaro-settings-samba

# sound
yay -S --noconfirm manjaro-pulse paprefs pulseaudio-ctl pulseaudio-qt pulseaudio-equalizer-ladspa pasystray sound-theme-elementary yaru-sound-theme sound-theme-smooth sound-theme-kayo sound-theme-sakura sound-theme-lbr-impress

chmod +x ~/Scripts/100-user-monitors.sh
sudo cp ~/Scripts/100-user-monitors.sh /etc/X11/xinit/xinitrc.d

# Default Browser setzen (vorher $BROWSER Variable entfernen)
xdg-settings set default-web-browser firefox.desktop

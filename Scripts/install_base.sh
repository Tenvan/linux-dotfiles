#!/usr/bin/env bash

# base i3 with bumblebee-status
yay -S picom-ibhagwan-git

yay -S termite termite-style-git nitrogen dmenu bmenu rofi zenity ripgrep \
exa glances bashtop gtop htop iftop iotop iptraf-ng s-tui \
nushell broot broom spectacle lightdm-gtk-greeter-settings pm-utils \
polkit-gnome polkit-kde-agent \
pamac-gtk pamac-cli pamac-tray-appindicator checkupdates-aur \
copyq oblogout-manjaro xautolock conky pavucontrol pa-applet \
python-psutil python-pygit2 python-xkbgroup python-taskw python-requests pygtk python2-distutils-extra \
octopi octopi-notifier-qt5 gnome-system-monitor gnome-system-log xclip xsel xdotool xorg-xfd xcwd-git progress \
bitwarden-bin bitwarden-cli-bin bitwarden-rofi foxitreader pfetch wedder-git

# main editors
yay -S kate geany geany-plugins geany-themes editorconfig-geany neovim vundle

# from XFCE4 (Tools, Panels, etc.)
yay -S xfce4 manjaro-xfce-settings xfce4-panel

yay -S xfce4-taskmanager xfce4-panel xfce4-panel-profiles xfce4-whiskermenu-plugin \
xfce4-cpugraph-plugin xfce4-datetime-plugin xfce4-diskperf-plugin xfce4-mount-plugin \ 
xfce4-sensors-plugin xfce4-taskmanager xfce4-systemload-plugin xfce4-weather-plugin \
xfce4-timer-plugin xfce4-indicator-plugin-git xfce4-calculator-plugin

# from kde (replace if possible)
yay -S krusader kde-cli-tools ksystemlog ksysguard kruler

# applications
yay -S ungit gitahead-bin timeset-gui python \
neovim vim-plugins vundle vim-vimwiki vifm vim-nerdtree vim-instant-markdown

pip install --user neovim

# printer setup
yay -S canon-cque samsung-printers cups-pdf system-config-printer manjaro-settings-samba

# sound
yay -S manjaro-pulse paprefs pulseaudio-ctl pulseaudio-qt pulseaudio-equalizer-ladspa pasystray sound-theme-elementary yaru-sound-theme sound-theme-smooth sound-theme-kayo sound-theme-sakura sound-theme-lbr-impress

chmod +x ~/Scripts/100-user-monitors.sh
sudo cp ~/Scripts/100-user-monitors.sh /etc/X11/xinit/xinitrc.d

# Default Browser setzen (vorher $BROWSER Variable entfernen)
xdg-settings set default-web-browser firefox.desktop

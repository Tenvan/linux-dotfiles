#!/usr/bin/env bash
# base i3 with bumblebee-status
yay -S picom-ibhagwan-git

yay -S termite termite-style-git nitrogen dmenu bmenu rofi zenity ripgrep \
nushell broot broom spectacle lightdm-gtk-greeter-settings pm-utils \
polkit-gnome polkit-kde-agent \
pamac-gtk pamac-cli pamac-tray-appindicator checkupdates-aur \
copyq oblogout-manjaro xautolock conky pavucontrol pa-applet \
python-psutil python-pygit2 python-xkbgroup python-taskw python-requests pygtk python2-distutils-extra \
octopi octopi-notifier-qt5 gnome-system-monitor gnome-system-log xclip xsel xdotool xorg-xfd xcwd-git progress \
bitwarden-bin bitwarden-cli-bin bitwarden-rofi foxitreader pfetch surf

# main editors (emacs, vim, neovim, etc)
yay -S geany emcas neovim

# main shell
yay -S zsh zsh-completions zsh-completion-generator-git

# from XFCE4 (Tools, Panels, etc.)
yay -S xfce4 manjaro-xfce-settings xfce4-panel

yay -S xfce4-taskmanager xfce4-panel xfce4-panel-profiles xfce4-whiskermenu-plugin \
xfce4-cpugraph-plugin xfce4-datetime-plugin xfce4-diskperf-plugin xfce4-mount-plugin xfce4-sensors-plugin \
xfce4-taskmanager xfce4-systemload-plugin xfce4-weather-plugin xfce4-timer-plugin xfce4-indicator-plugin-git xfce4-calculator-plugin

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

#fonts
yay -S gucharmap kcharselect charmap.app nerd-fonts-mononoki ttf-mononoki ttf-joypixels

# printer setup
yay -S canon-cque samsung-printers cups-pdf system-config-printer manjaro-settings-samba

chmod +x ~/Scripts/100-user-monitors.sh
sudo cp ~/Scripts/100-user-monitors.sh /etc/X11/xinit/xinitrc.d

# Default Browser setzen (vorher $BROWSER Variable entfernen)
xdg-settings set default-web-browser firefox.desktop

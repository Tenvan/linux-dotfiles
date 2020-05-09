# base i3 with bumblebee-status
yay -S --noconfirm bmenu perl-anyevent-i3 rofi zenity compton dunst i3exit spectacle polkit-gnome  pamac-gtk pamac-cli pamac-tray-appindicator clipit xautolock conky conky-i3 bumblebee-status pavucontrol pa-applet dmenu-manjaro checkupdates-aur python-i3ipc python-psutil xcwd-git python-pygit2 python-xkbgroup xkb-switch-i3 progress python-taskw python-requests octopi octopi-notifier-qt5 xfce4-taskmanager gnome-system-monitor gnome-system-log xclip xsel xdotool bitwarden-bin bitwarden-cli-bin bitwarden-rofi lightdm-gtk-greeter-settings

# from kde (replace if possible)
yay -S --noconfirm polkit-kde-agent kate krusader kde-cli-tools ksystemlog ksysguard

# applications
yay -S --noconfirm ungit gitahead-bin timeset-gui

# arts
yay -S --noconfirm manjaro-artwork manjaro-artwork-extra artwork-i3 i3-default-artwork illyria-wallpaper manjaro-wallpapers-17.0 manjaro-wallpapers-18.0 wallpapers-juhraya cinnamon-wallpapers manjaro-wallpapers-by-lunix-cinnamon manjaro-wallpapers-by-lunix-i3 yaru-colors-wallpapers-git manjaro-users-artwork-wallpapers muser-wallpapers

# cursor
yay -S --noconfirm xcursor-chameleon-pearl xcursor-chameleon-darkskyblue xcursor-chameleon-skyblue xcursor-chameleon-anthracite xcursor-chameleon-white

# icons
yay -S --noconfirm manjaro-artwork-icons ttf-weather-icons 

# printer setup
yay -S --noconfirm canon-cque samsung-printers cups-pdf foxitreader system-config-printer manjaro-settings-samba
 
chmod +x ~/100-user-monitors.sh
sudo cp ~/100-user-monitors.sh /etc/X11/xinit/xinitrc.d

# Default Browser setzen (vorher $BROWSER Variable entfernen)
xdg-settings set default-web-browser firefox.desktop

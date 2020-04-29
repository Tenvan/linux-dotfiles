yay -S --noconfirm morc_menu bmenu rofi zenity compton dunst i3-scrot spectacle polkit-gnome polkit-kde-agent xfce4-power-manager pamac-tray-appindicator clipit xautolock conky conky-i3 bumblebee-status pa-applet volumeicon xfce4-gtk3 xfce4-goodies xfce4-terminal
yay -S --noconfirm checkupdates-aur python-i3ipc python-psutil xcwd python-pygit2 python-xkbgroup xkb-switch-i3 progress python-taskw python-requests 
yay -S lightdm lightdm-gtk-greeter lightdm-gtk-greeter-settings
yay -S manjaro-xfce-gtk3-settings manjaro-settings-manager

sudo systemctl enable lightdm.service --force

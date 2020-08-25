#!/bin/bash

# Function create a scale dialog
filesEdit="code -r --file-uri"
folderEdit="code -r --folder-uri"

get_config_list() {
    zenity --list \
           --checklist \
           --width=300 \
           --height=600 \
           --title="Edit Konfiguation" \
           --text="Konfig file" \
           --column=Check \
           --column=Option \
           --column="Datei" \
           --separator=" " \
           --print-column=3 \
           --hide-column=3 \
           --hide-header \
           false "📑 awesome dir" "$folderEdit $HOME/.config/awesome" \
           false "📑 scripts autostart" "$fileEdit $HOME/Scripts/autostart*.sh" \
           false "📑 scripts install" "$fileEdit $HOME/Scripts/install_*.sh" \
           false "📑 scripts ldap" "$fileEdit $HOME/Scripts/ldap/*.sh" \
           false "📑 scripts all" "$folderEdit $HOME/Scripts" \
           false "📑 bash" "$fileEdit $HOME/.bashrc*" \
           false "📑 menus" "$folderEdit $HOME/Scripts/menu" \
           false "📑 ranger" "$fileEdit $HOME/.config/ranger/*.sh $HOME/.config/ranger/*.conf" \
           false "📑 broot" "$folderEdit $HOME/.config/broot" \
           false "📑 picom" "$folderEdit  $HOME/.config/picom" \
           false "📑 polybar" "$fileEdit $HOME/.config/polybar/*.ini $HOME/.config/polybar/*.sh" \
           false "📑 termite" "$fileEdit $HOME/.config/termite/config" \
           false "📑 alacritty" "$fileEdit $HOME/.config/alacritty/alacritty.yml"  \
           false "📑 themes" "$fileEdit $HOME/.Xresources* $HOME/.gtkrc-2.0 $HOME/.config/gtk-3.0/* $HOME/.config/gtk-4.0 $HOME/.config/qt5ct/* $HOME/.config/fontconfig/*" \
           false "📑 nvim" "$fileEdit $HOME/.SpaceVim.d/*.* $HOME/.SpaceVim.d/autoload/*.*" \
           false "📑 gitignore" "$fileEdit $HOME/.gitignore"
}

choice=$(get_config_list)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    $choice &
fi

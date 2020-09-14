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
           true "🇬 all (Git)" "$folderEdit $HOME" \
           false "📑 awesome dir" "$folderEdit $HOME/.config/awesome" \
           false "📑 scripts autostart" "$filesEdit $HOME/Scripts/autostart*.sh" \
           false "📑 scripts install" "$filesEdit $HOME/Scripts/install_*.sh" \
           false "📑 scripts ldap" "$filesEdit $HOME/Scripts/ldap/*.sh" \
           false "📑 scripts dir" "$folderEdit $HOME/Scripts" \
           false "📑 bash" "$filesEdit $HOME/.bashrc*" \
           false "📑 menu dir" "$folderEdit $HOME/Scripts/menu" \
           false "📑 ranger" "$filesEdit $HOME/.config/ranger/*.sh $HOME/.config/ranger/*.conf" \
           false "📑 broot dir" "$folderEdit $HOME/.config/broot" \
           false "📑 picom dir" "$folderEdit  $HOME/.config/picom" \
           false "📑 polybar" "$filesEdit $HOME/.config/polybar/*.ini $HOME/.config/polybar/*.sh" \
           false "📑 termite" "$filesEdit $HOME/.config/termite/config" \
           false "📑 alacritty" "$filesEdit $HOME/.config/alacritty/alacritty.yml"  \
           false "📑 themes" "$filesEdit $HOME/.Xresources* $HOME/.gtkrc-2.0 $HOME/.config/gtk-3.0/* $HOME/.config/gtk-4.0 $HOME/.config/qt5ct/* $HOME/.config/fontconfig/*" \
           false "📑 nvim" "$filesEdit $HOME/.SpaceVim.d/*.* $HOME/.SpaceVim.d/autoload/*.*" \
           false "📑 gitignoree" "$filesEdit $HOME/.gitignore"
}

choice=$(get_config_list)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    $choice &
fi

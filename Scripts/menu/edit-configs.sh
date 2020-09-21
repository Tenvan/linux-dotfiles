#!/bin/bash

# Function create a scale dialog
filesEdit="code -a --file-uri"
folderEdit="code -a --folder-uri"

get_config_list() {
    zenity --list \
           --width=300 \
           --height=600 \
           --title="Edit Konfiguation" \
           --text="Konfig file" \
           --column="Option" \
           --column="Datei" \
           --separator=" " \
           --print-column=2 \
           --hide-column=2 \
           --hide-header \
           "🇬 all (Git)" "$folderEdit $HOME" \
           "📑 awesome dir" "$folderEdit $HOME/.config/awesome" \
           "📑 x-files" "$filesEdit $HOME/.x*" \
           "📑 scripts autostart" "$filesEdit $HOME/Scripts/autostart*.sh" \
           "📑 scripts install" "$filesEdit $HOME/Scripts/install_*.sh" \
           "📑 scripts ldap" "$filesEdit $HOME/Scripts/ldap/*.sh" \
           "📑 scripts dir" "$folderEdit $HOME/Scripts" \
           "📑 bash" "$filesEdit $HOME/.bashrc*" \
           "📑 menu dir" "$folderEdit $HOME/Scripts/menu" \
           "📑 ranger" "$filesEdit $HOME/.config/ranger/*.sh $HOME/.config/ranger/*.conf" \
           "📑 broot dir" "$folderEdit $HOME/.config/broot" \
           "📑 picom dir" "$folderEdit  $HOME/.config/picom" \
           "📑 polybar" "$filesEdit $HOME/.config/polybar/*.ini $HOME/.config/polybar/*.sh" \
           "📑 termite" "$filesEdit $HOME/.config/termite/config" \
           "📑 alacritty" "$filesEdit $HOME/.config/alacritty/alacritty.yml"  \
           "📑 themes" "$filesEdit $HOME/.Xresources* $HOME/.gtkrc-2.0 $HOME/.config/gtk-3.0/* $HOME/.config/gtk-4.0 $HOME/.config/qt5ct/* $HOME/.config/fontconfig/*" \
           "📑 nvim" "$filesEdit $HOME/.SpaceVim.d/*.* $HOME/.SpaceVim.d/autoload/*.*" \
           "📑 gitignoree" "$filesEdit $HOME/.gitignore"
}

choice=$(get_config_list)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    echo $choice
    $choice &
fi

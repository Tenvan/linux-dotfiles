#!/bin/bash

# Function create a scale dialog
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
           false "📑 doom" "$HOME/.doom.d/*.*" \
           false "📑 spacemacs" "$HOME/.spacemacs" \
           false "📑 vim" "$HOME/.SpaceVim.d/*.* $HOME/.SpaceVim.d/autoload/*.*" \
           false "📑 awesome" ".config/awesome/*.lua" \
           false "📑 scripts autostart" "$HOME/Scripts/autostart*.sh" \
           false "📑 scripts install" "$HOME/Scripts/install_*.sh" \
           false "📑 scripts ldap" "$HOME/Scripts/ldap/*.sh" \
           false "📑 scripts all" "$HOME/Scripts/*.*" \
           false "📑 bash" "$HOME/.bashrc*" \
           false "📑 menus" "$HOME/Scripts/menu/*.*" \
           false "📑 ranger" "$HOME/.config/ranger/*.sh $HOME/.config/ranger/*.conf" \
           false "📑 broot" "$HOME/.config/broot/*.*" \
           false "📑 picom" "$HOME/.config/picom/*.*" \
           false "📑 polybar" "$HOME/.config/polybar/*.ini $HOME/.config/polybar/*.sh" \
           false "📑 termite" "$HOME/.config/termite/config" \
           false "📑 alacritty" "$HOME/.config/alacritty/alacritty.yml"  \
           false "📑 themes" "$HOME/.Xresources* $HOME/.gtkrc-2.0 $HOME/.config/gtk-3.0/* $HOME/.config/gtk-4.0 $HOME/.config/qt5ct/* $HOME/.config/fontconfig/*" \
           false "📑 gitignore" "$HOME/.gitignore"
}

choice=$(get_config_list)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    sh ~/Scripts/start-editor.sh "$choice" &
fi

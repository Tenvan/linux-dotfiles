#!/usr/bin/env bash

# Function create a scale dialog
filesEdit="code -a --file-uri"
folderEdit="code -a --folder-uri"
# filesEdit="atom "
# folderEdit="atom "

get_config_list() {
    zenity --list \
           --width=400 \
           --height=850 \
           --title="Edit Konfiguation" \
           --text="Konfig file" \
           --column="Option" \
           --column="Datei" \
           --separator=" " \
           --print-column=2 \
           --hide-column=2 \
           --hide-header \
           "🇬 Editor" "code" \
           "🇬 Workspace" "code -r $HOME/dotfiles.code-workspace" \
           "📁 config dir" "$folderEdit $HOME/.config" \
           "📁 awesome dir" "$folderEdit $HOME/.config/awesome" \
           "📑 shell configs" "$filesEdit $HOME/.bashrc* $HOME/.alias* $HOME/.profile* $HOME/.x* $HOME/.X* $HOME/.zsh*" \
           "📑 scripts autostart" "$filesEdit $SCRIPTS/autostart*.sh" \
           "📑 scripts install" "$filesEdit $SCRIPTS/install*" \
           "📑 scripts ldap" "$filesEdit $SCRIPTS/ldap/*.sh" \
           "📁 scripts dir" "$folderEdit $SCRIPTS" \
           "📁 menu dir" "$folderEdit $SCRIPTS/menu" \
           "📑 ranger" "$filesEdit $HOME/.config/ranger/*.sh $HOME/.config/ranger/*.conf" \
           "📁 picom dir" "$folderEdit  $HOME/.config/picom" \
           "📁 kitty" "$folderEdit $HOME/.config/kitty"  \
           "📁 neofetch" "$folderEdit $HOME/.config/neofetch"  \
           "📁 alacritty" "$folderEdit $HOME/.config/alacritty"  \
           "📑 themes" "$filesEdit $HOME/.Xresources* $HOME/.gtkrc-2.0 $HOME/.config/gtk-3.0/* $HOME/.config/gtk-4.0 $HOME/.config/qt5ct/* $HOME/.config/fontconfig/*" \
           "📑 screenlayout" "$filesEdit $HOME/.screenlayout/*" \
           "📑 gitignoree" "$filesEdit $HOME/.gitignore"
}

choice=$(get_config_list)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    echo $choice
    $choice &
fi

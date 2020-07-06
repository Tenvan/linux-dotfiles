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
    true awesome "$HOME/.config/awesome/*.lua" \
    false "scripts autostart" "$HOME/Scripts/autostart*.sh" \
    false "scripts install" "$HOME/Scripts/install_*.sh" \
    false "scripts all" "$HOME/Scripts/*.*" \
    false bash "$HOME/.bashrc*" \
    false menus "$HOME/Scripts/menu/*.*" \
    false ranger "$HOME/.config/ranger/*.sh $HOME/.config/ranger/*.conf" \
    false broot "$HOME/.config/broot/*.*" \
    false vim "$HOME/.config/nvim/init.vim" \
    false picom "$HOME/.config/picom/*.*" \
    false polybar "$HOME/.config/polybar/*.ini $HOME/.config/polybar/*.sh" \
    false termite "$HOME/.config/termite/config" \
    false xresources "$HOME/.Xresources*" \
    false gitignore "$HOME/.gitignore"
}

choice=$(get_config_list)

if [ -z "$choice" ]; then
  echo "abort choice"
else
  echo execute: $choice >>/dev/stderr
  # emacs $choice &
  # code --file-uri $choice &
  geany $choice &
  # eval $TERM -e "nvim -p -- $choice" &
fi

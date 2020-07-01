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
    false picom "$HOME/.config/picom/*.*" \
    false qtile "$HOME/.config/qtile/*.py" \
    false xmonad "$HOME/.xmonad/*.hs" \
    false bash "$HOME/.bashrc*" \
    false menus "$HOME/Scripts/menu/*.*" \
    false alacritty "$HOME/.config/alacritty/*" \
    false broot "$HOME/.config/broot/conf.toml" \
    false gitignore "$HOME/.gitignore" \
    false neovim "$HOME/.config/nvim/init.vim" \
    false polybar "$HOME/.config/polybar/*.ini $HOME/.config/polybar/*.sh" \
    false termite "$HOME/.config/termite/config" \
    false xresources "$HOME/.Xresources*" \
    false xinitrc "$HOME/.xinitrc" \
    false tint2 "$HOME/.config/tint2/*" \
    false spacemacs "$HOME/.spacemacs" \
    false doom "$HOME/.doom.d/*.*"
}

choice=$(get_config_list)

if [ -z "$choice" ]; then
  echo "abort choice"
else
  echo execute: $choice >>/dev/stderr
  # emacs $choice &
  # code --file-uri $choice &
  geany $choice &
fi

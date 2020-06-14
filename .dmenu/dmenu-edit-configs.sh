#!/bin/bash

#  ____ _____
# |  _ \_   _|  Derek Taylor (DistroTube)
# | | | || |    http://www.youtube.com/c/DistroTube
# | |_| || |    http://www.gitlab.com/dwt1/
# |____/ |_|
#
# Dmenu script for editing some of my more frequently edited config files.

# Function create a scale dialog
get_config_list() {
  zenity --list \
    --checklist \
    --width=600 \
    --height=600 \
    --title="Edit Konfiguation" \
    --text="Konfig file" \
    --column=Check \
    --column=Option \
    --column="Datei" \
    --separator=" " \
    --print-column=3 \
    true xmonad "$HOME/.xmonad/xmonad.hs" \
    true bash "$HOME/.bashrc" \
    true menus "$HOME/.dmenu/*.*" \
    false alacritty "$HOME/.config/alacritty/alacritty.yml" \
    false broot "$HOME/.config/broot/conf.toml" \
    true gitignore "$HOME/.gitignore" \
    false menu-sysmon "$HOME/.dmenu/dmenu-sysmon.sh" \
    false neovim "$HOME/.config/nvim/init.vim" \
    false picom "$HOME/.config/picom/*.*" \
    false polybar "$HOME/.config/polybar/config.ini" \
    false Scripts "$HOME/Scripts/*.*" \
    false termite "$HOME/.config/termite/config" \
    false xresources "$HOME/.Xresources" \
    false xinitrc "$HOME/.xinitrc"
}

choice=$(get_config_list)

if [ -z "$choice" ]; then
  echo "abort choice"
else
  code --folder-uri ~ --file-uri $choice &
fi

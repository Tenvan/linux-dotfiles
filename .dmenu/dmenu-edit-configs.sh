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
    false alacritty "$HOME/.config/alacritty/alacritty.yml" \
    true broot "$HOME/.config/broot/conf.toml" \
    true gitignore "$HOME/.gitignore" \
    true menu-edit "$HOME/.dmenu/dmenu-edit-configs.sh" \
    false menu-sysmon "$HOME/.dmenu/dmenu-sysmon.sh" \
    false neovim "$HOME/.config/nvim/init.vim" \
    false picom "$HOME/.config/picom/picom.conf" \
    true polybar "$HOME/.config/polybar/config.ini" \
    true install-arts "$HOME/Scripts/install_arts.sh" \
    true install-desktop "$HOME/Scripts/install_desktop.sh" \
    true install-editors "$HOME/Scripts/install_editors.sh" \
    true install-system "$HOME/Scripts/install_system.sh" \
    true install-wm "$HOME/Scripts/install_wm.sh" \
    false termite "$HOME/.config/termite/config" \
    true xresources "$HOME/.Xresources"
}

choice=$(get_config_list)

if [ -z "$choice" ]; then
  echo "abort choice"
else
  code dotfiles.code-workspace --file-uri $choice &
fi

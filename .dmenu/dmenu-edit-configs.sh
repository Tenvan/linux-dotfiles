#!/bin/bash
#  ____ _____
# |  _ \_   _|  Derek Taylor (DistroTube)
# | | | || |    http://www.youtube.com/c/DistroTube
# | |_| || |    http://www.gitlab.com/dwt1/
# |____/ |_|
#
# Dmenu script for editing some of my more frequently edited config files.


declare options=(
"xmonad
bash
alacritty
termite
broot
gitignore
menu-edit
menu-sysmon
neovim
picom
polybar
install-arts
install-desktop
install-editors
install-system
install-wm
xresources
quit"
)

choice=$(echo -e "${options[@]}" | rofi -dmenu -i -p 'Edit config file: ')

case "$choice" in
	quit)
		echo "Program terminated." && exit 1
	;;
	alacritty)
		choice="$HOME/.config/alacritty/alacritty.yml"
	;;
	bash)
		choice="$HOME/.bashrc"
	;;
	broot)
		choice="$HOME/.config/broot/conf.toml"
	;;
	gitignore)
		choice="$HOME/.gitignore"
	;;
	menu-edit)
		choice="$HOME/.dmenu/dmenu-edit-configs.sh"
	;;
	menu-sysmon)
		choice="$HOME/.dmenu/dmenu-sysmon.sh"
	;;
	neovim)
		choice="$HOME/.config/nvim/init.vim"
	;;
	picom)
		choice="$HOME/.config/picom/picom.conf"
	;;
	polybar)
		choice="$HOME/.config/polybar/config.ini"
	;;
	install-arts)
		choice="$HOME/Scripts/install_arts.sh"
	;;
	install-desktop)
		choice="$HOME/Scripts/install_desktop.sh"
	;;
	install-editors)
		choice="$HOME/Scripts/install_editors.sh"
	;;
	install-system)
		choice="$HOME/Scripts/install_system.sh"
	;;
	install-wm)
		choice="$HOME/Scripts/install_wm.sh"
	;;
	termite)
		choice="$HOME/.config/termite/config"
	;;
	xmonad)
		choice="$HOME/.xmonad/xmonad.hs $HOME/Scripts/install_xmonad.sh"
	;;
	xresources)
		choice="$HOME/.Xresources"
	;;
	*)
		exit 1
	;;
esac
kate $choice

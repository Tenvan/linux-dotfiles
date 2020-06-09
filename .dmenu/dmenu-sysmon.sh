#!/bin/bash
#  ____ _____
# |  _ \_   _|  Derek Taylor (DistroTube)
# | | | || |    http://www.youtube.com/c/DistroTube
# | |_| || |    http://www.gitlab.com/dwt1/
# |____/ |_|
#
# Dmenu script for launching system monitoring programs.

declare -a options=(
	"s-tui
bashtop
glances
gtop
htop
iftop
iotop
iptraf-ng")

choice=$(echo -e "${options[@]}" | zenity --text='System monitors: ' --list --width=300 --height=400 --column="Auswahl")

terminal=alacritty

case $choice in
quit)
	echo "Program terminated." && exit 1
	;;
bashtop | \
	glances | \
	gtop | \
	htop | \
	s-tui)
	exec $terminal -t $choice -e $choice
	;;
iftop | \
	iotop | \
	iptraf-ng)
	exec $terminal -t $choice -e sudo $choice
	;;
*)
	exit 1
	;;
esac

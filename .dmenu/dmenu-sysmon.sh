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
iptraf-ng
quit")

choice=$(echo -e "${options[@]}" | rofi -dmenu -i -p 'System monitors: ')

terminal=alacritty

case $choice in
	quit)
		echo "Program terminated." && exit 1
	;;
	bashtop| \
	glances| \
	gtop| \
	htop| \
	s-tui)
        exec $terminal -t $choice -e $choice 
	;;
	iftop| \
	iotop| \
	iptraf-ng)
        exec $terminal -t $choice -e sudo $choice
	;;
	*)
		exit 1
	;;
esac


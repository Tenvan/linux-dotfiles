#!/bin/bash

myFileManager="thunar"
myTerminal="alacritty"
myBrowser="$BROWSER"
workDir=$WORK_DIR
shellCmd="$myTerminal  "
timeCmd="/usr/bin/time -v "

filesEdit="code -r --file-uri"
folderEdit="code -r --folder-uri"

# Function create a scale dialog
select_application() {
    zenity --list \
           --width=400 \
           --height=500 \
           --title="System Monitors" \
           --text="MONITOR" \
           --column="Option" \
           --column="Aktion" \
           --separator=" " \
           --print-column=2 \
           --hide-column=2 \
           --hide-header \
           "💿 System Resourcen" "gnome-system-monitor -r" \
           "💿 s-tui" "$shellCmd -t SysMon:s-tui -e s-tui" \
           "💿 bashtop" "$shellCmd -t SysMon:bashtop -e bashtop" \
           "💿 glances" "$shellCmd -t SysMon:glances -e glances" \
           "💿 gtop" "$shellCmd -t SysMon:gtop -e gtop" \
           "💿 htop" "$shellCmd -t SysMon:htop -e htop" \
           "💽 iftop (sudo)" "$shellCmd -t SysMon:iftop -e sudo iftop" \
           "💽 iotop (sudo)" "$shellCmd -t SysMon:iotop -e sudo iotop" \
           "💽 iptraf-ng (sudo)" "$shellCmd -t SysMon:iptraf-ng -e sudo iptraf-ng"
}

choice=$(select_application)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    echo exec: $choice >>/dev/stderr
    eval $choice &
fi

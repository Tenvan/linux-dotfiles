#!/usr/bin/env bash

myFileManager="thunar"
myTerminal="kitty"
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
           --height=450 \
           --title="System Monitors" \
           --text="MONITOR" \
           --column="Option" \
           --column="Aktion" \
           --separator=" " \
           --print-column=2 \
           --hide-column=2 \
           --hide-header \
           "💿 System Resourcen" "gnome-system-monitor -r" \
           "💿 s-tui" "$shellCmd --title SysMon:s-tui s-tui" \
           "💿 bpytop" "$shellCmd --title SysMon:bashtop bpytop" \
           "💿 bashtop" "$shellCmd --title SysMon:bashtop bashtop" \
           "💿 glances" "$shellCmd --title SysMon:glances glances" \
           "💿 gtop" "$shellCmd --title SysMon:gtop gtop" \
           "💿 htop" "$shellCmd --title SysMon:htop htop" \
           "💽 iftop (sudo)" "$shellCmd --title SysMon:iftop sudo iftop" \
           "💽 iotop (sudo)" "$shellCmd --title SysMon:iotop sudo iotop" \
           "💽 iptraf-ng (sudo)" "$shellCmd --title SysMon:iptraf-ng sudo iptraf-ng"
}

choice=$(select_application)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    echo exec: $choice >>/dev/stderr
    eval $choice &
fi

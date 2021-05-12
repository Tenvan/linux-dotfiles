#!/usr/bin/env bash

myFileManager="thunar"
myTerminal="kitty"
workDir=$WORK_DIR
shellCmd="$myTerminal  "
timeCmd="/usr/bin/time -v "

filesEdit="code -r --file-uri"
folderEdit="code -r --folder-uri"

ACTIONS=(
    "💿 System Resourcen" "gnome-system-monitor -r"
    "💿 s-tui" "$shellCmd --hold --title SysMon:s-tui s-tui"
    "💿 bpytop" "$shellCmd --title SysMon:bashtop bpytop"
    "💿 bashtop" "$shellCmd --title SysMon:bashtop bashtop"
    "💿 glances" "$shellCmd --title SysMon:glances glances"
    "💿 gtop" "$shellCmd --title SysMon:gtop gtop"
    "💿 htop" "$shellCmd --title SysMon:htop htop"
    "💽 iftop (sudo)" "$shellCmd --title SysMon:iftop sudo iftop"
    "💽 iotop (sudo)" "$shellCmd --title SysMon:iotop sudo iotop"
    "💽 iptraf-ng (sudo)" "$shellCmd --title SysMon:iptraf-ng sudo iptraf-ng"
)

LINECOUNT=$(expr ${#ACTIONS[*]} / 2)
LINEHEIGHT=$(($LINECOUNT * $LINEHEIGHT))
HEIGHT=$(($LINEHEIGHT + $LINEOFFSET))

# Function create a scale dialog
select_application() {
    zenity --list \
        --width=400 \
        --height=$HEIGHT \
        --title="Monitor Apps" \
        --text="MONITOR" \
        --column="Option" \
        --column="Aktion" \
        --separator=" " \
        --hide-column=2 \
        --hide-header \
        "${ACTIONS[@]}"
}

choice=$(select_application)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    echo exec: $choice >>/dev/stderr
    eval $choice &
fi

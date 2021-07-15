#!/usr/bin/env bash

shellCmd="kitty "

ACTIONS=(
    "💿 System Resourcen" "gnome-system-monitor -r" \
    "💿 s-tui" "$shellCmd --hold --title SysMon:s-tui s-tui" \
    "💿 bpytop" "$shellCmd --title SysMon:bashtop bpytop" \
    "💿 bashtop" "$shellCmd --title SysMon:bashtop bashtop" \
    "💿 glances" "$shellCmd --title SysMon:glances glances" \
    "💿 gtop" "$shellCmd --title SysMon:gtop gtop" \
    "💿 htop" "$shellCmd --title SysMon:htop htop" \
    "💽 iftop (sudo)" "$shellCmd --title SysMon:iftop sudo iftop" \
    "💽 iotop (sudo)" "$shellCmd --title SysMon:iotop sudo iotop -Pao" \
    "💽 iptraf-ng (sudo)" "$shellCmd --title SysMon:iptraf-ng sudo iptraf-ng" \
)

LINECOUNT=$(expr ${#ACTIONS[*]} / 2)
MLINEHEIGHT=$(($LINECOUNT * $LINEHEIGHT))
HEIGHT=$(($MLINEHEIGHT + $LINEOFFSET))

# Function create a scale dialog
select_application() {
    yad --center --on-top --sticky \
        --list \
        --no-headers \
        --width=400 \
        --height=$HEIGHT \
        --title="Monitor Apps" \
        --text="MONITOR" \
        --column="Option" \
        --column="Aktion" \
        --separator=" " \
        --print-column=2 \
        --hide-column=2 \
        "${ACTIONS[@]}"
}

choice=$(select_application)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    echo exec: $choice >>/dev/stderr
    $choice &
fi

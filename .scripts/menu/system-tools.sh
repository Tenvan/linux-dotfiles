#!/usr/bin/env bash
. $SCRIPTS/defs

shellCmd="$TERMINAL "
timeCmd="$TIME "
myTestLua=$(eval echo $HOME/.config/awesome/rc.test.lua)

SYSTEM_ACTIONS=(
        "🪄 Install Updates" "$shellCmd --hold --title Sys:Upall $timeCmd yay -Syyu --needed" \
        "🪄 ReInstall All Packages" "$shellCmd --hold --title Sys:Upall pacman -Qqn | pacman -S -" \
        "🪄 Grub Update (BTRFS Snapshots)" "$shellCmd --hold --title Sys:Grubup $timeCmd sudo update-grub" \
        "🕰 Timeshift" "timeshift-launcher" \
        "🕰 Timeshift create Snapshot" "$shellCmd --hold --title Sys:Install $timeCmd sudo timeshift --create" \
        "🗂 Belegung Verzeichnisse" "baobab" \
        "🖥 Monitor einrichten" "arandr" \
        "🎫 Erscheinungsbild (Lx)" "lxappearance" \
        "🎫 Erscheinungsbild (Xfce4)" "xfce4-appearance-settings" \
        "🎫 Erscheinungsbild (Qt5)" "qt5ct" \
        "📛 Boot Logs" "qjournalctl" \
        "📛 Log Viewer (Gui)" "glogg" \
        "📛 System Logs (Gui)" "sudo ksystemlog" \
        "📛 System Logs (Console)" "$shellCmd --hold --title Sys:Install $timeCmd journalctl" \
        "📛 xsession Errors" "$shellCmd --title AWMTT multitail -cs -i $HOME/.xsession-errors" \
)

csource "$CUSTOMS/${0##*/}"

ACTIONS=("${CUSTOM_TOP_ACTIONS[@]}" "${SYSTEM_ACTIONS[@]}" "${CUSTOM_BOTTOM_ACTIONS[@]}")
LINECOUNT=$(expr ${#ACTIONS[*]} / 2)
MLINEHEIGHT=$(($LINECOUNT * $LINEHEIGHT))
HEIGHT=$(($MLINEHEIGHT + $LINEOFFSET))


# Function create a scale dialog
select_application() {
    yad --center --on-top --sticky \
        --list \
        --no-headers \
        --width=500 \
        --height=$HEIGHT \
        --title="Edit Konfiguation" \
        --text="APPLICATIONS" \
        --column=Option \
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
    echo excecute: $choice >>/dev/stderr
    $choice &
fi

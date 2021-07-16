#!/usr/bin/env bash

myFileManager="thunar"
shellCmd="kitty"
timeCmd="/usr/bin/time -v "
myTestLua=$(eval echo $HOME/.config/awesome/rc.test.lua)

ACTIONS=(
        "🪄 Install Updates" "$shellCmd --hold --title Sys:Upall $timeCmd yay -Syyu --needed" \
        "🪄 ReInstall All Packages" "$shellCmd --hold --title Sys:Upall $timeCmd pacman -Qqn | pacman -S -" \
        "🪄 Grub Update (BTRFS Snapshots)" "$shellCmd --hold --title Sys:Grubup $timeCmd sudo grub-mkconfig -o /boot/grub/grub.cfg" \
        "🔫 Snapper-Gui (BTRFS Snapshots)" "sudo snapper-gui" \
        "🕰 Timeshift" "timeshift-launcher" \
        "🕰 Timeshift create Snapshot" "$shellCmd --hold --title Sys:Install $timeCmd sudo timeshift --create" \
        "🗂 Belegung Verzeichnisse" "baobab" \
        "🖥 Monitor einrichten" "arandr" \
        "🎫 Erscheinungsbild" "xfce4-appearance-settings" \
        "🎫 Erscheinungsbild (Xfce4)" "xfce4-appearance-settings" \
        "🎫 Erscheinungsbild (Qt5)" "xfce4-appearance-settings" \
        "🧩 Install Base Packages" "$shellCmd --hold --title Sys:Install $timeCmd sh $SCRIPTS/install_base.zsh" \
        "🧩 Install Applications" "$shellCmd --hold --title Sys:Install $timeCmd sh $SCRIPTS/install_apps.zsh" \
        "🧩 Install Virtual Engines" "$shellCmd --hold --title Sys:Install $timeCmd sh $SCRIPTS/install_vm.zsh" \
        "🛻 SQL-Server Stop" "$shellCmd --hold --title OTC:SqlServer $timeCmd sudo systemctl stop mssql-server"
        "🛻 SQL-Server Start" "$shellCmd --hold --title OTC:SqlServer $timeCmd sudo systemctl start mssql-server"
        "🛻 SQL-Server Restart" "$shellCmd --hold --title OTC:SqlServer $timeCmd sudo systemctl restart mssql-server"
        "📛 Boot Logs" "qjournalctl" \
        "📛 Log Viewer (Gui)" "glogg" \
        "📛 System Logs (Gui)" "sudo ksystemlog" \
        "📛 System Logs (Console)" "$shellCmd --hold --title Sys:Install $timeCmd journalctl" \
        "📛 xsession Errors" "$shellCmd --title AWMTT multitail -i $HOME/.xsession-errors" \
        "🚧 Awmtt Default Start" "$shellCmd --hold --title AWMTT awmtt start -N --size 1920x1080" \
        "🚧 Awmtt Default Restart" "$shellCmd --title AWMTT awmtt restart" \
        "🚧 Awmtt Default Stop" "$shellCmd --title AWMTT awmtt stop" \
        "🚧 Awmtt Test Start" "$shellCmd --hold --title AWMTT awmtt start -C $myTestLua -D 1 --size 1920x1080" \
        "🚧 Awmtt Test Restart" "awmtt restart" \
        "🚧 Awmtt Test Stop" "awmtt stop"
)


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

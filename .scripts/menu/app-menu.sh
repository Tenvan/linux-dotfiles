#!/usr/bin/env bash

myFileManager="thunar"
myTerminal="kitty"
timeCmd="/usr/bin/time -v "
myTestLua=$(eval echo $HOME/.config/awesome/rc.test.lua)

# Function create a scale dialog
select_application() {
    zenity --list \
        --width=400 \
        --height=700 \
        --title="Edit Konfiguation" \
        --text="APPLICATIONS" \
        --column=Option \
        --column="Aktion" \
        --separator=" " \
        --print-column=2 \
        --hide-column=2 \
        --hide-header \
        "🧩 Install Base Packages" "$myTerminal --hold --title Sys:Install $timeCmd sh $SCRIPTS/install_base.sh" \
        "🧩 Install Applications" "$myTerminal --hold --title Sys:Install $timeCmd sh $SCRIPTS/install_apps.sh" \
        "🧩 Install Virtual Engines" "$myTerminal --hold --title Sys:Install $timeCmd sh $SCRIPTS/install_vm.sh" \
        "🧩 Install Updates" "$myTerminal --hold --title Sys:Upall $timeCmd paru" \
        "🪣 Cleanup Installs" "$myTerminal --hold --title Sys:Cleanup $timeCmd sudo pacman -Rns $(pacman -Qtdq)" \
        "😃 Emoji Test" "$myTerminal --hold curl https://unicode.org/Public/emoji/5.0/emoji-test.txt" \
        "☦ UTF8 Test" "$myTerminal --hold curl https://www.w3.org/2001/06/utf-8-test/UTF-8-demo.html" \
        "🌤 Wetter Brakel" "$myTerminal --hold --title Wetter:Brakel curl wttr.in/33034?lang=de" \
        "🌤 Wetter Höxter" "$myTerminal --hold --title Wetter:Höxter curl wttr.in/37671?lang=de" \
        "🌤 Wetter Mainz" "$myTerminal --hold --title Wetter:Mainz curl wttr.in/Mainz?lang=de" \
        "♻ Matrix" "$myTerminal --hold --title matrix cmatrix" \
        "📛 xsession Errors" "$myTerminal --title AWMTT multitail -i $HOME/.xsession-errors" \
        "🚧 Awmtt Default Start" "$myTerminal --hold --title AWMTT awmtt start -N --size 1920x1080" \
        "🚧 Awmtt Default Restart" "$myTerminal --title AWMTT awmtt restart" \
        "🚧 Awmtt Default Stop" "$myTerminal --title AWMTT awmtt stop" \
        "🚧 Awmtt Test Start" "$myTerminal --hold --title AWMTT awmtt start -C $myTestLua -D 1 --size 1920x1080" \
        "🚧 Awmtt Test Restart" "awmtt restart" \
        "🚧 Awmtt Test Stop" "awmtt stop"
}

choice=$(select_application)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    echo excecute: $choice >>/dev/stderr
    $choice &
fi

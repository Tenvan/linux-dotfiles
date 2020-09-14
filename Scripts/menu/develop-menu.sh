#!/bin/bash

myFileManager="thunar"
myTerminal="alacritty"
myBrowser="$BROWSER"
workDir=$WORK_DIR
shellCmd="$myTerminal --working-directory $workDir"
timeCmd="/usr/bin/time -v "

# Function create a scale dialog
select_application() {
    zenity --list \
           --width=300 \
           --height=700 \
           --title="Edit Konfiguation" \
           --text="System Menü" \
           --column=Option \
           --column="Aktion" \
           --separator=" " \
           --print-column=2 \
           --hide-column=2 \
           --hide-header \
           "🇬 Git" "sh ~/Scripts/start-editor.sh $WORK_DIR/package.json" \
           "♻ Yarn quick install" "$shellCmd --hold -t OTC:QuickInstall -e $timeCmd yarn install --ignore-scripts" \
           "♻ Yarn full install" "$shellCmd --hold -t OTC:FullInstall -e $timeCmd yarn" \
           "🏄 Start Server" "$shellCmd --hold -t OTC:StartServer -e yarn server:dev" \
           "🇵 Pug watch" "$shellCmd --hold -t OTC:PugWatch -e yarn --cwd src/client pug:watch" \
           "🇵 Pug once" "$shellCmd --hold -t OTC:PugOnce -e yarn --cwd src/client pug:once" \
           "▶ Start" "$shellCmd --hold -t OTC:StartDefault -e $timeCmd yarn --cwd src/client start" \
           "⚗ Generate" "$shellCmd --hold -t OTC:Generate -e $timeCmd yarn generate" \
           "🇺 Check Client Updates" "$shellCmd --hold -t OTC:CheckClientUpdates -e $timeCmd yarn outdated" \
           "🇺 Check Server Updates" "$shellCmd --hold -t OTC:CheckServerUpdates -e $timeCmd yarn --cwd src/server4 outdated" \
           "🆙 Client Upgrade" "$shellCmd --hold -t OTC:ClientUpgrade -e $timeCmd yarn upgrade" \
           "🆙 Server Upgrade" "$shellCmd --hold -t OTC:ServerUpgrade -e $timeCmd yarn --cwd src/server4 upgrade" \
           "🥋 Upgrade Full" "$shellCmd --hold -t OTC:FullUpgrade -e $timeCmd yarn run update:all" \
           "💉 Doctor" "$shellCmd --hold -t OTC:Doctor -e $timeCmd yarn doctor" \
           "💉 Doctor Check" "$shellCmd --hold -t OTC:DoctorCheck -e $timeCmd yarn doctor:check" \
           "♻ Client Check" "$shellCmd --hold -t OTC:ClientCheck -e $timeCmd yarn client:check" \
           "✅ Prod Check" "$shellCmd --hold -t OTC:ClientCheck -e $timeCmd yarn client:check:prod" \
           "🛠 Shell" "$shellCmd --hold -t OTC:Shell" \
           "♻ Dateien" "$myFileManager $workDir"
}

choice=$(select_application)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    echo exec: $choice >>/dev/stderr
    eval $choice &
fi

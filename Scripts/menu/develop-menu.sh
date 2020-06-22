#!/bin/bash

myFileManager="thunar"
myTerminal="alacritty"
myBrowser="firefox"
workDir=$WORK_DIR
shellCmd="$myTerminal --working-directory $workDir"

# Function create a scale dialog
select_application() {
  zenity --list \
    --width=300 \
    --height=600 \
    --title="Edit Konfiguation" \
    --text="System Menü" \
    --column=Option \
    --column="Aktion" \
    --separator=" " \
    --print-column=2 \
    --hide-column=2 \
    --hide-header \
    "Yarn quick install" "$shellCmd --hold -t OTC:QuickInstall -e yarn install --ignore-scripts" \
    "Yarn full install" "$shellCmd --hold -t OTC:FullInstall -e yarn" \
    "Start Server" "$shellCmd --hold -t OTC:StartServer -e yarn server:dev" \
    "Pug watch" "$shellCmd/src/client --hold -t OTC:PugWatch -e yarn pug:watch" \
    "Start Default" "$shellCmd/src/client --hold -t OTC:StartDefault -e yarn start" \
    "Start HMR" "$shellCmd/src/client --hold -t OTC:StartHMR -e yarn start:client:hmr --port 4201" \
    "Start AOT" "$shellCmd/src/client --hold -t OTC:StartAOT -e yarn start:client:dev --aot --port 4202" \
    "Generate" "$shellCmd --hold -t OTC:Generate -e yarn generate" \
    "Check Client Updates" "$shellCmd --hold -t OTC:CheckClientUpdates -e yarn outdated" \
    "Check Server Updates" "$shellCmd --hold -t OTC:CheckServerUpdates -e yarn --cwd src/server4 outdated" \
    "Client Upgrade" "$shellCmd --hold -t OTC:ClientUpgrade -e yarn upgrade" \
    "Server Upgrade" "$shellCmd --hold -t OTC:ServerUpgrade -e yarn --cwd src/server4 upgrade" \
    "Upgrade Full" "$shellCmd --hold -t OTC:FullUpgrade -e yarn run update:all" \
    "Shell" "$shellCmd --hold -t OTC:Shell" \
    "Dateien" "$myFileManager $workDir"
}

choice=$(select_application)

if [ -z "$choice" ]; then
  echo "abort choice"
else
  echo exceute: $choice >>/dev/stderr
  $choice &
fi

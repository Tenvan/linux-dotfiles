#!/usr/bin/env bash

myFileManager="thunar"
myTerminal="kitty"
workDir=$WORK_DIR
shellCmd="$myTerminal --directory $workDir"
timeCmd="/usr/bin/time -v "

filesEdit="code -r --file-uri"
folderEdit="code -r --folder-uri"

startServer() {
    $shellCmd --hold --title OTC:StartServer yarn workspace onetime-server run start &
}

startCompiler() {
    $shellCmd --hold --title OTC:StartDefault yarn workspace onetime-client start &
}

startAll() {
    killall node
    fuser -k 4200/tcp
    fuser -k 4201/tcp
    fuser -k 4202/tcp
    startServer
    startCompiler
}

ACTIONS=(
    "🇬 Git" "code $WORK_DIR"
    "🏄 Start Server" "startServer"
    "🛫 Start Compiler" "startCompiler"
    "🏄 Start All" "startAll"
    "🇬 Generate" "$shellCmd --hold --title OTC:Generate $timeCmd yarn generate"
    "🇩 Deploy Build" "$shellCmd --hold --title OTC:DeployBuild yarn deploy_build"
    "💽 Yarn quick install" "$shellCmd --hold --title OTC:QuickInstall $timeCmd yarn install"
    "💽 Yarn full install" "$shellCmd --hold --title OTC:FullInstall $timeCmd yarn"
    "🇺 Check Updates" "$shellCmd --hold --title OTC:CheckClientUpdates $timeCmd yarn upgrade-interactive"
    "💉 Doctor" "$shellCmd --hold --title OTC:Doctor $timeCmd yarn doctor"
    "💉 Doctor Check" "$shellCmd --hold --title OTC:DoctorCheck $timeCmd yarn doctor:check"
    "☑ Client Check" "$shellCmd --hold --title OTC:ClientCheck $timeCmd yarn client:check"
    "✅ Prod Check" "$shellCmd --hold --title OTC:ClientCheck $timeCmd yarn client:check:prod"
    "📑 Dateien" "$myFileManager $workDir"
    "💻 Shell" "$shellCmd --hold --title OTC:Shell"
)

LINECOUNT=$(expr ${#ACTIONS[*]} / 2)
LINEHEIGHT=$(($LINECOUNT * $LINEHEIGHT))
HEIGHT=$(($LINEHEIGHT + $LINEOFFSET))

# Function create a scale dialog
select_application() {
    yad --center --on-top --sticky \
        --list \
        --no-headers \
        --width=400 \
        --height=$HEIGHT \
        --title="Edit Konfiguation" \
        --text="DEVELOP" \
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

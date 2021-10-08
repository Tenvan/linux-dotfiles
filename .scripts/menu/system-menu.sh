#!/usr/bin/env bash
. $SCRIPTS/defs

SYSTEM_ACTIONS=(
    "📲 Abmelden" "sh $SCRIPTS/session_logout.sh"
    "📵 Sperren" "sh $SCRIPTS/session_lock.sh"
    "📵 Bildschirm sperren" "sh $SCRIPTS/session_lock_screen.sh"
    "🙅 Benutzerwechsel" "sh $SCRIPTS/session_switch_user.sh"
    "🙋 Bereitschaft" "sh $SCRIPTS/session_suspend.sh"
    "🙆 Hibernate" "sh $SCRIPTS/session_hibernate.sh"
    "🪃 Neustart" "sh $SCRIPTS/session_reboot.sh"
    "📴 Runterfahren" "sh $SCRIPTS/session_shutdown.sh"
)

csource "$CUSTOMS/${0##*/}"

ACTIONS=("${CUSTOM_TOP_ACTIONS[@]}" "${SYSTEM_ACTIONS[@]}" "${CUSTOM_BOTTOM_ACTIONS[@]}")
LINECOUNT=$(expr ${#ACTIONS[*]} / 2)
MLINEHEIGHT=$(($LINECOUNT * $LINEHEIGHT))
HEIGHT=$(($MLINEHEIGHT + $LINEOFFSET))

# Function create a scale dialog
select_system_action() {
    yad --center --on-top --sticky \
        --list \
        --no-headers \
        --width=400 \
        --height=$HEIGHT \
        --title="Edit Konfiguation" \
        --text="System Menü" \
        --column=Option \
        --column="Aktion" \
        --separator=" " \
        --print-column=2 \
        --hide-column=2 \
        "${ACTIONS[@]}"
}

choice=$(select_system_action)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    echo execute: $choice >>/dev/stderr
    $choice &
fi

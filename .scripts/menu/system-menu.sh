#!/usr/bin/env bash

LINECOUNT=9
LINEHEIGHT=$(($LINECOUNT * 40))
OFFSET=120
HEIGHT=$(($LINEHEIGHT + $OFFSET))

# Function create a scale dialog
select_system_action() {
    zenity --list \
           --width=400 \
           --height=$HEIGHT \
           --title="Edit Konfiguation" \
           --text="System Menü" \
           --column=Option \
           --column="Aktion" \
           --separator=" " \
           --print-column=2 \
           --hide-column=2 \
           --hide-header \
           "📲 Abmelden" "sh $SCRIPTS/session_logout.sh" \
           "◽ Light Theme" "sh $SCRIPTS/lighttheme.sh" \
           "📵 Sperren" "sh $SCRIPTS/session_lock.sh" \
           "📵 Bildschirm sperren" "sh $SCRIPTS/session_lock_screen.sh" \
           "🙅 Benutzerwechsel" "sh $SCRIPTS/session_switch_user.sh" \
           "🙋 Bereitschaft" "sh $SCRIPTS/session_suspend.sh" \
           "🙆 Hibernate" "sh $SCRIPTS/session_hibernate.sh" \
           "🪃 Neustart" "sh $SCRIPTS/session_reboot.sh" \
           "📴 Runterfahren" "sh $SCRIPTS/session_shutdown.sh"
}

choice=$(select_system_action)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    echo execute: $choice >>/dev/stderr
    $choice &
fi

#!/bin/bash

# Function create a scale dialog
select_system_action() {
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
           " Matrix" "alacritty -e cmatrix -C green" \
           " Abmelden" "sh $HOME$/Scripts/session_logout.sh" \
           " Light Theme" "sh $HOME/Scripts/lighttheme.sh" \
           " Sperren" "sh $HOME/Scripts/session_lock.sh" \
           " Bildschirm sperren" "sh $HOME/Scripts/session_lock_screen.sh" \
           " Benutzerwechsel" "sh $HOME/Scripts/session_switch_user.sh" \
           " Bereitschaft" "sh $HOME/Scripts/session_suspend.sh" \
           " Hibernate" "sh $HOME/Scripts/session_hibernate.sh" \
           " Neustart" "sh $HOME/Scripts/session_reboot.sh" \
           " Runterfahren" "sh $HOME/Scripts/session_shutdown.sh"
}

choice=$(select_system_action)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    echo execute: $choice >>/dev/stderr
    $choice &
fi

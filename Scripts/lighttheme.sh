#!/usr/bin/env bash
cd ~
git checkout light-theme -f;
xrdb -merge ~/.Xresources;
notif-send -u critical 'Auf Light-Theme gewechselt. QT5 Anwendungen neu starten.'

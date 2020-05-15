#!/usr/bin/env bash
cd ~
git checkout dark-theme -f;
xrdb -merge ~/.Xresources;
notif-send -u critical 'Auf Dark-Theme gewechselt. QT5 Anwendungen neu starten.'

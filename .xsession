#!/usr/bin/env bash
#
# ~/.xsession
#
# executed from lightdm -> XSession
#
# custom resources
echo 'Running ~/.xsession'

sh ~/.scripts/autostart-global.sh &

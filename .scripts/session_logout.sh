#!/usr/bin/env bash
echo "Logout from session..."
sudo systemctl restart lightdm
sudo systemctl restart lightdm-plymouth

#!/usr/bin/env bash
echo "reboot machine..."
sudo systemctl stop mssql-server
shutdown --reboot 0

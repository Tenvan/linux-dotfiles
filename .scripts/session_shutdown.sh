#!/usr/bin/env bash
echo "shutdown machine..."
sudo systemctl stop mssql-server
shutdown --poweroff 0

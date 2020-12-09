#!/usr/bin/env bash

# Install mssql server
yay -S mssql-server

dataRootDir=/media/BIGDATA/DATA
 
# Optional: Standard Verzeichnisse setzen
sudo mkdir $dataRootDir/data
sudo mkdir $dataRootDir/backup
sudo mkdir $dataRootDir/dump
sudo mkdir $dataRootDir/log
 
sudo chown -R mssql:mssql $dataRootDir
 
sudo /opt/mssql/bin/mssql-conf set filelocation.defaultdatadir $dataRootDir/data
sudo /opt/mssql/bin/mssql-conf set filelocation.defaultbackupdir $dataRootDir/backup
sudo /opt/mssql/bin/mssql-conf set filelocation.defaultdumpdir $dataRootDir/dump
sudo /opt/mssql/bin/mssql-conf set filelocation.defaultlogdir $dataRootDir/log
 
# Server konfigurieren
sudo /opt/mssql/bin/mssql-conf setup


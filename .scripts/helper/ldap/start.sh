#!/usr/bin/env bash

. ./ldap_config.sh

echo "Aktuell installierte Container:"
sudo docker container ls

echo "Starte: $ldap_container_name $admin_container_name"
sudo docker container start $ldap_container_name $admin_container_name


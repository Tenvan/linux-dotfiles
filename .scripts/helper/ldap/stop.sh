#!/usr/bin/env bash

. ./ldap_config.sh

echo "Aktuell installierte Container:"
sudo docker container ls

echo "Stoppe: $ldap_container_name $admin_container_name"
sudo docker container stop $ldap_container_name $admin_container_name

 

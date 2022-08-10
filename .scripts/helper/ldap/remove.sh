#!/usr/bin/env bash

. ./ldap_config.sh

echo "Aktuell installierte Container:"
sudo docker container ls

sudo docker container stop $ldap_container_name $admin_container_name

sudo docker container prune --force

echo "Aktuell installierte Container:"
sudo docker container ls


#!/usr/bin/env bash

. ./ldap_config.sh

PHPLDAP_IP=$(sudo docker inspect -f "{{ .NetworkSettings.IPAddress }}" $admin_container_name)

# Admin testen
echo "Go to: https://$PHPLDAP_IP"
echo "Login DN: cn=$admin,dc=$domain_host,dc=$domain_suffix"
echo "Password: $adminpw"

xdg-open "https://$PHPLDAP_IP"


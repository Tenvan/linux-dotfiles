#!/usr/bin/env bash

. ./ldap_config.sh

# LDAP Container erstellen und starten
sudo docker run -dit --privileged --restart unless-stopped \
     -p 389:389 -p 636:636 \
     --name $ldap_container_name \
     --hostname $ldap_host_name \
     -v $ldap_dir:/var/lib/ldap \
     -v $slapd_dir:/etc/ldap/slapd.d  \
     --env LDAP_ORGANISATION=$organisation \
     --env LDAP_DOMAIN=$domain_host.$domain_suffix \
     --env LDAP_ADMIN_PASSWORD=$adminpw \
     --detach osixia/openldap

# Ldap Server testen
sudo docker exec $ldap_container_name ldapsearch -x -H ldap://localhost -b dc=$domain_host,dc=$domain_suffix -D "cn=$admin,dc=$domain_host,dc=$domain_suffix" -w $adminpw

# Admin Container erstellen und starten
sudo docker run -dit --privileged --restart unless-stopped \
	 -p $admin_port:443 \
     --name $admin_container_name \
     --hostname $admin_host_name \
     --link $ldap_container_name:ldap-host \
     --env PHPLDAPADMIN_LDAP_HOSTS=ldap-host \
     --detach osixia/phpldapadmin


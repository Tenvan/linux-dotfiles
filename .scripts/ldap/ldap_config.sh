#!/usr/bin/env bash

# Konfiguration LDAP
export ldap_container_name=ldap-container
export ldap_host_name=ldap-service
export ldap_dir=/srv/ldap/data
export slapd_dir=/srv/ldap/slap
export organisation=Infoniqa
export domain_host=infoniqa
export domain_suffix=com
export admin=admin
export adminpw=time

# Konfiguration ADMIN
export admin_container_name=phpldapadmin-container
export admin_host_name=phpldapadmin_service
export ldap_host=localhost
export admin_port=6443

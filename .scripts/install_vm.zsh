#!/usr/bin/env zsh

. ~/.scripts/defs.zsh

# Init Install
initInstall "install_vm"

#####################
# init distro check #
#####################

INSTALL_VIRTIO=true
INSTALL_VIRTUALBOX=false

###########################
# collect needed packages #
###########################

# virtualbox
if [ $INSTALL_VIRTUALBOX = true ]; then
	if [ $IS_GARUDA = true ]; then
		inst virtualbox-meta
	elif [ $IS_MANJARO = true ]; then
		inst virtualbox
		inst virtualbox-ext-oracle
	else
		inst virtualbox
		inst virtualbox-ext-oracle
		inst virtualbox-host-modules-arch
		inst linux-headers
	fi
fi

# libvirt service and manager
if [ $INSTALL_VIRTIO = true ]; then
	if [ $IS_GARUDA = true ]; then
		inst virt-manager-meta
	else
		inst virt-manager
		inst virt-viewer
		uninst virt-backup
		inst virtio-win
		inst qemu
		inst qemu-arch-extra
		inst libvirt
		inst iptables-nft
		inst ebtables
		inst dnsmasq
		#inst ssh-askpass-fullscreen
	fi
fi

###############################
# uninstall unneeded packages #
###############################
fullUninstall

#################################
# install all (needed) packages #
#################################
fullInstall

## FINISHING #
finish

sudo systemctl enable --now libvirtd
errorCheck "libvirtd service"

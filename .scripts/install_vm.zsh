#!/usr/bin/env zsh

. ~/.scripts/defs.zsh

#####################
# init distro check #
#####################

INSTALL_VIRTIO=true
INSTALL_VIRTUALBOX=false

errorCheck() {
	retVal=$?
	if [ $retVal -ne 0 ]; then
		print "abort installation script 'install_vm': $1"
		exit $retVal
	fi
}

inst() {
	PAKAGE_INST="${PAKAGE_INST} $1"

	if [ $DEBUG = true ]; then
		eval "$PACKER -S $PAKKU_ALL $1"

		retVal=$?
		if [ $retVal -ne 0 ]; then
			print "error on install: $1"
			ERROR_PAKAGE_INST="${ERROR_PAKAGE_INST}
$1"
		fi
	fi
}

uninst() {
	PAKAGE_UNINST="${PAKAGE_UNINST} $1"

	if [ $DEBUG = true ]; then
		eval "$PACKER -R --noconfirm $1"

		retVal=$?
		if [ $retVal -ne 0 ]; then
			print "error on uninstall: $1"
			ERROR_PAKAGE_UNINST="${ERROR_PAKAGE_UNINST}
$1"
		fi
	fi
}

###########################
# collect needed packages #
###########################

# virtualbox
if [ $INSTALL_VIRTUALBOX = true ]; then
	if [ $IS_GARUDA = true ]; then
		inst virtualbox-meta
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
		inst virt-backup
		inst qemu
		inst qemu-arch-extra
		inst libvirt
		inst ebtables 
		inst dnsmasq
	fi
fi

#################################
# install all (needed) packages #
#################################
if [ $DEBUG != "TRUE" ]; then
	eval "$PACKER -S $PAKKU_ALL $PAKAGE_INST"
	errorCheck "install packages"
fi

sudo systemctl enable --now libvirtd
errorCheck "libvirtd service"

## FINISHING #
if [ $ERROR_PAKAGE_UNINST ]; then
	print 'No Errors on Install'
else
	print "Error in Inst: ${ERROR_PAKAGE_INST}"
fi

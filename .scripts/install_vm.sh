#!/usr/bin/env bash

sh $SCRIPTS/defs.sh

#####################
# init distro check #
#####################
DEBUG=FALSE
#DEBUG=TRUE

errorCheck() {
    retVal=$?
    if [ $retVal -ne 0 ]; then
        echo "abort installation script 'install_vm': $1"
        exit $retVal
    fi
}

inst() {
    PAKAGE_INST="${PAKAGE_INST} $1"
    
    if [ $DEBUG = "TRUE" ]; then
		pakku -S $PAKKU_ALL $1
		
	    retVal=$?
	    if [ $retVal -ne 0 ]; then
	        echo "error on install: $1"
			ERROR_PAKAGE_INST="${ERROR_PAKAGE_INST}
$1"        
	    fi
    fi
}

uninst() {
    PAKAGE_UNINST="${PAKAGE_UNINST} $1"

    if [ $DEBUG = "TRUE" ]; then
	    pakku -R --noconfirm $1
	    
	    retVal=$?
	    if [ $retVal -ne 0 ]; then
	        echo "error on uninstall: $1"
			ERROR_PAKAGE_UNINST="${ERROR_PAKAGE_UNINST}
$1"        
	    fi
	fi
}

###########################
# collect needed packages #
###########################

# virtualbox
if [ $IS_GARUDA = true ]; then
	inst virtualbox-meta
else
	inst virtualbox
	inst virtualbox-ext-oracle
	inst virtualbox-host-modules-arch
	inst linux-headers
fi

# libvirt service and manager
if [ $IS_GARUDA = true ]; then
	inst virt-manager-meta
else
	inst virt-manager
	inst qemu
	inst qemu-arch-extra
	inst libvirt
fi

#################################
# install all (needed) packages #
#################################
if [ $DEBUG != "TRUE" ]; then
	echo "INST: $PAKKU_PAKAGE"
	pakku -S $PAKKU_ALL $PAKAGE_INST
	errorCheck "install packages"
fi

#echo "Error in Uninst: ${ERROR_PAKAGE_UNINST}"
echo "Error in Inst: ${ERROR_PAKAGE_INST}"

## FINISHING #

sudo systemctl enable libvirtd.service
sudo systemctl start libvirtd.service
errorCheck "libvirtd service"

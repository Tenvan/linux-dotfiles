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
        echo "abort installation script 'install_all': $1"
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

# .virtualbox
inst virtualbox
inst virtualbox-ext-oracle
inst virtualbox-host-modules-arch
inst linux-headers

# libvirt service and manager
inst virt-manager
inst qemu
inst qemu-arch-extra
inst libvirt

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

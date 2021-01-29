#!/usr/bin/env bash

. $SCRIPTS/defs.sh

#####################
# init distro check #
#####################
#DEBUG=false
DEBUG=true
PACKER=paru

errorCheck() {
    retVal=$?
    if [ $retVal -ne 0 ]; then
        echo "abort installation script 'install_rust_apps': $1"
        exit $retVal
    fi
}

inst() {
    PAKAGE_INST="${PAKAGE_INST} $1"
    
    if [ $DEBUG = "TRUE" ]; then
		$PACKER -S $PAKKU_ALL $1
		
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
	    $PACKER -R --noconfirm $1
	    
	    retVal=$?
	    if [ $retVal -ne 0 ]; then
	        echo "error on uninstall: $1"
			ERROR_PAKAGE_UNINST="${ERROR_PAKAGE_UNINST}
$1"        
	    fi
	fi
}

#####################
# collect rust apps #
#####################

inst bat
inst fd
inst procs
inst ripgrep
inst tokei

sudo rm /var/lib/pacman/db.lck

###############################
# uninstall unneeded packages #
###############################
if [ $DEBUG != true ]; then
	echo "UNINST: $PAKKU_PAKAGE_U"
	$PACKER -R --noconfirm $PAKAGE_UNINST
	#errorCheck "uninstall packages"
fi

#################################
# install all (needed) packages #
#################################
if [ $DEBUG != true ]; then
	echo "INST: $PAKKU_PAKAGE"
	$PACKER -S $PAKKU_ALL $PAKAGE_INST
	errorCheck "install packages"
fi

## FINISHING #

#echo "Error in Uninst: ${ERROR_PAKAGE_UNINST}"
echo "Error in Inst: ${ERROR_PAKAGE_INST}"

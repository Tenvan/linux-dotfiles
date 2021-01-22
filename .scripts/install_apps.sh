#!/usr/bin/env bash

. $SCRIPTS/defs.sh

#####################
# init distro check #
#####################
DEBUG=FALSE
#DEBUG=TRUE

errorCheck() {
    retVal=$?
    if [ $retVal -ne 0 ]; then
        echo "abort installation script 'install_apps': $1"
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

#########################
# collect optional apps #
#########################

# gimicks
inst cmatrix
inst hollywood

# Office
inst libreoffice-fresh
inst libreoffice-fresh-de

# Browser
inst google-chrome
inst microsoft-edge-dev-bin
inst vivaldi

# optional application packages
inst aspell
inst evolution
inst gimp
inst gimp-help-de
inst imagemagick
inst inkscape
inst partitionmanager
inst pinta
inst playerctl
inst pstoedit
inst python-lxml
inst python-numpy
inst radiotray
inst teams
inst themix-full-git

sudo rm /var/lib/pacman/db.lck

###############################
# uninstall unneeded packages #
###############################
if [ $DEBUG != "TRUE" ]; then
	echo "UNINST: $PAKKU_PAKAGE_U"
	pakku -R --noconfirm $PAKAGE_UNINST
	#errorCheck "uninstall packages"
fi

#################################
# install all (needed) packages #
#################################
if [ $DEBUG != "TRUE" ]; then
	echo "INST: $PAKKU_PAKAGE"
	pakku -S $PAKKU_ALL $PAKAGE_INST
	errorCheck "install packages"
fi

## FINISHING #

#echo "Error in Uninst: ${ERROR_PAKAGE_UNINST}"
echo "Error in Inst: ${ERROR_PAKAGE_INST}"

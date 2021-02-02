#!/usr/bin/env zsh

. ~/.scripts/defs.zsh

errorCheck() {
	retVal=$?
	if [ $retVal -ne 0 ]; then
		print "abort installation script 'install_rust_apps': $1"
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
	eval "$PACKER -R --noconfirm $PAKAGE_UNINST"
	#errorCheck "uninstall packages"
fi

#################################
# install all (needed) packages #
#################################
if [ $DEBUG != true ]; then
	eval "$PACKER -S $PAKKU_ALL $PAKAGE_INST"
	errorCheck "install packages"
fi

## FINISHING #
if [ $ERROR_PAKAGE_UNINST ]; then
	print 'No Errors on Install'
else
	print "Error in Inst: ${ERROR_PAKAGE_INST}"
fi

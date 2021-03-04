#!/usr/bin/env zsh

. ~/.scripts/defs.zsh

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

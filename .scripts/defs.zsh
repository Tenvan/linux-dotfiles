#!/usr/bin/env zsh

#####################
# init distro check #
#####################
LINUX_VERSION_NAME=$(lsb_release -si)
IS_ARCH=false
IS_ARCO=false
IS_MANJARO=false
IS_GARUDA=false

SCRIPTS="$HOME/.scripts"

#PACKER=pikaur
PACKER=pikaur
DEBUG=false

#DEBUG=true
ERROR_PAKAGE_UNINST=
ERROR_PAKAGE_INST=

if [ $LINUX_VERSION_NAME = "ArchLinux" ]; then
	IS_ARCH=true
fi

if [ $LINUX_VERSION_NAME = "ArcoLinux" ]; then
	IS_ARCO=true
fi

if [ $LINUX_VERSION_NAME = "ManjaroLinux" ]; then
	IS_MANJARO=true
fi

if [ $LINUX_VERSION_NAME = "Garuda" ]; then
	IS_GARUDA=true
fi

print "Linux Version: $LINUX_VERSION_NAME"
print "IsArch:        $IS_ARCH"
print "IsArco:        $IS_ARCO"
print "IsGaruda:      $IS_GARUDA"
print "IsManjaro:     $IS_MANJARO"

MAKEFLAGS="-j$(nproc)"
PAKKU_ALL="--color always --needed "

initInstall() {
	INSTALL_SCRIPT=$1
	echo "Step: init Install '$INSTALL_SCRIPT'"
	sudo rm /var/lib/pacman/db.lck 2&> /dev/null	
}

inst() {
    PAKAGE_INST="${PAKAGE_INST} $1"
    
    if [ $DEBUG = true ]; then
		eval "$PACKER -S --needed --noconfirm $PAKKU_ALL $1"
		
	    retVal=$?
	    if [ $retVal -ne 0 ]; then
	        echo "error on install: $1"
			ERROR_PAKAGE_INST="${ERROR_PAKAGE_INST} $1"        
	    fi
    fi
}

fullInstall() {
	echo "Step: full Install"
	if [ $DEBUG != true -a "$PAKAGE_INST" != "" ]; then
		eval "$PACKER -S --color always --needed --noconfirm $PAKAGE_INST"
		# errorCheck "install packages"
	fi
}

uninst() {
    PAKAGE_UNINST="${PAKAGE_UNINST} $1"

    if [ $DEBUG = true ]; then
	    eval "$PACKER -R --noconfirm $1"
	    
	    retVal=$?
	    if [ $retVal -ne 0 ]; then
	        echo "error on uninstall: $1"
			ERROR_PAKAGE_UNINST="${ERROR_PAKAGE_UNINST} $1"        
	    fi
	fi
}

fullUninstall() {
	printf "Step: full Uninstall"
	if [ $DEBUG != true -a "$PAKAGE_UNINST" != "" ]; then
		eval "$PACKER -R --noconfirm $PAKAGE_UNINST"
	fi
}

errorCheck() {
    retVal=$?
    if [ $retVal -ne 0 ]; then
        echo "abort installation script '$INSTALL_SCRIPT': $1 ($retVal)"
        exit $retVal
    fi
}

finish() {
	echo "Step: finish Install"

	if [ "$ERROR_PAKAGE_UNINST" = "" ]; then
		echo 'No Errors on Uninstall'
	else
		echo "Error on Uninstall: ${ERROR_PAKAGE_UNINST}"
	fi	

	if [ "$ERROR_PAKAGE_INST" = "" ]; then
		echo 'No Errors on Install'
	else
		echo "Error on Install: ${ERROR_PAKAGE_INST}"
	fi	
}

function run() {
    if command -v $1 &>/dev/null; then
        #pgrep -u "$USER" -fx "$1" >/dev/null || ($@) &
        if ! pgrep "$1"; then
            $@ &
        fi
    else
        notify-send -t 10000 -u critical "Error Autostart" "Kommando '$1' nicht gefunden"
    fi
}

function restart() {
    killall "$1"
    sleep 1
    run $@
}

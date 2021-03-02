#!/usr/bin/env zsh

errorCheck() {
    retVal=$?
    if [ $retVal -ne 0 ]; then
        print "abort installation script 'install_base': $1"
        exit $retVal
    fi
}

inst() {
    PAKAGE_INST="${PAKAGE_INST} $1"
    
	eval "brew install $1"
		
    retVal=$?
    if [ $retVal -ne 0 ]; then
        print "error on install: $1"
		ERROR_PAKAGE_INST="${ERROR_PAKAGE_INST} $1"        
    fi
}

uninst() {
    PAKAGE_UNINST="${PAKAGE_UNINST} $1"

    eval "brew remove $1"
	    
    retVal=$?
    if [ $retVal -ne 0 ]; then
        print "error on uninstall: $1"
		ERROR_PAKAGE_UNINST="${ERROR_PAKAGE_UNINST} $1"        
    fi
}

###########################
# collect needed packages #
###########################

# system packages
inst git
inst jetbrains-toolbox
inst mc
inst ranger
inst micro
inst visual-studio-code-bin
inst graphviz
inst ripgrep
inst bat
inst fd
inst procs
inst tokei

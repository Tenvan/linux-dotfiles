
#################################
# install all (needed) packages #
#################################

sh ~/Scripts/install_system.sh
errorCheck "install_system"

sh ~/Scripts/install_develop.sh
errorCheck "install_develop"

sh ~/Scripts/install_wm.sh
errorCheck "install_wm"

sh ~/Scripts/install_desktop.sh
errorCheck "install_desktop"

sh ~/Scripts/install_editors.sh
errorCheck "install_editors"

sh ~/Scripts/install_arts.sh
errorCheck "install_arts"


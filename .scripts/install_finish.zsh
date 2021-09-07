#!/usr/bin/env zsh

. ~/.scripts/defs.zsh
. ~/.scripts/sounds

# Init Install
initInstall "install_finish"

# Install packages for finishing
inst libmagick-full
inst terminus-font
inst powerline-fonts

if [ $IS_MANJARO = true ]; then
	inst manjaro-wallpapers-18.0
	inst bootsplash-systemd
	inst bootsplash-theme-manjaro
fi

# powerline in linux console
if [ $IS_GARUDA = true ]; then
    inst terminess-powerline-font-git
fi

###############################
# uninstall unneeded packages #
###############################
fullUninstall

#################################
# install all (needed) packages #
#################################
fullInstall

## FINISHING #
finish

# refresh icons
sudo gdk-pixbuf-query-loaders --update-cache

if [ ! -f "$HOME/.screenlayout/screenlayout.sh" ]; then
	print 'Fehler: keine .screenlayout.sh gefunden'
	exit -1
fi

sudo cp $HOME/.screenlayout/screenlayout.sh /opt
errorCheck "copy screenlayout"

#GREETER="lightdm-gtk-greeter"
#GREETER="lightdm-slick-greeter"
GREETER="lightdm-webkit2-greeter"

sudo sed -i "s/^.*greeter-session=.*$/greeter-session=$GREETER/g" /etc/lightdm/lightdm.conf
sudo sed -i "s/^.*display-setup-script=.*$/display-setup-script=\/opt\/screenlayout.sh/g" /etc/lightdm/lightdm.conf
# sudo sed -i "s/^.*greeter-setup-script\s*=.*$/greeter-setup-script=\/opt\/system-ready.sh/g" /etc/lightdm/lightdm.conf

sudo cp $oxygen/system-ready.oga /opt/system-ready.oga
echo "paplay /opt/system-ready.oga" | sudo tee /opt/system-ready.sh
sudo chmod +x /opt/system-ready.sh

errorCheck "lightdm config"

# config slick-greeter
if [ $IS_MANJARO = true ]; then
	GREETER_BACKGROUND="/usr/share/backgrounds/manjaro-wallpapers-18.0/manjaro-cat.jpg"
elif [ $IS_ARCO = true ]; then
	GREETER_BACKGROUND="/usr/share/backgrounds/arcolinux/arco-wallpaper.jpg"
else
	GREETER_BACKGROUND="/usr/share/backgrounds/desert.png"
fi

echo "[Greeter]
background=$GREETER_BACKGROUND
theme-name=Materia-dark
icon-theme-name=Papirus-Dark
activate-numlock=true
cursor-theme-name=Bibata-Modern-Ice
font-name='Cantarell Bold 14'
xft-antialias=true
xft-hintstyle=hintfull
enable-hidpi=auto
draw-user-backgrounds=false
activate-numlock=true
show-power=false
show-a11y=false" | sudo tee /etc/lightdm/slick-greeter.conf
errorCheck "lightdm greeter config"

# config webkit2 greeter
# Set default lightdm greeter to lightdm-webkit2-greeter
sudo sed -i 's/^\(#?greeter\)-session\s*=\s*\(.*\)/greeter-session = lightdm-webkit2-greeter #\1/ #\2g' /etc/lightdm/lightdm.conf
sudo sed -i 's/^debug_mode\s*=\s*\(.*\)/debug_mode = true #\1/g' /etc/lightdm/lightdm-webkit2-greeter.conf
sudo sed -i 's/^webkit_theme\s*=\s*\(.*\)/webkit_theme = glorious #\1/g' /etc/lightdm/lightdm-webkit2-greeter.conf

echo "KEYMAP=de
FONT=ter-powerline-v12n
FONT_MAP=" | sudo tee /etc/vconsole.conf

# Default Browser setzen (vorher $BROWSER Variable entfernen)
export BROWSER=
xdg-settings set default-web-browser google-chrome.desktop

sudo fc-cache -fv
errorCheck "fontcache"

rm -f $PKG_FILE
rm -f $PKG_UNINST_FILE

sudo usermod -aG docker $USER

###########################
# enable services

# display manager
sudo systemctl disable ly.service
sudo systemctl disable sddm.service
sudo systemctl enable --now lightdm.service
errorCheck "lightdm service"

# printer Service
sudo systemctl enable --now cups.socket
sudo systemctl enable --now cups.service
errorCheck "printer service"

# docker
sudo systemctl enable --now docker
errorCheck "docker service"

#sudo systemctl enable --now bluetooth-autoconnect
#errorCheck "bluetooth-autoconnect service"

sudo systemctl enable --now fstrim.timer
errorCheck "fstrim service"

mkdir -p ~/.config/systemd/user/
sudo cp /usr/lib/systemd/user/pulseaudio-bluetooth-autoconnect.service /etc/systemd/user
systemctl enable pulseaudio-bluetooth-autoconnect --user --now
#errorCheck "pulseaudio-bluetooth-autoconnect service"

# grub config
sed 's/.*GRUB_GFXMODE=.*$/GRUB_GFXMODE="1920x1080,auto"/g' </etc/default/grub >grub
sudo mv -f grub /etc/default

if [ $IS_MANJARO = true ]; then
	sudo convert /usr/share/backgrounds/manjaro-wallpapers-18.0/manjaro-cat.jpg /usr/share/grub/themes/manjaro/background.png
	errorCheck "convert manjaro grub image"
	sed 's/.*GRUB_THEME=.*$/GRUB_THEME="\/usr\/share\/grub\/themes\/manjaro\/theme.txt"/g' </etc/default/grub >grub
	sudo mv -f grub /etc/default
	errorCheck "move manjaro grub file"
	sudo cp $SCRIPTS/setup/manjaro-cat.png /usr/share/grub/themes/manjaro/background.png
	sed 's/.*GRUB_THEME=.*$/GRUB_THEME="\/usr\/share\/grub\/themes\/manjaro\/theme.txt"/g' </etc/default/grub >grub
	sudo mv -f grub /etc/default
	echo '#
#
# ==> ADD "bootsplash.bootfile=bootsplash-themes/manjaro/bootsplash" to GRUB_CMDLINE_LINUX_DEFAULT
#' | sudo tee -a /etc/default/grub

	echo '#
#
# ==> ADD "bootsplash-manjaro" to HOOKS
#' | sudo tee -a /etc/mkinitcpio.conf
elif [ $IS_ARCO = true ]; then
	sed 's/.*GRUB_THEME=.*$/GRUB_THEME="\/usr\/share\/grub\/themes\/Vimix\/theme.txt"/g' </etc/default/grub >grub
	sudo mv -f grub /etc/default
	errorCheck "move arco grub file"
elif [ $IS_ARCO != true ]; then
	sed 's/.*GRUB_THEME=.*$/GRUB_THEME="\/boot\/grub\/themes\/Stylish\/theme.txt"/g' </etc/default/grub >grub
	sudo mv -f grub /etc/default
elif [ $IS_GARUDA = true ]; then
	sudo cp $SCRIPTS/setup/manjaro-cat.png /usr/share/grub/themes/garuda/background.png
	sed 's/.*GRUB_THEME=.*$/GRUB_THEME="\/usr\/share\/grub\/themes\/garuda\/theme.txt"/g' </etc/default/grub >grub
	sudo mv -f grub /etc/default
elif [ $IS_ENDEA = false ]; then
	sed 's/.*GRUB_THEME=.*$/GRUB_THEME="\/boot\/grub\/themes\/EndeavourOS\/theme.txt"/g' </etc/default/grub >grub
	sudo mv -f grub /etc/default
else
	sed 's/.*GRUB_THEME=.*$/GRUB_THEME="\/boot\/grub\/themes\/Archlinux\/theme.txt"/g' </etc/default/grub >grub
	sudo mv -f grub /etc/default
fi
errorCheck "grub config"

sudo micro /etc/mkinitcpio.conf
sudo micro /etc/default/grub

sudo mkinitcpio -P
sudo grub-mkconfig -o /boot/grub/grub.cfg
errorCheck "grub mkconfig"

# login screen console
sudo cp $SCRIPTS/issue /etc

# nodejs tools for editors
sudo npm uninstall -g eslint jshint jsxhint stylelint sass-lint markdownlint-cli raml-cop typescript tern js-beautify iconv-lite
errorCheck "uninstall global npm"

yarn global upgrade
yarn global add eslint jshint jsxhint stylelint sass-lint markdownlint-cli raml-cop typescript tern js-beautify iconv-lite
errorCheck "install global yarn"

sound complete

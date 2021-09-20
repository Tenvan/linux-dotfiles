#!/usr/bin/env zsh

. ~/.scripts/defs.zsh

# Init Install
initInstall "install_finish"
sound count-down

if [ $IS_MANJARO = true ]; then
	inst manjaro-wallpapers-18.0
	inst bootsplash-systemd
	inst bootsplash-theme-manjaro
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
GREETER="lightdm-slick-greeter"
#GREETER="lightdm-webkit2-greeter"

sudo sed -i "s/^.*greeter-session=.*$/greeter-session=$GREETER/g" /etc/lightdm/lightdm.conf
sudo sed -i "s/^.*display-setup-script=.*$/display-setup-script=\/opt\/screenlayout.sh/g" /etc/lightdm/lightdm.conf
# sudo sed -i "s/^.*greeter-setup-script\s*=.*$/greeter-setup-script=\/opt\/system-ready.sh/g" /etc/lightdm/lightdm.conf

sudo cp $oxygen/system-ready.oga /opt/system-ready.oga
echo "paplay /opt/system-ready.oga" | sudo tee /opt/system-ready.sh
sudo chmod +x /opt/system-ready.sh

errorCheck "lightdm config"

# config slick-greeter
GREETER_THEME="Adwaita-dark"
if [ $IS_MANJARO = true ]; then
	GREETER_BACKGROUND="/usr/share/backgrounds/manjaro-wallpapers-18.0/manjaro-cat.jpg"
elif [ $IS_ARCO = true ]; then
	GREETER_BACKGROUND="/usr/share/backgrounds/arcolinux/arco-wallpaper.jpg"
	GREETER_THEME="Arc-dark"
else
	GREETER_BACKGROUND="/usr/share/backgrounds/desert.png"
fi

echo "# LightDM GTK+ Configuration
# Available configuration options listed below.
#
# activate-numlock=Whether to activate numlock. This features requires the installation of numlockx. (true or false)
# background=Background file to use, either an image path or a color (e.g. #772953)
# background-color=Background color (e.g. #772953), set before wallpaper is seen
# draw-user-backgrounds=Whether to draw user backgrounds (true or false)
# draw-grid=Whether to draw an overlay grid (true or false)
# show-hostname=Whether to show the hostname in the menubar (true or false)
# show-power=Whether to show the power indicator in the menubar (true or false)
# show-a11y=Whether to show the accessibility options in the menubar (true or false)
# show-keyboard=Whether to show the keyboard indicator in the menubar (true or false)
# show-clock=Whether to show the clock in the menubar (true or false)
# show-quit=Whether to show the quit menu in the menubar (true or false)
# logo=Logo file to use
# other-monitors-logo=Logo file to use for other monitors
# theme-name=GTK+ theme to use
# icon-theme-name=Icon theme to use
# font-name=Font to use
# xft-antialias=Whether to antialias Xft fonts (true or false)
# xft-dpi=Resolution for Xft in dots per inch
# xft-hintstyle=What degree of hinting to use (hintnone/hintslight/hintmedium/hintfull)
# xft-rgba=Type of subpixel antialiasing (none/rgb/bgr/vrgb/vbgr)
# onscreen-keyboard=Whether to enable the onscreen keyboard (true or false)
# high-contrast=Whether to use a high contrast theme (true or false)
# screen-reader=Whether to enable the screen reader (true or false)
# play-ready-sound=A sound file to play when the greeter is ready
# hidden-users=List of usernames that are hidden until a special key combination is hit
# group-filter=List of groups that users must be part of to be shown (empty list shows all users)
# enable-hidpi=Whether to enable HiDPI support (on/off/auto)
# only-on-monitor=Sets the monitor on which to show the login window, -1 means "follow the mouse"
# stretch-background-across-monitors=Whether to stretch the background across multiple monitors (false by default)
# clock-format=What clock format to use (e.g., %H:%M or %l:%M %p)
[Greeter]
activate-numlock=true
background=$GREETER_BACKGROUND
draw-user-backgrounds=false
draw-grid=true
show-hostname=true
show-power=true
show-a11y=false
show-quit=true
theme-name=$GREETER_THEME
icon-theme-name=Papirus-Dark
font-name='Noto Sans Bold 20'
xft-antialias=true
xft-dpi=200
xft-hintstyle=hintfull
enable-hidpi=on
cursor-theme-name=Bibata-Modern-Ice" | sudo tee /etc/lightdm/slick-greeter.conf
errorCheck "lightdm greeter config"

# config webkit2 greeter
# Set default lightdm greeter
sudo sed -i 's/^\(#?greeter\)-session\s*=\s*\(.*\)/greeter-session = $GREETER #\1/ #\2g' /etc/lightdm/lightdm.conf
sudo sed -i 's/^debug_mode\s*=\s*\(.*\)/debug_mode = true #\1/g' /etc/lightdm/lightdm-webkit2-greeter.conf
sudo sed -i 's/^webkit_theme\s*=\s*\(.*\)/webkit_theme = glorious #\1/g' /etc/lightdm/lightdm-webkit2-greeter.conf

echo "KEYMAP=de
FONT=ter-powerline-v12n
FONT_MAP=" | sudo tee /etc/vconsole.conf

# Default Browser setzen (vorher $BROWSER Variable entfernen)
export BROWSER=
xdg-settings set default-web-browser vivaldi-stable.desktop

# Standard Dateimanager setzen
xdg-mime default nemo.desktop inode/directory application/x-gnome-saved-search

sudo fc-cache -fv
errorCheck "fontcache"

rm -f $PKG_FILE
rm -f $PKG_UNINST_FILE

sudo usermod -aG docker $USER

###########################
# enable services

# display manager
sudo systemctl disable sddm.service
sudo systemctl enable lightdm.service
errorCheck "lightdm service"

# printer Service
sudo systemctl enable cups.socket
sudo systemctl enable cups.service
errorCheck "printer service"

# docker
sudo systemctl enable docker
errorCheck "docker service"

#sudo systemctl enable --now bluetooth-autoconnect
#errorCheck "bluetooth-autoconnect service"

sudo systemctl enable fstrim.timer
errorCheck "fstrim service"

mkdir -p ~/.config/systemd/user/
sudo cp /usr/lib/systemd/user/pulseaudio-bluetooth-autoconnect.service /etc/systemd/user
systemctl enable pulseaudio-bluetooth-autoconnect --user
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

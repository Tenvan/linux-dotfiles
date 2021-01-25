#!/usr/bin/env bash

export SCRIPTS=~/.scripts

. $SCRIPTS/defs.sh

errorCheck() {
    retVal=$?
    if [ $retVal -ne 0 ]; then
        echo "abort installation script 'install_all': $1"
        exit $retVal
    fi
}

## FINISHING #

# refresh icons
sudo gdk-pixbuf-query-loaders --update-cache

# config slick-greeter
echo "[Greeter]
background=/usr/share/backgrounds/the-mouse.jpg
background-color=#263138
draw-grid=false
theme-name=Arc-Dark
icon-theme-name=Sardi-Arc
font-name='Cantarell Bold 14'
xft-antialias=true
xft-hintstyle=hintfull
enable-hidpi=auto
draw-user-backgrounds=false
activate-numlock=true
show-power=false
show-a11y=false" | sudo tee /etc/lightdm/slick-greeter.conf
errorCheck "slick greeter config"

# config lightdm greeter
sed 's/^.*greeter-session=$/greeter-session=lightdm-slick-greeter/g' </etc/lightdm/lightdm.conf >lightdm.conf
sudo mv -f lightdm.conf /etc/lightdm
sed 's/^.*user-session=$/user-session=awesome/g' </etc/lightdm/lightdm.conf >lightdm.conf
errorCheck "lightdm greeter config"

if [ -f $HOME/.screenlayout/screenlayout.sh ]; then
    sudo cp $HOME/.screenlayout/screenlayout.sh /opt/screenlayout.sh
    sed 's/^.*display-setup-script=$/display-setup-script=\/opt\/screenlayout.sh/g' </etc/lightdm/lightdm.conf >lightdm.conf
	sudo mv lightdm.conf /etc/lightdm
	errorCheck "screenlayout config"
fi

# grub config

# powerline in linux console
pakku -S --needed --noconfirm terminus-font powerline-fonts 
if [ $IS_GARUDA = true ]; then
    pakku -S $PAKKU_ALL terminess-powerline-font-git terminus-font powerline-fonts 
else
    pakku -S $PAKKU_ALL terminus-font powerline-fonts 
fi

echo "KEYMAP=de
FONT=ter-powerline-v12n
FONT_MAP=" | sudo tee /etc/vconsole.conf

sudo cp -r /usr/share/grub/themes/Stylish/ /boot/grub/themes/
sed 's/.*GRUB_GFXMODE=.*$/GRUB_GFXMODE="1920x1080,auto"/g' </etc/default/grub >grub
sudo mv -f grub /etc/default
if [ $IS_GARUDA ]; then
	sed 's/.*GRUB_THEME=.*$/GRUB_THEME="\/usr\/share\/grub\/themes\/garuda\/theme.txt"/g' </etc/default/grub >grub
else
	sed 's/.*GRUB_THEME=.*$/GRUB_THEME="\/boot\/grub\/themes\/Stylish\/theme.txt"/g' </etc/default/grub >grub
fi
sudo mv -f grub /etc/default
errorCheck "grub config"

micro /etc/default/grub

sudo mkinitcpio -P
sudo grub-mkconfig -o /boot/grub/grub.cfg
errorCheck "grub mkconfig"

# login screen console
sudo cp $SCRIPTS/issue /etc

# Git config for Visual Studio Code
git config --global diff.tool code
git config --global difftool.code.cmd "$(which code) --wait --diff \"\$LOCAL\" \"\$BASE\" \"\$REMOTE\""
git config --global difftool.prompt false

git config --global merge.tool code
git config --global mergetool.code.cmd "$(which code) --wait \"\$MERGED\""
git config --global mergetool.prompt false

git config --global core.editor micro

git config --global user.name "stira"
git config --global user.email "ralf.stich@infoniqa.com"

sudo git config --system core.editor micro

git config --global credential.helper cache
git config --global credential.helper 'cache --timeout=25000'
git config --global push.default simple

git config pull.rebase false  # merge (the default strategy)
git config pull.ff only       # fast-forward only

git config --global credential.helper /usr/lib/git-core/git-credential-libsecret

# nodejs tools for editors
sudo npm install -g eslint jshint jsxhint stylelint sass-lint markdownlint-cli raml-cop typescript tern js-beautify iconv-lite
errorCheck "install required nodejs-tools"

# Default Browser setzen (vorher $BROWSER Variable entfernen)
export BROWSER=
xdg-settings set default-web-browser firefox.desktop

sudo fc-cache -fv
errorCheck "fontcache"

rm -f $PKG_FILE
rm -f $PKG_UNINST_FILE

sudo usermod -aG docker $USER

###########################
# enable services

# printer Service
sudo systemctl enable --now cups
errorCheck "printer service"

sudo systemctl enable --now docker
errorCheck "docker service"

sudo systemctl enable --now webmin
errorCheck "webmin service"

sudo systemctl enable --now bluetooth-autoconnect
errorCheck "bluetooth-autoconnect service"

mkdir -p ~/.config/systemd/user/
sudo cp /usr/lib/systemd/user/pulseaudio-bluetooth-autoconnect.service /etc/systemd/user
systemctl enable pulseaudio-bluetooth-autoconnect --user --now
errorCheck "pulseaudio-bluetooth-autoconnect service"

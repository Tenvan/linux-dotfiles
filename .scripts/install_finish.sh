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
background=/usr/share/slick-greeter/slick-greeter.png
background-color=#263138
draw-grid=false
theme-name=Arc-Dark
icon-theme-name=Papirus-Dark-Maia
font-name='Cantarell 11'
xft-antialias=true
xft-hintstyle=hintfull
enable-hidpi=auto" | sudo tee /etc/lightdm/slick-greeter.conf
errorCheck "slick greeter config"

# config lightdm greeter
sed 's/^.*greeter-session=$/greeter-session=lightdm-slick-greeter/g' </etc/lightdm/lightdm.conf >lightdm.conf
sudo mv -f lightdm.conf /etc/lightdm
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

# printer Service
sudo systemctl enable cups
sudo systemctl start cups
errorCheck "printer service"

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
xdg-settings set default-web-browser firefox.desktop

sudo fc-cache -fv
errorCheck "fontcache"

rm -f $PKG_FILE
rm -f $PKG_UNINST_FILE

sudo usermod -aG docker $USER
sudo systemctl enable docker
sudo systemctl start docker
errorCheck "docker service"

sudo systemctl enable webmin
sudo systemctl start webmin
errorCheck "webmin service"

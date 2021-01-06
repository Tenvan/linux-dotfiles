#!/usr/bin/env bash

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

# lightdm config
pakku -S $PAKKU_ALL lightdm \
	lightdm-settings \
	lightdm-slick-greeter \
	lightdm-webkit-greeter \
	lightdm-webkit2-clean \
	lightdm-webkit2-greeter \
errorCheck "install lightdm"

pakku -S $PAKKU_ALL lightdm-webkit2-theme-alter \
	lightdm-webkit2-theme-glorious \
	lightdm-webkit2-theme-material2 \
	lightdm-webkit2-theme-obsidian \
	lightdm-webkit2-theme-sapphire
errorCheck "install webkit2 themes"

pakku -S $PAKKU_ALL lightdm-webkit-theme-osmos
errorCheck "install webkit theme lightdm-webkit-theme-osmos"
pakku -S $PAKKU_ALL lightdm-webkit-theme-aether
errorCheck "install webkit theme lightdm-webkit-theme-aether"
pakku -S $PAKKU_ALL lightdm-webkit-theme-contemporary
errorCheck "install webkit theme lightdm-webkit-theme-contemporary"
pakku -S $PAKKU_ALL lightdm-webkit-theme-luminos
errorCheck "install webkit theme lightdm-webkit-theme-luminos"
pakku -S $PAKKU_ALL lightdm-webkit-theme-wisp
errorCheck "install webkit theme lightdm-webkit-theme-wisp"
	# lightdm-webkit-theme-archlinux \
errorCheck "install webkit themes"

pakku -S $PAKKU_ALL lightdm-webkit-theme-osmos
errorCheck "install webkit theme lightdm-webkit-theme-osmos"
pakku -S $PAKKU_ALL lightdm-webkit-theme-petrichor-git
errorCheck "install webkit theme lightdm-webkit-theme-petrichor-git"
pakku -S $PAKKU_ALL lightdm-webkit-theme-sequoia-git
errorCheck "install webkit theme lightdm-webkit-theme-sequoia-git"

errorCheck "install git webkit themes"

echo "[Greeter]
background=/usr/share/backgrounds/greeter_default.jpg
background-color=#263138
draw-grid=false
theme-name=Adapta-Nokto-Eta-Maia
icon-theme-name=Papirus-Dark-Maia
font-name='Cantarell 11'
xft-antialias=true
xft-hintstyle=hintfull
enable-hidpi=auto" | sudo tee /etc/lightdm/slick-greeter.conf

# sed 's/^.*greeter-session=$/greeter-session=lightdm-slick-greeter/g' </etc/lightdm/lightdm.conf >lightdm.conf
sed 's/^.*greeter-session=.*$/greeter-session=lightdm-webkit2-greeter/g' </etc/lightdm/lightdm.conf >lightdm.conf
sudo mv lightdm.conf /etc/lightdm

if [ -f $HOME/.screenlayout/screenlayout.sh ]; then
    sed 's/^.*display-setup-script=$/display-setup-script=\/opt\/screenlayout.sh/g' </etc/lightdm/lightdm.conf >lightdm.conf
    sudo cp $HOME/.screenlayout/screenlayout.sh /opt
fi
sudo mv lightdm.conf /etc/lightdm

# grub config
sudo cp -r /usr/share/grub/themes/Stylish/ /boot/grub/themes/
sed 's/.*GRUB_CMDLINE_LINUX_DEFAULT=.*$/GRUB_CMDLINE_LINUX_DEFAULT="loglevel=3"/g' </etc/default/grub >grub
sudo mv grub /etc/default
sed 's/.*GRUB_GFXMODE=.*$/GRUB_GFXMODE="1920x1080,auto"/g' </etc/default/grub >grub
sudo mv grub /etc/default
sed 's/.*GRUB_THEME=.*$/GRUB_THEME="\/boot\/grub\/themes\/Stylish\/theme.txt"/g' </etc/default/grub >grub
sudo mv grub /etc/default

sudo mkinitcpio -P
sudo grub-mkconfig -o /boot/grub/grub.cfg
errorCheck "grub config"

# printer Service
sudo systemctl enable cups
sudo systemctl start cups
errorCheck "printer service"

# Git config for meld
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
sudo npm install -g neovim eslint jshint jsxhint stylelint sass-lint markdownlint-cli raml-cop typescript tern js-beautify iconv-lite
errorCheck "install required nodejs-tools"

# Default Browser setzen (vorher $BROWSER Variable entfernen)
xdg-settings set default-web-browser firefox-developer-edition.desktop

sudo fc-cache -fv
errorCheck "fontcache"

rm -f $PKG_FILE
rm -f $PKG_UNINST_FILE

sudo usermod -aG docker $USER
sudo systemctl enable docker
sudo systemctl start docker
errorCheck "docker service"

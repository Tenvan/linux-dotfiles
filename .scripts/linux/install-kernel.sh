#!/usr/bin/env bash
. ./.configrc

pushd kernel/ || exit

sudo make modules_install
sudo make install

sudo dracut /boot/initramfs-$VERSION.img $VERSION --force -v
sudo grub2-mkconfig --output /boot/grub2/grub.cfg

popd || exit

#!/usr/bin/env bash
. ./.configrc

#
# Re installation of Linux Kerbel with defined version
#
pushd kernel/ || exit

sudo dracut /boot/initramfs-$VERSION.img $VERSION --force -v
sudo grub2-mkconfig --output /boot/grub2/grub.cfg

popd || exit

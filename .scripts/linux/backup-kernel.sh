#!/usr/bin/env bash
. ./.configrc

pushd /boot || exit

echo Backup Kernel Version: $VERSION

sudo cp vmlinuz-$VERSION vmlinuz-backup
sudo cp initramfs-$VERSION.img initramfs-backup.img

sudo grub2-mkconfig --output /boot/grub2/grub.cfg

popd || exit

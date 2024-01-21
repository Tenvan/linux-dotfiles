#!/usr/bin/env bash
. ./.configrc

echo Backup Kernel Version: $VERSION

sudo cp /boot/vmlinuz-$VERSION /boot/vmlinuz-backup
sudo cp /boot/initramfs-$VERSION.img /boot/initramfs-backup.img

sudo grub2-mkconfig --output /boot/grub2/grub.cfg

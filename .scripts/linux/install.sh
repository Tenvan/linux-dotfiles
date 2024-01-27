#!/usr/bin/env bash
. ./.configrc

echo Install version: $VERSION

#
# Install Linux Kerbel with defined version
# Source: https://kernelnewbies.org/KernelBuild
#
pushd kernel/ || exit

sudo cp .config /boot/config-$VERSION.x86_64
sudo chown root:root /boot/config-$VERSION.x86_64
#sudo cp System.map /boot/System.map-$VERSION.x86_64
#sudo chown root:root /boot/System.map-$VERSION.x86_64
#sudo cp vmlinux.symvers /boot/symvers-$VERSION.x86_64
#sudo chown root:root /boot/symvers-$VERSION.x86_64
#exit

sudo make modules_install
sudo make install

sudo dracut /boot/initramfs-$VERSION.img $VERSION --force -v
sudo grub2-mkconfig --output /boot/grub2/grub.cfg

popd || exit

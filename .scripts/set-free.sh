##remove old
paru -R nvidia nvidia-utils nvidia-vulkan
#sudo mhwd -r pci video-nvidia
sudo pacman -Syyuu

#install new
#sudo mhwd -i pci video-linux
paru -S xf86-video-nouveau
sudo pacman -Syu

##remove old
pakku -R nvidia nvidia-utils nvidia-vulkan
#sudo mhwd -r pci video-nvidia
sudo pacman -Syyuu

#install new
#sudo mhwd -i pci video-linux
pakku -S xf86-video-nouveau
sudo pacman -Syu

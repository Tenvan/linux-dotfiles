##remove old
yay -Rcnu xf86-video-nouveau
#sudo mhwd -r pci video-nvidia
sudo pacman -Syyuu

#install new
#sudo mhwd -i pci video-linux
yay -S nvidia-vulkan
sudo pacman -Syu

##remove old
yay -Rcnu nvidia-vulkan
#sudo mhwd -r pci video-nvidia
sudo pacman -Syyuu

#install new
#sudo mhwd -i pci video-linux
yay -S xf86-video-nouveau
sudo pacman -Syu

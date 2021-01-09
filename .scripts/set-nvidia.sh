##remove old
pakku -Rcnu xf86-video-nouveau
#sudo mhwd -r pci video-nvidia
sudo pacman -Syyuu

#install new
#sudo mhwd -i pci video-linux
pakku -S nvidia nvidia-utils
sudo pacman -Syu

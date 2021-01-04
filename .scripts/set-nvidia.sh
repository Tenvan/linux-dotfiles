#remove old
sudo mhwd -r pci video-nvidia
sudo mhwd -r pci video-linux
sudo pacman -Syyuu

#install new
sudo mhwd -i pci video-nvidia
sudo pacman -Syu

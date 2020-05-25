##################################################
# Alias Definitions

alias rc="kate ~/.config/fish/*.fish"

alias sys-update="yay -S --noconfirm "
alias sys-install="yay -S --noconfirm "
alias sys-remove="yay -Rnsu "
alias sys-search="yay -Ss "

alias killx="sudo systemctl restart lightdm; sudo systemctl restart sddm"

abbr --add --global sys-up yay -S --noconfirm 
abbr --add --global sys-in yay -Sy --noconfirm 
abbr --add --global sys-re yay -Rnsu 
abbr --add --global sys-se yay -Ss 

alias ll="exa -lh"
alias la="exa -lha"
alias dir="exa -lh -gGalg --classify"
alias lf="br -dgiph"

#
##################################################

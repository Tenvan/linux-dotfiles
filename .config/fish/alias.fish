##################################################
# Alias Definitions

alias rc="kate ~/.config/fish/*.fish ~/.env.fish"
# alias rc="vim -p -c 'set mouse=a' ~/.config/fish/*.fish"

alias sys-update="yay -S --noconfirm "
alias sys-install="yay -S --noconfirm "
alias sys-remove="yay -Rnsu "
alias sys-search="yay -Ss "

alias vim="vim -p -c 'set mouse=a' "
alias gvim="gvim -p "

alias killx="sudo systemctl restart lightdm; sudo systemctl restart sddm"

abbr --add --global sys-up yay -S --noconfirm 
abbr --add --global sys-in yay -Sy --noconfirm 
abbr --add --global sys-re yay -Rnsu 
abbr --add --global sys-se yay -Ss 

alias ll="ls -lh --color=always"
alias la="ll --almost-all"
alias dir="la -g --no-group --classify --human-readable"

#
##################################################

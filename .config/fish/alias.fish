##################################################
# Alias Definitions
if test -e ~/.aliasrc
  source ~/.aliasrc
end

#Cleanup orphaned packages
alias cleanup='sudo pacman -Rns (pacman -Qtdq)'

#get fastest mirrors in your neighborhood
# ArcoLinux
if $IS_ARCO == true
  alias mirror="sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"
  alias mirrord="sudo reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist"
  alias mirrors="sudo reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist"
  alias mirrora="sudo reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist"
end

# Manjaro
if $IS_MANJARO == true
  alias mirror="sudo pacman-mirrors -i"
  alias mirrord="sudo pacman-mirrors -c Germany"
  alias mirrors="sudo pacman-mirrors -a"
end

abbr --add --global sys-up yay -S --noconfirm 
abbr --add --global sys-in yay -Sy --noconfirm 
abbr --add --global sys-re yay -Rnsu 
abbr --add --global sys-se yay -Ss 

#
##################################################

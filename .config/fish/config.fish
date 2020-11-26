
### BASH POWERLINE ###
if test -f ~/Scripts/bash-powerline.sh
  source ~/Scripts/bash-powerline.sh
end

if test -f "/usr/bin/neofetch"
  neofetch
end


# Dump Farbpalette
echo -e "\033[0mNC (No color)"
echo -e "\033[1;37mWHITE\t\033[0;30mBLACK"
echo -e "\033[0;34mBLUE\t\033[1;34mLIGHT_BLUE"
echo -e "\033[0;32mGREEN\t\033[1;32mLIGHT_GREEN"
echo -e "\033[0;36mCYAN\t\033[1;36mLIGHT_CYAN"
echo -e "\033[0;31mRED\t\033[1;31mLIGHT_RED"
echo -e "\033[0;35mPURPLE\t\033[1;35mLIGHT_PURPLE"
echo -e "\033[0;33mYELLOW\t\033[1;33mLIGHT_YELLOW"
echo -e "\033[1;30mGRAY\t\033[0;37mLIGHT_GRAY"

##################################################
# Custom Environment Variables:
if test -e ~/.env.fish
  source ~/.env.fish
end
#
##################################################

##################################################
# Function Definitions
if test -e ~/.config/fish/function.fish
  source ~/.config/fish/function.fish
end
#
##################################################

##################################################
# Alias Definitions
if test -e ~/.config/fish/alias.fish
  source ~/.config/fish/alias.fish
end
#
##################################################

##################################################
# Call other Customizing Definitions:
if test -e ~/.config/fish/custom.fish
  source ~/.config/fish/custom.fish
end
#
##################################################

##################################################
# Call Binding Definitions:
if test -e ~/.config/fish/bind.fish
  source ~/.config/fish/bind.fish
end
#
##################################################

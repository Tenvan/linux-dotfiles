screenfetch
fish_logo
fish -v

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

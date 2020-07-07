echo start edit: $@ >>/dev/stderr

# emacs $@ &
# code --file-uri $@ &
geany $@ &
# notepadqq $@ &
# alacritty -e nvim -p $@ &
# emacs $@ &

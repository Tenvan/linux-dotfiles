echo start edit: $@ >>/dev/stderr

# emacs $@ &
code $@ &
# geany $@ &
# notepadqq $@ &
# alacritty -e nvim -p $@ &


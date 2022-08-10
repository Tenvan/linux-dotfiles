#!/usr/bin/env zsh

# shellcheck shell=zsh

# Compatible with ranger 1.4.2 through 1.9.*
#
# Automatically change the current working directory after closing ranger
#
# This is a shell function to automatically change the current working
# directory to the last visited one after ranger quits. Either put it into your
# .zshrc/.bashrc/etc or source this file from your shell configuration.
# To undo the effect of this function, you can type "cd -" to return to the
# original directory.
rr() {
	local temp_file=$(mktemp -t ranger_cd.XXX)

	command ranger --choosedir="$temp_file" -- "${@:-$PWD}"

    if chosen_dir="$(cat -- "$temp_file")" && [ -n "$chosen_dir" ] && [ "$chosen_dir" != "$PWD" ]; then
        cd -- "$chosen_dir"
    fi
	rm -f -- "$temp_file"
}

# Compatible with ranger 1.5.3 through 1.9.*
#
# Change the prompt when you open a shell from inside ranger
#
# Source this file from your shell startup file (.bashrc, .zshrc etc) for it to
# work.
if [ -n "$RANGER_LEVEL" ]; then
    echo "(in ranger)"
fi

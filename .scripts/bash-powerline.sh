prompt() {
if test -f "/usr/bin/powerline-rs"; then
	PS1="$(powerline-rs --shell bash $?)"
else
	PS1="\d- \t:[\u@\H]\n\w :>\[$(tput sgr0)\]"
fi
}

# PROMPT_COMMAND=prompt

eval "$(starship init bash)"
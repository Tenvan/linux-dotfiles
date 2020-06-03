prompt() {
    PS1="$(powerline-rs --shell bash $?)"
}
PROMPT_COMMAND=prompt

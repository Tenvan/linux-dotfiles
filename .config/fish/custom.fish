##################################################
# Custom Definitions

set -g theme_nerd_fonts yes
set -g theme_powerline_fonts yes

set -g theme_display_git yes
set -g theme_display_git_untracked yes
set -g theme_display_git_ahead_verbose yes
set -g theme_git_worktree_support yes

set -g theme_display_vagrant yes
# set -g theme_display_docker_machine yes
set -g theme_display_hg yes
set -g theme_display_virtualenv yes

# set -g theme_display_ruby no
# set -g theme_display_user yes
# set -g theme_display_vi no
# set -g theme_display_date no
# set -g theme_display_cmd_duration yes
set -g theme_title_display_process yes
set -g theme_title_display_path yes
set -g theme_title_use_abbreviated_path no
# set -g theme_date_format "+%a %H:%M"
set -g theme_avoid_ambiguous_glyphs yes
set -g theme_show_exit_status yes
set -g default_user your_normal_user
set -g theme_color_scheme zenburn
# set -g fish_prompt_pwd_dir_length 0
set -g theme_project_dir_length 0

#
# Theme
#
set -g fish_color_normal normal

set -g fish_color_command 99cc99
set -g fish_color_quote ffcc66
set -g fish_color_redirection d3d0c8
set -g fish_color_end cc99cc
set -g fish_color_error f2777a
set -g fish_color_param d3d0c8
set -g fish_color_selection white --bold --background=brblack
set -g fish_color_search_match bryellow --background=brblack
set -g fish_color_history_current --bold
set -g fish_color_operator 6699cc
set -g fish_color_escape 66cccc
set -g fish_color_cwd green
set -g fish_color_cwd_root red
set -g fish_color_valid_path --underline
set -g fish_color_autosuggestion 747369
set -g fish_color_user brgreen
set -g fish_color_host normal
set -g fish_color_cancel -r
set -g fish_pager_color_completion normal
set -g fish_pager_color_description B3A06D yellow
set -g fish_pager_color_prefix white --bold --underline
set -g fish_pager_color_progress brwhite --background=cyan

#
##################################################


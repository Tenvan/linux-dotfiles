function fish_bindings ()
  #####################
  # Alt-R Refresh Shell
  bind \er fish_reload_config 'next_line'
  #
  #####################

  #####################
  #
  bind \e0 change_to_work next_line
  bind \e1 start_task_1 next_line
  bind \e2 start_task_2 next_line
  bind \e3 start_task_3 next_line
  bind \e4 start_task_4 next_line
  bind \e5 start_task_5 next_line
  
  bind \eh 'cd ~' next_line
  
  bind \ey 'echo Alle Pakete installieren' 'yarn install --ignore-scripts' next_line
  bind \eo 'echo Alle Paketeaktualisierungen anzeigen...' 'yarn outdated' next_line
  bind \eu 'echo Alle Pakete updaten' 'yarn run update:all' next_line
  #
  #####################

  #####################
  bind  \ek kill_node next_line
  #
  #####################
end

function fish_user_key_bindings
  fish_bindings
end

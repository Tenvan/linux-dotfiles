function fish_bindings ()
  #####################
  # Alt-R Refresh Shell
  bind \er fish_reload_config 'next_line'
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

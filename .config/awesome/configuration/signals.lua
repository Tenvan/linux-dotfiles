log("Enter Module => " .. ... )

local awesome, client, screen = awesome, client, screen

local beautiful = require('beautiful')
local config = require('configuration.config')

local notify = require('utilities.notify')
local sound = require('utilities.sound')

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal('property::geometry', function(s)
  notify('Window', "'property::geometry' event raised: ")
  -- Wallpaper
  -- awful.spawn.with_shell('sh ~/.scripts/set-wallpaper.sh')
end)

-- No borders when rearranging only 1 non-floating or maximized client
screen.connect_signal('arrange', function(s)
  log("'screen:arange' event raised")
  local only_one = #s.tiled_clients == 1
  for _, c in pairs(s.clients) do
    if (c.class == 'firefox') or (c.class == 'firefoxdeveloperedition') or (c.class == 'Chromium') or
      (c.class == 'Google-chrome') or (c.class == 'Microsoft Teams.*') or (c.class == 'teams-for-linux') then
    elseif c.maximized or (only_one and not c.floating) then
      c.border_width = 0
    else
      c.border_width = beautiful.border_width
    end
  end
end)

awesome.connect_signal('debug::deprecation', function(hint, see, args)
  notify('Deprecated Function called!', tostring(hint), 'critical')
end)

awesome.connect_signal('spawn::initiated', function(arg)
  if config.debug_mode then
    notify('Awesome', "'spawn::initiated' event raised: " .. arg.id, 'info')
    log("'awesome:spawn::initiated' event raised: " .. arg.id)
  end
end)

awesome.connect_signal('spawn::changed', function(arg)
  if config.debug_mode then
    notify('Awesome', "'spawn::changed' event raised: " .. arg.id, 'info')
    log("'awesome:spawn::changed' event raised: " .. arg.id)
  end
end)

awesome.connect_signal('spawn::timeout', function(arg)
  if config.debug_mode then
    notify('Awesome', "'spawn::timeout' event raised: " .. arg.id, 'info')
    log("'awesome:spawn::timeout' event raised: " .. arg.id)
  end
end)

awesome.connect_signal('spawn::completed', function(arg)
  if config.debug_mode then
    notify('Awesome', "'spawn::completed' event raised: " .. arg.id, 'info')
    log("'awesome:spawn::completed' event raised: " .. arg.id)
  end
end)

awesome.connect_signal('startup', function()
  log("'awesome:startup' event raised: ")
  sound('desktop-login')
end)

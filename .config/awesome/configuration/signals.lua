log('Enter Module => ' .. ...)

local awful = require('awful')
local beautiful = require('beautiful')

local notify = require('utilities.notify')
local sound = require('utilities.sound')

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal('property::geometry', function(s)
  notify('Window', "'property::geometry' event raised: ")
  -- Wallpaper
  -- awful.spawn.with_shell('sh ~/.scripts/set-wallpaper.sh')
end)

local ChromiumMatch = {
  class = { 'firefox*', 'Chromium', 'Google-chrome', 'Microsoft Teams.*', 'teams-for-linux', 'Code' }
}

-- No borders when rearranging only 1 non-floating or maximized client
screen.connect_signal('arrange', function(s)
  local only_one = #s.tiled_clients == 1
  -- log('spawned::arrange')
  -- log(' --> only_one: ' .. tostring(only_one))

  for _, c in pairs(s.clients) do
    local match = awful.rules.match_any(c, ChromiumMatch)
    log('no Chromium/Electron')
    if only_one and not c.floating or c.maximized or c.fullscreen then
      c.border_width = 0
    else
      c.border_width = beautiful.border_width
    end
  end
  -- end
end)

client.connect_signal('property::border_width', function(c)
  log(string.format('property::border_width for %s with width=%d', tostring(c), c.border_width))
end)

awesome.connect_signal('debug::deprecation', function(hint, see, args)
  notify('Deprecated Function called!', tostring(hint), 'critical')
end)

awesome.connect_signal('startup', function()
  log("'awesome:startup' event raised")
  sound('desktop-login')
end)

-- Handle runtime errors after startup
do
  local in_error = false
  awesome.connect_signal('debug::error', function(err)
    -- Make sure we don't go into an endless error loop
    if in_error then return end
    in_error = true

    notify('Oops, an error happened!', err, 'critical')
    log('Errors ' .. err)

    in_error = false
  end)
end
-- }}}

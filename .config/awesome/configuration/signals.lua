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
  log('spawned::arrange')
  log(' --> only_one: ' .. tostring(only_one))

  for _, c in pairs(s.clients) do
    local match = awful.rules.match_any(c, ChromiumMatch)
    log(' --> client.class     : ' .. tostring(c.class))
    log(' --> client.instance  : ' .. tostring(c.instance))
    log(' --> client.maximized : ' .. tostring(c.maximized))
    log(' --> client.floating  : ' .. tostring(c.floating))
    log(' --> client.fullscreen: ' .. tostring(c.fullscreen))
    log(' --> rule match       : ' .. tostring(match))

    if match then
      log('Chromium/Electron detected (suppess arrange)')
    else
      log('no Chromium/Electron')
      if only_one and not c.floating or c.maximized or c.fullscreen then
        log('set border: 0')
        c.border_width = 0
      else
        log('set theme border: ' .. tostring(beautiful.border_width))
        c.border_width = beautiful.border_width
      end
    end
  end
end)

awesome.connect_signal('debug::deprecation', function(hint, see, args)
  notify('Deprecated Function called!', tostring(hint), 'critical')
end)

awesome.connect_signal('startup', function()
  log("'awesome:startup' event raised")
  sound('desktop-login')
end)

client.connect_signal('property::border_width', function(c)
  print(debug.traceback(string.format('property::border_width for %s with width=%d', tostring(c), c.border_width)))
end)

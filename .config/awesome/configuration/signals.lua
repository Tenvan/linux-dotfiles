local log = require('utilities.debug').log
local dump = require('utilities.debug').dump
log("Enter Module => configuration/signals.lua" )

local awesome, client, screen = awesome, client, screen

local awful = require('awful') -- Everything related to window managment
local beautiful = require('beautiful')
local naughty = require('naughty')

local notify = require("utilities.notify")
local sound = require("utilities.sound")

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal('property::geometry', function(s)
    notify('Window', "'property::geometry' event raised: ")
    -- Wallpaper
    awful.spawn.with_shell('sh ~/.scripts/set-wallpaper.sh')
end)

-- No borders when rearranging only 1 non-floating or maximized client
screen.connect_signal('arrange', function(s)
    notify("Window", "'arange' event raised")
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

awesome.connect_signal('debug::error', function(err)
    if in_error then
        return
    end
    in_error = true

    notify('Oops, an error happened!', tostring(err), naughty.config.presets.critical, true)
    in_error = false
end)

awesome.connect_signal('debug::deprecation', function(hint, see, args)
    notify('Deprecated Function called!', tostring(hint), naughty.config.presets.critical, true)
end)

awesome.connect_signal('spawn::initiated', function(arg)
    notify("Awesome", "'spawn::initiated' event raised: ".. arg.id, naughty.config.presets.info)
    -- dump(arg, "spawn::initiated".. arg.id)
end)

awesome.connect_signal('spawn::changed', function(arg)
    notify("Awesome", "'spawn::changed' event raised: ".. arg.id, naughty.config.presets.info)
    -- dump(arg, "spawn::changed" .. arg.id)
end)

awesome.connect_signal('spawn::timeout', function(arg)
    notify("Awesome", "'spawn::timeout' event raised: ".. arg.id, naughty.config.presets.info)
    -- dump(arg, "spawn::timeout".. arg.id)
end)

awesome.connect_signal('spawn::completed', function(arg)
    notify("Awesome", "'spawn::completed' event raised: ".. arg.id, naughty.config.presets.info)
    -- dump(arg, "spawn::completed".. arg.id)
end)

awesome.connect_signal('startup', function(hint, see, args)
    sound('desktop-login')
    notify('Awesome', "'Autostart' callback raised")
end)

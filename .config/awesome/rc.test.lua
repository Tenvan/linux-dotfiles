--[[
░▀█▀░█▀▀░█▀█░█░█░█▀█░█▀█░█▀▀░░░█▀█░█░█░█▀▀░█▀▀░█▀█░█▄█░█▀▀
░░█░░█▀▀░█░█░▀▄▀░█▀█░█░█░▀▀█░░░█▀█░█▄█░█▀▀░▀▀█░█░█░█░█░█▀▀
░░▀░░▀▀▀░▀░▀░░▀░░▀░▀░▀░▀░▀▀▀░░░▀░▀░▀░▀░▀▀▀░▀▀▀░▀▀▀░▀░▀░▀▀▀
--]]

local log = require('utilities.debug').log
local dump = require('utilities.debug').dump
log("Enter Module => rc.test.lua" )

local awesome, client, screen = awesome, client, screen
local string, os, tostring = string, os, tostring

local is_initialized = false

pcall(require, "luarocks.loader")
local awful = require('awful') -- Everything related to window managment
local wibox = require('wibox')
local beautiful = require('beautiful')
local naughty = require('naughty')
require('awful.autofocus')

local dpi = require('beautiful.xresources').apply_dpi

-- ░▀█▀░█░█░█▀▀░█▄█░█▀▀
-- ░░█░░█▀█░█▀▀░█░█░█▀▀
-- ░░▀░░▀░▀░▀▀▀░▀░▀░▀▀▀
beautiful.init(require('theme'))

-- ░█▀▀░█▀█░█▀█░█▀▀░▀█▀░█▀▀░█░█░█▀▄░█▀█░▀█▀░▀█▀░█▀█░█▀█░█▀▀
-- ░█░░░█░█░█░█░█▀▀░░█░░█░█░█░█░█▀▄░█▀█░░█░░░█░░█░█░█░█░▀▀█
-- ░▀▀▀░▀▀▀░▀░▀░▀░░░▀▀▀░▀▀▀░▀▀▀░▀░▀░▀░▀░░▀░░▀▀▀░▀▀▀░▀░▀░▀▀▀

require("configuration")

-- ░█▄█░█▀█░█▀▄░█░█░█░░░█▀▀░█▀▀
-- ░█░█░█░█░█░█░█░█░█░░░█▀▀░▀▀█
-- ░▀░▀░▀▀▀░▀▀░░▀▀▀░▀▀▀░▀▀▀░▀▀▀

require("module")

-- ░█▀█░█▀█░▀█▀░▀█▀░█▀▀░▀█▀░█▀▀░█▀█░▀█▀░▀█▀░█▀█░█▀█░░░█░░░▀█▀░█▀▄░█▀▄░█▀█░█▀▄░█░█
-- ░█░█░█░█░░█░░░█░░█▀▀░░█░░█░░░█▀█░░█░░░█░░█░█░█░█░░░█░░░░█░░█▀▄░█▀▄░█▀█░█▀▄░░█░
-- ░▀░▀░▀▀▀░░▀░░▀▀▀░▀░░░▀▀▀░▀▀▀░▀░▀░░▀░░▀▀▀░▀▀▀░▀░▀░░░▀▀▀░▀▀▀░▀▀░░▀░▀░▀░▀░▀░▀░░▀░
local notify = require('utilities.notify')

local sound_path = string.format('%s/.scripts/play-sound.zsh', os.getenv('HOME'))
local sound = require("utilities.sound");

local logout_popup = require('awesome-wm-widgets.logout-popup-widget.logout-popup')

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
local hotkeys_popup = require('awful.hotkeys_popup').widget

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    notify('Oops, there were errors during startup!', awesome.startup_errors, naughty.config.presets.critical, true)
end

-- {{{ Variable definitions

-- personal variables
-- change these variables if you want
local editorgui = 'Geany'
local terminal = 'kitty'

-- awesome variables
awful.util.terminal = terminal

-- {{{ Screen
-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal('property::geometry', function(s)
    notify('Window', "'property::geometry' event raised: ")
    -- Wallpaper
    awful.spawn.with_shell('sh ~/.scripts/set-wallpaper.sh')
end)

-- No borders when rearranging only 1 non-floating or maximized client
screen.connect_signal('arrange', function(s)
    -- notify("Window", "'arange' event raised")
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

-- Create a wibox for each screen and add it
awful.screen.connect_for_each_screen(function(s)
    beautiful.at_screen_connect(s)
end)
-- }}}

-- {{{ Client Signals
-- Signal function to execute when a new client appears.
client.connect_signal('manage', function(c)
    if is_initialized then
        -- notify("Client", "'manage' event raised:" .. c.name)
        -- sound("window-switch")
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- if not awesome.startup then awful.client.setslave(c) end

        if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
            -- Prevent clients from being unreachable after screen count changes.
            awful.placement.no_offscreen(c)
        end
    end
end)

client.connect_signal('swapped', function(c, source, is_source)
    if is_initialized and not is_source then
        sound('window-switch')
    end
end)

client.connect_signal('raised', function(c)
    if is_initialized then
        -- sound("window-attention-active")
    end
end)

client.connect_signal('lowered', function(c)
    if is_initialized then
        -- sound("window-attention-inactive")
    end
end)

client.connect_signal('request::activate', function(c)
    if is_initialized then
        -- notify("Client", "'request::activate' event raised: " .. c.name)
    end
end)

client.connect_signal('request::geometry', function(c, context, Additional)
    if is_initialized and context ~= 'mouse.move' and context ~= 'mouse.resize' then
        if context == 'maximized' then
            notify('Client', 'Maximized: ' .. c.name)
        else
            -- notify("Client", "'request::geometry' event raised: " .. c.name .. " Context: " .. context)
        end
    end
end)

client.connect_signal('property::window', function(c)
    if is_initialized then
        -- notify("Client", "'property::window' event raised: " .. c.name)
    end
end)

client.connect_signal('property::size', function(c)
    if is_initialized then
        -- notify("Client", "'property::size' event raised: " .. c.name)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal('request::titlebars', function(c)
    -- notify("Client", "'request::titlebars' event raised: " .. c.name)
    -- Custom
    -- if beautiful.titlebar_fun then
    --     beautiful.titlebar_fun(c)
    --     return
    -- end

    -- Default
    -- buttons for the titlebar
    local buttons = my_table.join(
        awful.button({}, 1, function()
            c:emit_signal('request::activate', 'titlebar', {
                raise = true
            })
            awful.mouse.client.move(c)
        end),
        awful.button({}, 3, function()
            c:emit_signal('request::activate', 'titlebar', {
                raise = true
            })
            awful.mouse.client.resize(c)
        end))

    awful.titlebar(c, {
        size = dpi(21),
        height = 20,
        bg_normal = beautiful.bg_normal,
        bg_focus = beautiful.bg_focus,
        fg_normal = beautiful.fg_normal,
        fg_focus = beautiful.fg_focus
    }):setup {
        {
            -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout = wibox.layout.fixed.horizontal
        },
        {
            -- Middle
            {
                -- Title
                align = 'center',
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout = wibox.layout.flex.horizontal
        },
        {
            -- Right
            awful.titlebar.widget.floatingbutton(c),
            awful.titlebar.widget.minimizebutton(c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton(c),
            awful.titlebar.widget.ontopbutton(c),
            awful.titlebar.widget.closebutton(c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- -- Enable sloppy focus, so that focus follows mouse.
-- client.connect_signal(
--     "mouse::enter",
--     function(c)
--         c:emit_signal("request::activate", "mouse_enter", {raise = false})
--     end
-- )

client.connect_signal('focus', function(c)
    if is_initialized then
        -- notify("Window", "'focus' event raised: " .. c.name)
        c.border_color = beautiful.border_focus
        -- c.border_color = '#ff000050'
    end
end)

client.connect_signal('unfocus', function(c)
    if is_initialized then
        -- notify("Window", "'unfocus' event raised: " .. c.name)
        c.border_color = beautiful.border_normal
    end
end)

-- }}}

-- {{{ Awesome Signals
-- Handle runtime errors after startup
local in_error = false
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
    -- notify("Awesome", "'spawn::initiated' event raised: ".. arg.id, naughty.config.presets.info)
    -- dump(arg, "spawn::initiated".. arg.id)
end)

awesome.connect_signal('spawn::changed', function(arg)
    -- notify("Awesome", "'spawn::changed' event raised: ".. arg.id, naughty.config.presets.info)
    -- dump(arg, "spawn::changed" .. arg.id)
end)

awesome.connect_signal('spawn::timeout', function(arg)
    -- notify("Awesome", "'spawn::timeout' event raised: ".. arg.id, naughty.config.presets.info)
    -- dump(arg, "spawn::timeout".. arg.id)
end)

awesome.connect_signal('spawn::completed', function(arg)
    -- notify("Awesome", "'spawn::completed' event raised: ".. arg.id, naughty.config.presets.info)
    -- dump(arg, "spawn::completed".. arg.id)
end)

awesome.connect_signal('startup', function(hint, see, args)
    sound('desktop-login')

    notify('Awesome', "'Autostart' callback raised")
    is_initialized = true
    -- notify("Awesome Default", "Awesome Default erfolgreich gestartet !!", naughty.config.presets.critical, false)
end)
-- }}}

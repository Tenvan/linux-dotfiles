--[[
░▀█▀░█▀▀░█▀█░█░█░█▀█░█▀█░█▀▀░░░█▀█░█░█░█▀▀░█▀▀░█▀█░█▄█░█▀▀
░░█░░█▀▀░█░█░▀▄▀░█▀█░█░█░▀▀█░░░█▀█░█▄█░█▀▀░▀▀█░█░█░█░█░█▀▀
░░▀░░▀▀▀░▀░▀░░▀░░▀░▀░▀░▀░▀▀▀░░░▀░▀░▀░▀░▀▀▀░▀▀▀░▀▀▀░▀░▀░▀▀▀
--]] 

local awesome, client, screen = awesome, client, screen
local ipairs, string, os, tostring = ipairs, string, os, tostring

local is_initialized = false

pcall(require, "luarocks.loader")
local gears = require('gears') -- Utilities such as color parsing and objects
local awful = require('awful') -- Everything related to window managment
local wibox = require('wibox')
local beautiful = require('beautiful')
local lain = require('lain')
local freedesktop = require('freedesktop')
require('awful.autofocus')

local dpi = require('beautiful.xresources').apply_dpi

local gdebug = require('gears.debug')


-- ░▀█▀░█░█░█▀▀░█▄█░█▀▀
-- ░░█░░█▀█░█▀▀░█░█░█▀▀
-- ░░▀░░▀░▀░▀▀▀░▀░▀░▀▀▀
local theme_dir = gears.filesystem.get_configuration_dir() .. "theme/theme.lua"
beautiful.init(theme_dir)

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

local my_table = awful.util.table or gears.table -- 4.{0,1} compatibility
-- }}}

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    notify('Oops, there were errors during startup!', awesome.startup_errors, naughty.config.presets.critical, true)
end

-- {{{ Autostart windowless processes
local function run_once(cmd_arr)
    for _, cmd in ipairs(cmd_arr) do
        awful.spawn.with_shell(string.format("pgrep -u $USER -fx '%s' > /dev/null || (%s)", cmd, cmd))
    end
end

-- }}}

-- This function implements the XDG autostart specification
awful.spawn.with_shell('if (xrdb -query | grep -q "^awesome\\.started:\\s*true$"); then exit; fi;' ..
    'xrdb -merge <<< "awesome.started:true";' ..
    'dex --autostart --search-paths "$HOME/.config/autostart";')
-- }}}

-- {{{ Variable definitions

-- personal variables
-- change these variables if you want
local editorgui = 'Geany'
local terminal = 'kitty'

-- awesome variables
awful.util.terminal = terminal
-- local tagnames = {"󾠮", "󾠯", "󾠰", "󾠱", "󾠲", "󾠳", "󾠴", "󾠵", "󾠶"}
-- local tagnames = {"1", "2", "3", "4", "5", "6", "7", "8", "9"}
-- local tagnames = { "", "", "", "", "", "", "", "", "" }
-- local tagnames = {  "", "", "", "", "", "", "", "", "", "" }
-- local tagnames = { "⠐", "⠡", "⠲", "⠵", "⠻", "⠿" }
-- local tagnames = { "www", "edit", "gimp", "inkscape", "music" }
-- local tagnames = { "➊", "➋", "➌", "➍", "➎", "➏", "➐", "➑", "➒" }
local tagnames = { '①', '②', '③', '④', '⑤', '⑥', '⑦', '⑧', '⑨' }
awful.util.tagnames = tagnames

local defaultLayouts = { awful.layout.suit.tile, awful.layout.suit.floating, awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom, awful.layout.suit.tile.top, awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal, -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max, awful.layout.suit.max.fullscreen, awful.layout.suit.magnifier -- awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se
    -- ~ lain.layout.cascade,
    -- ~ lain.layout.cascade.tile,
    -- ~ lain.layout.centerwork,
    -- ~ lain.layout.centerwork.horizontal,
    -- ~ lain.layout.termfair,
    -- ~ lain.layout.termfair.center,
}

local tags = { {
    layout = awful.layout.suit.max,
    layouts = { awful.layout.suit.tile, awful.layout.suit.max, awful.layout.suit.max.fullscreen,
        awful.layout.suit.magnifier },
    master_fill_policy = 'expand',
    gap_single_client = false,
    gap = 5,
    selected = true
}, {
    layout = awful.layout.suit.tile,
    layouts = defaultLayouts,
    master_fill_policy = 'expand',
    gap_single_client = true,
    gap = 2
}, {
    layout = awful.layout.suit.tile,
    layouts = defaultLayouts,
    master_fill_policy = 'expand',
    gap_single_client = false,
    gap = 5
}, {
    layout = awful.layout.suit.tile,
    layouts = defaultLayouts,
    master_fill_policy = 'expand',
    gap_single_client = false,
    gap = 5
}, {
    layout = awful.layout.suit.tile,
    layouts = defaultLayouts,
    master_fill_policy = 'expand',
    gap_single_client = false,
    gap = 5
}, {
    layout = awful.layout.suit.tile,
    layouts = defaultLayouts,
    master_fill_policy = 'expand',
    gap_single_client = false,
    gap = 5
}, {
    layout = awful.layout.suit.tile,
    layouts = defaultLayouts,
    master_fill_policy = 'expand',
    gap_single_client = false,
    gap = 5
}, {
    layout = awful.layout.suit.tile,
    layouts = defaultLayouts,
    master_fill_policy = 'expand',
    gap_single_client = false,
    gap = 5
}, {
    layout = awful.layout.suit.tile,
    layouts = defaultLayouts,
    master_fill_policy = 'expand',
    gap_single_client = false,
    gap = 5
} }

for s in screen do
    for i = 1, 9 do
        local props = tags[i]
        props.screen = s
        awful.tag.add(tagnames[i], props)
    end
end

awful.layout.suit.tile.left.mirror = true

awful.layout.append_default_layouts = defaultLayouts
awful.layout.layouts = defaultLayouts

local modkey = "Mod4"

awful.util.taglist_buttons = my_table.join(awful.button({}, 1, function(t)
    t:view_only()
end),
    awful.button({ modkey }, 1, function(t)
        if client.focus then
            client.focus:move_to_tag(t)
        end
    end),
    awful.button({}, 3, awful.tag.viewtoggle), awful.button({ modkey }, 3, function(t)
        if client.focus then
            client.focus:toggle_tag(t)
        end
    end),
    awful.button({}, 4, function(t)
        awful.tag.viewnext(t.screen)
    end),
    awful.button({}, 5, function(t)
        awful.tag.viewprev(t.screen)
    end))

awful.util.tasklist_buttons = my_table.join(awful.button({}, 1, function(c)
    if c == client.focus then
        c.minimized = true
    else
        -- c:emit_signal("request::activate", "tasklist", {raise = true})<Paste>

        -- Without this, the following
        -- :isvisible() makes no sense
        c.minimized = false
        if not c:isvisible() and c.first_tag then
            c.first_tag:view_only()
        end
        -- This will also un-minimize
        -- the client, if needed
        client.focus = c
        c:raise()
    end
end), awful.button({}, 3, function()
    local instance = nil

    return function()
        if instance and instance.wibox.visible then
            instance:hide()
            instance = nil
        else
            instance = awful.menu.clients({
                theme = {
                    width = dpi(250)
                }
            })
        end
    end
end), awful.button({}, 4, function()
    awful.client.focus.byidx(1)
end), awful.button({}, 5, function()
    awful.client.focus.byidx(-1)
end))

lain.layout.termfair.nmaster = 3
lain.layout.termfair.ncol = 1
lain.layout.termfair.center.nmaster = 3
lain.layout.termfair.center.ncol = 1
lain.layout.cascade.tile.offset_x = dpi(2)
lain.layout.cascade.tile.offset_y = dpi(32)
lain.layout.cascade.tile.extra_padding = dpi(5)
lain.layout.cascade.tile.nmaster = 5
lain.layout.cascade.tile.ncol = 2

-- beautiful.init(string.format('%s/.config/awesome/themes/mytheme/theme.lua', os.getenv('HOME')))
-- }}}

-- {{{ Menu
local myawesomemenu = { { 'hotkeys', function()
    return false, hotkeys_popup.show_help
end }, { 'arandr', 'arandr' } }

awful.util.mymainmenu = freedesktop.menu.build({
    before = { { 'Awesome', myawesomemenu, beautiful.awesome_icon } -- { "Atom", "atom" },
        -- other triads can be put here
    },
    after = { { 'Terminal', terminal }, { 'Log out', function()
        awesome.quit()
    end }, { 'Sleep', 'systemctl suspend' }, { 'Restart', 'systemctl reboot' }, { 'Shutdown', 'systemctl poweroff' } -- other triads can be put here
    }
})

-- hide menu when mouse leaves it
awful.util.mymainmenu.wibox:connect_signal('mouse::leave', function()
    awful.util.mymainmenu:hide()
    sound('menu-popdown')
end)

-- menubar.utils.terminal = terminal -- Set the Menubar terminal for applications that require it
-- }}}

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

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = { -- All clients will match this rule.
    {
        rule = {},
        properties = {
            border_width = beautiful.border_width,
            border_color = beautiful.border_normal,
            focus = awful.client.focus.filter,
            raise = true,
            keys = clientkeys,
            buttons = clientbuttons,
            screen = awful.screen.preferred,
            placement = awful.placement.no_overlap + awful.placement.no_offscreen,
            size_hints_honor = false,
            -- float first
            maximized = false,
            floating = true
        }
    }, -- Toolbars
    {
        rule_any = {
            class = { 'Polybar', 'Plank' }
        },
        properties = {
            border_width = 0
        }
    }, -- Titlebars
    {
        rule_any = {
            type = { 'dialog', 'normal' }
        },
        properties = {
            titlebars_enabled = true
        }
    }, -- Set applications to be maximized at startup.
    -- find class or role via xprop command
    {
        rule_any = {
            class = {}
        },
        properties = {
            border_width = 0,
            titlebars_enabled = false,
            maximized = false,
            floating = false
        }
    }, {
        rule = {
            class = 'Geany'
        },
        properties = {
            maximized = false
        }
    }, {
        rule = {
            class = 'Geany',
            type = 'dialog'
        },
        properties = {
            maximized = false
        }
    }, -- Maximized clients.
    {
        rule = {
            class = 'Gimp*',
            role = 'gimp-image-window'
        },
        properties = {
            maximized = true,
            floating = false
        }
    }, {
        rule_any = {
            instance = {},
            class = { 'inkscape', 'Mailspring', 'VirtualBox Machine', 'Vlc' },
            role = {}
        },
        properties = {
            maximized = true,
            floating = false
        }
    }, -- Non-Floating clients.
    {
        rule_any = {
            instance = {},
            class = { 'Alacritty', 'kitty', 'Thunar', 'Nemo' },
            name = {},
            role = {}
        },
        properties = {
            maximized = false,
            floating = false
        }
    }, -- Floating clients but centered in screen
    {
        rule_any = {
            class = { '.*gnome-authentication-agent.*', 'Gcr-prompter', 'Rofi' }
        },
        properties = {
            floating = true,
            opacity = 0.8
        },
        callback = function(c)
            awful.placement.centered(c, nil)
        end
    }, -- Floating clients (overide non-floating rules bevor this)
    {
        rule_any = {
            instance = {},
            class = {},
            name = { 'Wetter:.*', 'XBindKey: Hit a key' },
            role = { 'pop-up', 'Preferences', 'setup' }
        },
        properties = {
            floating = true
        }
    }, -- Special applications mappings
    {
        rule_any = {
            class = { 'Pavucontrol' }
        },
        properties = {
            screen = 2,
            tag = awful.util.tagnames[3],
            ontop = true,
            switchtotag = true,
            maximized = false,
            floating = true
        }
    }, -- Audio Clients
    {
        rule_any = {
            class = { 'Spotify', 'Psst-gui', 'Shortwave' }
        },
        properties = {
            screen = 2,
            tag = awful.util.tagnames[5],
            switchtotag = true,
            maximized = false,
            floating = false
        }
    }, -- Steam
    {
        rule_any = {
            class = { 'RuneScape' }
        },
        properties = {
            border_width = 5,
            titlebars_enabled = false,
            maximized = true,
            floating = false,
            screen = 1,
            tag = awful.util.tagnames[7],
            switchtotag = true
        }
    }, {
        rule_any = {
            class = { 'Steam' }
        },
        properties = {
            border_width = 5,
            titlebars_enabled = false,
            maximized = false,
            floating = false,
            screen = 1,
            tag = awful.util.tagnames[7],
            switchtotag = true
        }
    }, {
        rule = {
            class = 'Code'
        },
        properties = {
            screen = 1,
            tag = awful.util.tagnames[9],
            switchtotag = true,
            maximized = false,
            floating = false
        }
    }, {
        rule_any = {
            name = { 'AWMTT' }
        },
        properties = {
            screen = 2,
            tag = awful.util.tagnames[6],
            switchtotag = true,
            maximized = false,
            floating = false
        }
    }, -- Set applications to always map on the tag 5 (Virtual/RDP) on screen 1.
    {
        rule_any = {
            class = { 'Virt-manager' }
        },
        properties = {
            screen = 1,
            tag = awful.util.tagnames[5],
            switchtotag = false
        }
    }, -- Set applications to always map on the tag 6 (RemoteDesktops) on screen 1.
    {
        rule_any = {
            class = { 'org.remmina.Remmina' }
        },
        properties = {
            screen = 1,
            tag = awful.util.tagnames[6],
            switchtotag = true
        }
    }, {
        rule_any = {
            class = { 'RemoteDesktopManager.*' }
        },
        properties = {
            screen = 1,
            tag = awful.util.tagnames[6],
            -- maximized = false,
            -- floating = false,
            switchtotag = true
        }
    }, -- Set applications to always map on the tag 9 (Systemtools) on screen 1.
    {
        rule_any = {
            class = { editorgui }
        },
        properties = {
            screen = 1,
            tag = awful.util.tagnames[9],
            switchtotag = true
        }
    }, -- Special handled applications
    {
        -- FreeCAD
        rule = {
            class = 'FreeCAD'
        },
        properties = {
            maximized = false,
            floating = false,
            screen = 1,
            tag = awful.util.tagnames[7],
            switchtotag = true
        }
    }, {
        -- Fusion 360
        rule = {
            class = 'fusion360.exe'
        },
        properties = {
            border_width = 0,
            titlebars_enabled = false,
            screen = 1,
            tag = awful.util.tagnames[7],
            switchtotag = true
        }
    }, {
        -- JetBrains InteliJ Hauptfenster
        rule = {
            class = 'jetbrains-.*',
            type = 'dialog'
        },
        properties = {
            maximized = false,
            float = true,
            opacity = 0.8
        }
    }, {
        -- Teams Hauptfenster
        rule = {
            class = 'teams-for-linux',
            type = 'normal'
        },
        properties = {
            maximized = false,
            floating = false,
            screen = 2,
            tag = awful.util.tagnames[4],
            switchtotag = false
        }
    }, {
        -- Teams Messagebox
        rule = {
            class = 'teams-for-linux',
            name = '.*Benachrichtigung.*'
            -- ~ type = "notification",
            -- name = "Microsoft Teams-Benachrichtigung"
        },
        properties = {
            maximized = false,
            floating = true,
            screen = 1,
            tag = awful.util.tagnames[1],
            switchtotag = true -- zur Nachricht springen
        }
    }, -- Alle Develop Consolen auf Screen 2 tag 2 schieben
    {
        rule_any = {
            name = { 'OT.:*' }
        },
        properties = {
            screen = 2,
            tag = awful.util.tagnames[2],
            switchtotag = true,
            maximized = false,
            floating = false
        }
    }, -- OTW Develop Consolen auf Screen 2 tag 3 schieben
    {
        rule_any = {
            name = { 'OTW:*' }
        },
        properties = {
            screen = 2,
            tag = awful.util.tagnames[3],
            switchtotag = true,
            maximized = false,
            floating = false
        }
    }, {
        rule_any = {
            class = { 'URxtv', 'XTerm' }
            -- instance = {
            --     "urxtv",
            --     "xterm"
            -- }
        },
        properties = {
            screen = 2,
            tag = awful.util.tagnames[2],
            switchtotag = true,
            maximized = false,
            floating = false
        }
    }, -- Chromium Debugger Instanz auf Screen 2 tag 2 schieben
    {
        rule = {
            class = 'Chromium',
            instance = 'chromium *'
        },
        properties = {
            floating = false,
            tag = awful.util.tagnames[1],
            switchtotag = false
        }
    }, -- Firefox Develop Edition auf Screen 2 tag 2 schieben
    {
        rule_any = {
            class = { 'Google-chrome', 'Vivaldi*', 'firefox', 'firefoxdeveloperedition' }
        },
        properties = {
            maximized = false,
            floating = false,
            tag = awful.util.tagnames[1],
            switchtotag = false
        }
    }, -- System Monitor Consolen auf Screen 2 tag 9 schieben
    {
        rule_any = {
            name = { 'SysMon:*', 'Sys:*', 'CF:*' },
            class = { 'Gnome-system-monitor' }
        },
        properties = {
            screen = 2,
            tag = awful.util.tagnames[9],
            floating = false,
            switchtotag = true
        }
    }, -- Finishings
    -- Dialogs
    {
        rule_any = {
            type = { 'dialog' }
        },
        properties = {
            maximized = false,
            floating = true
        }
    } }
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
    local buttons = my_table.join(awful.button({}, 1, function()
        c:emit_signal('request::activate', 'titlebar', {
            raise = true
        })
        awful.mouse.client.move(c)
    end), awful.button({}, 3, function()
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
    -- gdebug.dump(arg, "spawn::initiated".. arg.id, 2)
end)

awesome.connect_signal('spawn::changed', function(arg)
    -- notify("Awesome", "'spawn::changed' event raised: ".. arg.id, naughty.config.presets.info)
    -- gdebug.dump(arg, "spawn::changed" .. arg.id, 2)
end)

awesome.connect_signal('spawn::timeout', function(arg)
    -- notify("Awesome", "'spawn::timeout' event raised: ".. arg.id, naughty.config.presets.info)
    -- gdebug.dump(arg, "spawn::timeout".. arg.id, 2)
end)

awesome.connect_signal('spawn::completed', function(arg)
    -- notify("Awesome", "'spawn::completed' event raised: ".. arg.id, naughty.config.presets.info)
    -- gdebug.dump(arg, "spawn::completed".. arg.id, 2)
end)

awesome.connect_signal('startup', function(hint, see, args)
    sound('desktop-login')

    notify('Awesome', "'Autostart' callback raised")
    is_initialized = true
    -- notify("Awesome Default", "Awesome Default erfolgreich gestartet !!", naughty.config.presets.critical, false)
end)
-- }}}

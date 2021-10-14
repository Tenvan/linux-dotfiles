--[[

     Awesome WM configuration template
     https://github.com/awesomeWM

     Freedesktop : https://github.com/lcpz/awesome-freedesktop

     Copycats themes : https://github.com/lcpz/awesome-copycats

     lain : https://github.com/lcpz/lain

--]] -- {{{ Required libraries
local awesome, client, screen = awesome, client, screen
local ipairs, string, os, tostring = ipairs, string, os, tostring

local is_initialized = false

-- https://awesomewm.org/doc/api/documentation/05-awesomerc.md.html
-- Standard awesome library
local gears = require("gears") -- Utilities such as color parsing and objects
local awful = require("awful") -- Everything related to window managment
local dpi = require("beautiful.xresources").apply_dpi

local gdebug = require("gears.debug")

require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")

-- Theme handling library
local beautiful = require("beautiful")

local theme_path = string.format("%s/.config/awesome/themes/mytheme/theme.lua", os.getenv("HOME"))
beautiful.init(theme_path)

-- Notification library
local naughty = require("naughty")
naughty.config.defaults["icon_size"] = 100
naughty.config.defaults["border_width"] = beautiful.border_width
naughty.config.defaults["border_width"] = beautiful.border_width
naughty.config.defaults["position"] = "bottom_right"

local sound_path = string.format("%s/.scripts/play-sound.zsh", os.getenv("HOME"))

local function notify(titel, message, category, playSound)
    naughty.notify(
        {
            presets = category,
            text = message,
            title = titel
        }
    )

    if playSound == true then
        if category == naughty.config.presets.critical then
            awful.spawn(sound_path .. " " .. "notify-error")
        else
            awful.spawn(sound_path .. " " .. "notify")
        end
    end
end

local function sound(soundFile)
    notify("Play Sound", soundFile, naughty.config.presets.info)
    awful.spawn(sound_path .. " " .. soundFile)
end

-- local menubar       = require("menubar")

local lain = require("lain")
local freedesktop = require("freedesktop")

local logout_popup = require("awesome-wm-widgets.logout-popup-widget.logout-popup")

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
local hotkeys_popup = require("awful.hotkeys_popup").widget
-- require("awful.hotkeys_popup.keys")

local my_table = awful.util.table or gears.table -- 4.{0,1} compatibility
local dpi = require("beautiful.xresources").apply_dpi
-- }}}

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    notify("Oops, there were errors during startup!", awesome.startup_errors, naughty.config.presets.critical, true)
end

-- {{{ Autostart windowless processes
local function run_once(cmd_arr)
    for _, cmd in ipairs(cmd_arr) do
        awful.spawn.with_shell(string.format("pgrep -u $USER -fx '%s' > /dev/null || (%s)", cmd, cmd))
    end
end

-- }}}

-- This function implements the XDG autostart specification
--[[
awful.spawn.with_shell(
    'if (xrdb -query | grep -q "^awesome\\.started:\\s*true$"); then exit; fi;' ..
    'xrdb -merge <<< "awesome.started:true";' ..
    -- list each of your autostart commands, followed by ; inside single quotes, followed by ..
    'dex --environment Awesome --autostart --search-paths "$XDG_CONFIG_DIRS/autostart:$XDG_CONFIG_HOME/autostart"' -- https://github.com/jceb/dex
)
--]]
-- }}}

-- {{{ Variable definitions

-- modkey or mod4 = super key
local modkey = "Mod4"
local altkey = "Mod1"
local controlkey = "Control"
local shiftkey = "Shift"
local returnkey = "Return"
local escapekey = "Escape"

-- personal variables
-- change these variables if you want
local editorgui = "Geany"
local terminal = "kitty"

-- key groups
local kgAwesome = "AwesomeWM"
local kgApps = "Anwendungen"
local kgMenus = "Menüs"
local kgClient = "Client Aktionen"
local kgLayout = "Layout Aktionen"
local kgMaster = "Master Aktionen"
local kgScreen = "Screen Aktionen"
local kgScreenshot = "Screenshot"
local kgSound = "Audio"
local kgSystem = "System"
local kgTag = "Tags"

-- awesome variables
awful.util.terminal = terminal
-- awful.util.tagnames = {"󾠮", "󾠯", "󾠰", "󾠱", "󾠲", "󾠳", "󾠴", "󾠵", "󾠶"}
-- awful.util.tagnames = {"1", "2", "3", "4", "5", "6", "7", "8", "9"}
-- awful.util.tagnames = { "", "", "", "", "", "", "", "", "" }
-- awful.util.tagnames = {  "", "", "", "", "", "", "", "", "", "" }
-- awful.util.tagnames = { "⠐", "⠡", "⠲", "⠵", "⠻", "⠿" }
-- awful.util.tagnames = { "www", "edit", "gimp", "inkscape", "music" }
-- awful.util.tagnames = { "⓵", "⓶", "⓷", "⓸", "⓹", "⓺", "⓻", "⓼", "⓽"}
awful.util.tagnames = {"①", "②", "③", "④", "⑤", "⑥", "⑦", "⑧", "⑨"}
-- awful.util.tagnames = { "➊", "➋", "➌", "➍", "➎", "➏", "➐", "➑", "➒" }

awful.layout.suit.tile.left.mirror = true

awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.floating,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
    awful.layout.suit.spiral,
    awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier
    -- awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se
    --~ lain.layout.cascade,
    --~ lain.layout.cascade.tile,
    --~ lain.layout.centerwork,
    --~ lain.layout.centerwork.horizontal,
    --~ lain.layout.termfair,
    --~ lain.layout.termfair.center,
}

awful.util.taglist_buttons =
    my_table.join(
    awful.button(
        {},
        1,
        function(t)
            t:view_only()
        end
    ),
    awful.button(
        {modkey},
        1,
        function(t)
            if client.focus then
                client.focus:move_to_tag(t)
            end
        end
    ),
    awful.button({}, 3, awful.tag.viewtoggle),
    awful.button(
        {modkey},
        3,
        function(t)
            if client.focus then
                client.focus:toggle_tag(t)
            end
        end
    ),
    awful.button(
        {},
        4,
        function(t)
            awful.tag.viewnext(t.screen)
        end
    ),
    awful.button(
        {},
        5,
        function(t)
            awful.tag.viewprev(t.screen)
        end
    )
)

awful.util.tasklist_buttons =
    my_table.join(
    awful.button(
        {},
        1,
        function(c)
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
        end
    ),
    awful.button(
        {},
        3,
        function()
            local instance = nil

            return function()
                if instance and instance.wibox.visible then
                    instance:hide()
                    instance = nil
                else
                    instance = awful.menu.clients({theme = {width = dpi(250)}})
                end
            end
        end
    ),
    awful.button(
        {},
        4,
        function()
            awful.client.focus.byidx(1)
        end
    ),
    awful.button(
        {},
        5,
        function()
            awful.client.focus.byidx(-1)
        end
    )
)

lain.layout.termfair.nmaster = 3
lain.layout.termfair.ncol = 1
lain.layout.termfair.center.nmaster = 3
lain.layout.termfair.center.ncol = 1
lain.layout.cascade.tile.offset_x = dpi(2)
lain.layout.cascade.tile.offset_y = dpi(32)
lain.layout.cascade.tile.extra_padding = dpi(5)
lain.layout.cascade.tile.nmaster = 5
lain.layout.cascade.tile.ncol = 2

beautiful.init(string.format("%s/.config/awesome/themes/mytheme/theme.lua", os.getenv("HOME")))
-- }}}

-- {{{ Menu
local myawesomemenu = {
    {
        "hotkeys",
        function()
            return false, hotkeys_popup.show_help
        end
    },
    {"arandr", "arandr"}
}

awful.util.mymainmenu =
    freedesktop.menu.build(
    {
        before = {
            {"Awesome", myawesomemenu, beautiful.awesome_icon}
            -- { "Atom", "atom" },
            -- other triads can be put here
        },
        after = {
            {"Terminal", terminal},
            {
                "Log out",
                function()
                    awesome.quit()
                end
            },
            {"Sleep", "systemctl suspend"},
            {"Restart", "systemctl reboot"},
            {"Shutdown", "systemctl poweroff"}
            -- other triads can be put here
        }
    }
)

-- hide menu when mouse leaves it
awful.util.mymainmenu.wibox:connect_signal(
    "mouse::leave",
    function()
        awful.util.mymainmenu:hide()
        sound("menu-popdown")
    end
)

-- menubar.utils.terminal = terminal -- Set the Menubar terminal for applications that require it
-- }}}

-- {{{ Screen
-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal(
    "property::geometry",
    function(s)
        -- notify("Window", "'property::geometry' event raised: " .. s.name)
        -- Wallpaper
        awful.spawn.with_shell("sh ~/.scripts/set-wallpaper.sh")
    end
)

-- No borders when rearranging only 1 non-floating or maximized client
screen.connect_signal(
    "arrange",
    function(s)
        -- notify("Window", "'arange' event raised: " .. s.name)
        local only_one = #s.tiled_clients == 1
        for _, c in pairs(s.clients) do
            if
                (c.class == "firefox") or (c.class == "firefoxdeveloperedition") or (c.class == "Chromium") or
                    (c.class == "Google-chrome") or
                    (c.class == "Microsoft Teams.*")
             then
            elseif c.maximized or (only_one and not c.floating) then
                -- c.border_width = 0
            else
                -- c.border_width = beautiful.border_width
            end
        end
    end
)

-- Create a wibox for each screen and add it
awful.screen.connect_for_each_screen(
    function(s)
        gdebug.print_warning("Screen " .. s.index)
        gdebug.dump(s, "Screen", 3)

        beautiful.at_screen_connect(s)
    end
)
-- }}}

-- {{{ Mouse bindings
root.buttons(
    my_table.join(
        awful.button(
            {},
            3,
            function()
                awful.util.mymainmenu:toggle()
                sound("menu-popup")
            end
        ),
        awful.button({}, 4, awful.tag.viewnext),
        awful.button({}, 5, awful.tag.viewprev)
    )
)
-- }}}

-- {{{ Key bindings
local globalkeys =
    my_table.join(
    awful.key(
        {modkey, shiftkey},
        "r",
        function()
            awesome.restart()
        end,
        {description = "reload awesome", group = kgAwesome}
    ),
    -- alt + ...
    -- Hotkeys Awesome
    awful.key({modkey}, "s", hotkeys_popup.show_help, {description = "show help", group = kgAwesome}),
    -- Tag browsing with modkey
    -- awful.key({modkey}, "Left", awful.tag.viewprev, {description = "view previous", group = kgTag}),
    -- awful.key({modkey}, "Right", awful.tag.viewnext, {description = "view next", group = kgTag}),
    awful.key({altkey}, escapekey, awful.tag.history.restore, {description = "go back", group = kgTag}),
    -- Tag browsing alt + tab
    -- Tag browsing modkey + tab
    awful.key({modkey}, "Tab", awful.tag.viewnext, {description = "view next", group = kgTag}),
    awful.key({modkey, shiftkey}, "Tab", awful.tag.viewprev, {description = "view previous", group = kgTag}),
    -- Non-empty tag browsing
    -- awful.key({ modkey }, "Left", function () lain.util.tag_view_nonempty(-1) end,
    -- {description = "view  previous nonempty", group = kgTag}),
    -- awful.key({ modkey }, "Right", function () lain.util.tag_view_nonempty(1) end,
    -- {description = "view  previous nonempty", group = kgTag}),
    -- Default client focus
    -- Window Sizing
    awful.key(
        {modkey, shiftkey},
        "Next",
        function()
            awful.client.moveresize(20, 20, -40, -40)
        end,
        {description = "increase window size", group = kgClient}
    ),
    awful.key(
        {modkey, shiftkey},
        "Prior",
        function()
            awful.client.moveresize(-20, -20, 40, 40)
        end,
        {description = "decrease window size", group = kgClient}
    ),
    awful.key(
        {modkey, shiftkey},
        "Down",
        function()
            awful.client.incwfact(0.05)
        end,
        {description = "increase window size", group = kgClient}
    ),
    awful.key(
        {modkey, shiftkey},
        "Up",
        function()
            awful.client.incwfact(-0.05)
        end,
        {description = "decrease window size", group = kgClient}
    ),
    awful.key(
        {modkey, shiftkey},
        "Left",
        function()
            awful.tag.incmwfact(-0.05)
        end,
        {description = "increase master size", group = kgClient}
    ),
    awful.key(
        {modkey, shiftkey},
        "Right",
        function()
            awful.tag.incmwfact(0.05)
        end,
        {description = "decrease master size", group = kgClient}
    ),
    awful.key(
        {modkey},
        "Right",
        function()
            awful.client.focus.byidx(1)
        end,
        {description = "focus next by index", group = kgClient}
    ),
    awful.key(
        {modkey},
        "Left",
        function()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = kgClient}
    ),
    -- By direction client focus with arrows
    awful.key(
        {controlkey, modkey},
        "Down",
        function()
            awful.client.focus.global_bydirection("down")
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "focus down", group = kgClient}
    ),
    awful.key(
        {controlkey, modkey},
        "Up",
        function()
            awful.client.focus.global_bydirection("up")
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "focus up", group = kgClient}
    ),
    awful.key(
        {controlkey, modkey},
        "Left",
        function()
            awful.client.focus.global_bydirection("left")
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "focus left", group = kgClient}
    ),
    awful.key(
        {controlkey, modkey},
        "Right",
        function()
            awful.client.focus.global_bydirection("right")
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "focus right", group = kgClient}
    ),
    -- Layout manipulation
    awful.key(
        {modkey},
        "Down",
        function()
            awful.client.swap.byidx(1)
        end,
        {
            description = "swap with next client by index",
            group = kgClient
        }
    ),
    awful.key(
        {modkey},
        "Up",
        function()
            awful.client.swap.byidx(-1)
        end,
        {
            description = "swap with previous client by index",
            group = kgClient
        }
    ),
    awful.key(
        {modkey},
        ".",
        function()
            awful.screen.focus_relative(1)
        end,
        {description = "focus the next screen", group = kgScreen}
    ),
    awful.key(
        {modkey},
        ",",
        function()
            awful.screen.focus_relative(-1)
        end,
        {description = "focus the previous screen", group = kgScreen}
    ),
    awful.key({modkey}, "u", awful.client.urgent.jumpto, {description = "jump to urgent client", group = kgClient}),
    -- Show/Hide Wibox
    awful.key(
        {modkey},
        "b",
        function()
            for s in screen do
                s.mywibox.visible = not s.mywibox.visible
                if s.mybottomwibox then
                    s.mybottomwibox.visible = not s.mybottomwibox.visible
                end
            end
        end,
        {description = "toggle wibox", group = kgLayout}
    ),
    -- On the fly useless gaps change
    awful.key(
        {modkey, altkey},
        "Up",
        function()
            lain.util.useless_gaps_resize(1)
        end,
        {description = "increment useless gaps", group = kgLayout}
    ),
    awful.key(
        {modkey, altkey},
        "Down",
        function()
            lain.util.useless_gaps_resize(-1)
        end,
        {description = "decrement useless gaps", group = kgLayout}
    ),
    -- On the fly master handling
    awful.key(
        {modkey, controlkey},
        "m",
        function()
            awful.tag.incnmaster(1, nil, true)
        end,
        {
            description = "increase the number of master clients",
            group = kgMaster
        }
    ),
    awful.key(
        {modkey, shiftkey},
        "m",
        function()
            awful.tag.incnmaster(-1, nil, true)
        end,
        {
            description = "decrease the number of master clients",
            group = kgMaster
        }
    ),
    awful.key(
        {modkey, controlkey},
        "h",
        function()
            awful.tag.incncol(1, nil, true)
        end,
        {
            description = "increase the number of columns",
            group = kgLayout
        }
    ),
    awful.key(
        {modkey, controlkey},
        "l",
        function()
            awful.tag.incncol(-1, nil, true)
        end,
        {
            description = "decrease the number of columns",
            group = kgLayout
        }
    ),
    -- On the fly layout handling
    awful.key(
        {modkey, shiftkey},
        "space",
        function()
            awful.layout.inc(1)
        end,
        {description = "select next layout", group = kgLayout}
    ),
    awful.key(
        {modkey, controlkey},
        "space",
        function()
            awful.layout.inc(-1)
        end,
        {description = "select previous layout", group = kgLayout}
    ),
    awful.key(
        {modkey, controlkey},
        "n",
        function()
            local c = awful.client.restore()
            -- Focus restored client
            if c then
                client.focus = c
                c:raise()
            end
        end,
        {description = "restore minimized", group = kgClient}
    ),
    -- Dropdown application
    awful.key(
        {modkey, controlkey},
        returnkey,
        function()
            awful.screen.focused().quake:toggle()
        end,
        {description = "dropdown application", group = kgAwesome}
    ),
    -- TODO Widgets einbauen
    -- Widgets popups
    -- awful.key(
    --     {modkey},
    --     "c",
    --     function()
    --         if lain.widget.calendar then
    --             lain.widget.calendar.show(7)
    --         end
    --     end,
    --     {description = "show calendar", group = "widgets"}
    -- ),
    -- awful.key(
    --     {modkey},
    --     "h",
    --     function()
    --         if beautiful.fs then
    --             beautiful.fs.show(7)
    --         end
    --     end,
    --     {description = "show filesystem", group = "widgets"}
    -- ),
    -- awful.key(
    --     {modkey},
    --     "w",
    --     function()
    --         if beautiful.weather then
    --             beautiful.weather.show(7)
    --         end
    --     end,
    --     {description = "show weather", group = "widgets"}
    -- ),

    -- Brightness
    awful.key(
        {},
        "XF86MonBrightnessUp",
        function()
            os.execute("xbacklight -inc 10")
        end,
        {description = "+10% Helligkeit", group = kgSound}
    ),
    awful.key(
        {},
        "XF86MonBrightnessDown",
        function()
            os.execute("xbacklight -dec 10")
        end,
        {description = "-10% Helligkeit", group = kgSound}
    ),
    -- ALSA device control
    awful.key(
        {modkey, controlkey},
        "KP_Add",
        function()
            awful.spawn.with_shell("~/.bin/audio-next")
        end,
        {description = "Nächste Soundkarte", group = kgSound}
    ),
    awful.key(
        {modkey, controlkey},
        "KP_Subtract",
        function()
            awful.spawn.with_shell("~/.bin/audio-prev")
        end,
        {description = "Vorherige Soundkarte", group = kgSound}
    ),
    -- ALSA volume control
    awful.key(
        {},
        "XF86AudioRaiseVolume",
        function()
            os.execute("amixer -d set Master 5%+")
        end,
        {description = "+5% Volume", group = kgSound}
    ),
    awful.key(
        {modkey, altkey},
        "KP_Add",
        function()
            os.execute("amixer -d set Master 5%+")
        end,
        {description = "+5% Volume", group = kgSound}
    ),
    awful.key(
        {},
        "XF86AudioLowerVolume",
        function()
            os.execute("amixer -d set Master 5%-")
        end,
        {description = "-5% Volume", group = kgSound}
    ),
    awful.key(
        {modkey, altkey},
        "KP_Subtract",
        function()
            os.execute("amixer -d set Master 5%-")
        end,
        {description = "-5% Volume", group = kgSound}
    ),
    awful.key(
        {},
        "XF86AudioMute",
        function()
            os.execute("amixer -q set Master toggle")
        end,
        {description = "Mute Volume", group = kgSound}
    ),
    awful.key(
        {modkey, altkey},
        "KP_Multiply",
        function()
            os.execute("amixer -q set Master toggle")
        end,
        {description = "Mute Volume", group = kgSound}
    ),
    awful.key(
        {},
        "XF86AudioPlay",
        function()
            notify("playerctl play-pause")
            os.execute("playerctl play-pause")
        end,
        {description = "Player Start/Pause", group = kgSound}
    ),
    awful.key(
        {},
        "XF86AudioNext",
        function()
            notify("playerctl next")
            os.execute("playerctl next")
        end,
        {description = "Player Next", group = kgSound}
    ),
    awful.key(
        {},
        "XF86AudioPrev",
        function()
            notify("playerctl previous")
            os.execute("playerctl previous")
        end,
        {description = "Player Zurück", group = kgSound}
    ),
    awful.key(
        {},
        "XF86AudioStop",
        function()
            notify("playerctl stop")
            os.execute("playerctl stop")
        end
    ),
    -- other media keys
    awful.key(
        {},
        "XF86Calculator",
        function()
            awful.spawn("gnome-calculator")
        end
    ),
    -- Menu Shortcuts
    awful.key(
        {modkey},
        "F2",
        function()
            awful.spawn("xfce4-appfinder")
        end,
        {description = "Anwendungs Finder", group = kgMenus}
    ),
    awful.key(
        {modkey},
        "z",
        function()
            awful.spawn.with_shell("sh ~/.scripts/menu/rofi.sh -show combi")
        end,
        {description = "Rofi Menü", group = kgMenus}
    ),
    awful.key(
        {modkey},
        "a",
        function()
            awful.spawn.with_shell("sh ~/.scripts/menu/app-menu.sh")
        end,
        {description = "Applikations Menü", group = kgMenus}
    ),
    awful.key(
        {modkey},
        "d",
        function()
            awful.spawn.with_shell("sh ~/.scripts/menu/develop-menu.sh")
        end,
        {description = "Developer Menü", group = kgMenus}
    ),
    awful.key(
        {modkey},
        "e",
        function()
            awful.spawn.with_shell("sh ~/.scripts/menu/edit-configs.sh")
        end,
        {description = "System Edit Menü", group = kgMenus}
    ),
    awful.key(
        {modkey},
        "t",
        function()
            awful.spawn.with_shell("sh ~/.scripts/menu/system-tools.sh")
        end,
        {description = "System Tools Menü", group = kgMenus}
    ),
    awful.key(
        {modkey},
        "m",
        function()
            awful.spawn.with_shell("sh ~/.scripts/menu/system-monitor.sh")
        end,
        {description = "System Monitors Menü", group = kgMenus}
    ),
    awful.key(
        {modkey},
        "x",
        function()
            awful.spawn.with_shell("sh ~/.scripts/menu/system-menu.sh")
        end,
        {description = "System Power Menü", group = kgMenus}
    ),
    -- Printer Shortcuts
    awful.key(
        {},
        "Print",
        function()
            awful.spawn("spectacle -i")
        end,
        {description = "Screenshot App", group = kgScreenshot}
    ),
    awful.key(
        {altkey},
        "Print",
        function()
            awful.spawn("spectacle -i -r")
        end,
        {description = "Screenshot Rect", group = kgScreenshot}
    ),
    awful.key(
        {controlkey},
        "Print",
        function()
            awful.spawn("spectacle -i -a")
        end,
        {description = "Screenshot Fenster", group = kgScreenshot}
    ),
    -- Shortcuts to Applications
    awful.key(
        {modkey},
        "F1",
        function()
            awful.spawn.with_shell("$(xdg-settings get default-web-browser | cut -f1 -d '.')")
        end,
        {description = "Standard Browser", group = kgApps}
    ),
    awful.key(
        {modkey},
        "F8",
        function()
            awful.spawn("nemo")
        end,
        {description = "Dateimanager", group = kgApps}
    ),
    -- System Tools
    awful.key(
        {controlkey, altkey},
        "k",
        function()
            awful.spawn(
                "killall node -s KILL; fuser -k 4200/tcp; fuser -k 4201/tcp; fuser -k 4202/tcp; notify-send.sh 'Node' 'all nodes-processes and ng-services killed'"
            )
            notify("Kill Node.js", "Alle laufenden Node.js Tasks wurden beendet!")
        end,
        {description = "Nodejs killen", group = kgSystem}
    ),
    awful.key(
        {modkey, altkey},
        "t",
        function()
            notify(
                "Test Nachricht",
                "Dies ist eine Test Nachicht.\nAmet dolor amet elitr sea justo eirmod ipsum sit.\nSit sed eos dolore vero vero ea, ea magna at et."
            )
        end,
        {description = "Test Benachrichtigung", group = kgSystem}
    ),
    awful.key(
        {modkey},
        "Return",
        function()
            awful.spawn("kitty")
        end,
        {description = "Terminal starten", group = kgSystem}
    ),
    awful.key(
        {modkey, controlkey},
        "x",
        function()
            awful.spawn.with_shell("kitty --hold --title CF:XProp --name CF:XProp xprop")
        end,
        {description = "Xprop", group = kgSystem}
    ),
    awful.key(
        {modkey, controlkey},
        "t",
        function()
            awful.spawn.with_shell("sh ~/.scripts/picom-toggle-awesome.sh")
        end,
        {description = "Picom Toggle", group = kgSystem}
    ),
    awful.key(
        {modkey},
        "Escape",
        function()
            awful.spawn("xkill")
        end,
        {description = "XKill", group = kgSystem}
    )
)

local clientkeys =
    my_table.join(
    awful.key({altkey, shiftkey}, "m", lain.util.magnify_client, {description = "magnify client", group = kgClient}),
    awful.key(
        {modkey},
        "f",
        function(c)
            c.maximized = not c.maximized
            c:raise()
        end,
        {
            description = "toggle maximized",
            group = kgClient
        }
    ),
    awful.key(
        {modkey},
        "q",
        function(c)
            c:kill()
        end,
        {
            description = "close",
            group = kgClient
        }
    ),
    awful.key(
        {modkey, altkey},
        "space",
        awful.client.floating.toggle,
        {
            description = "toggle floating",
            group = kgClient
        }
    ),
    awful.key(
        {modkey, altkey},
        "m",
        function(c)
            c:swap(awful.client.getmaster())
        end,
        {description = "move to master", group = kgMaster}
    ),
    awful.key(
        {modkey, shiftkey},
        "t",
        function(c)
            c.ontop = not c.ontop
        end,
        {description = "toggle keep on top", group = kgClient}
    ),
    awful.key(
        {modkey, shiftkey},
        "i",
        function(c)
            notify(
                "Oops, dies ist eine Test-Benachrichtung!",
                "Dolor et est dolor sed labore dolores, lorem sea kasd sed accusam.\nNonumy ipsum elitr aliquyam eirmod.\nNo sit lorem.",
                naughty.config.presets.critical,
                true
            )
        end,
        {description = "toggle keep on top", group = kgClient}
    )
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    -- Hack to only show tags 1 and 9 in the shortcut window (mod+s)
    local descr_view, descr_toggle, descr_move, descr_toggle_focus
    if i == 1 or i == 9 then
        descr_view = {description = "view tag #", group = kgTag}
        descr_toggle = {description = "toggle tag #", group = kgTag}
        descr_move = {description = "move focused client to tag #", group = kgTag}
        descr_toggle_focus = {
            description = "toggle focused client on tag #",
            group = kgTag
        }
    end
    globalkeys =
        my_table.join(
        globalkeys,
        -- View tag only.
        awful.key(
            {modkey},
            "#" .. i + 9,
            function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    tag:view_only()
                end
            end,
            descr_view
        ),
        -- Toggle tag display.
        awful.key(
            {modkey, "Control"},
            "#" .. i + 9,
            function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    awful.tag.viewtoggle(tag)
                end
            end,
            descr_toggle
        ),
        -- Move client to tag.
        awful.key(
            {modkey, "Shift"},
            "#" .. i + 9,
            function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:move_to_tag(tag)
                        tag:view_only()
                    end
                end
            end,
            descr_move
        ),
        -- Toggle tag on focused client.
        awful.key(
            {modkey, "Control", "Shift"},
            "#" .. i + 9,
            function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:toggle_tag(tag)
                    end
                end
            end,
            descr_toggle_focus
        )
    )
end

local clientbuttons =
    gears.table.join(
    awful.button(
        {},
        1,
        function(c)
            c:emit_signal("request::activate", "mouse_click", {raise = true})
        end
    ),
    awful.button(
        {modkey},
        1,
        function(c)
            c:emit_signal("request::activate", "mouse_click", {raise = true})
            awful.mouse.client.move(c)
        end
    ),
    awful.button(
        {modkey},
        3,
        function(c)
            c:emit_signal("request::activate", "mouse_click", {raise = true})
            awful.mouse.client.resize(c)
        end
    )
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
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
    },
    -- Toolbars
    {
        rule_any = {
            class = {"Polybar", "Plank"}
        },
        properties = {
            border_width = 0
        }
    },
    -- Titlebars
    {
        rule_any = {
            type = {"dialog", "normal"}
        },
        properties = {
            titlebars_enabled = true
        }
    },
    -- Set applications to be maximized at startup.
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
    },
    {
        rule = {
            class = "Geany"
        },
        properties = {
            maximized = false
        }
    },
    {
        rule = {
            class = "Geany",
            type = "dialog"
        },
        properties = {
            maximized = false
        }
    },
    -- Maximized clients.
    {
        rule = {
            class = "Gimp*",
            role = "gimp-image-window"
        },
        properties = {
            maximized = true,
            floating = false
        }
    },
    {
        rule_any = {
            instance = {},
            class = {
                "inkscape",
                "Mailspring",
                "VirtualBox Machine",
                "Vlc"
            },
            role = {}
        },
        properties = {
            maximized = true,
            floating = false
        }
    },
    -- Non-Floating clients.
    {
        rule_any = {
            instance = {},
            class = {
                "Alacritty",
                "kitty",
                "Thunar",
                "Nemo"
            },
            name = {},
            role = {}
        },
        properties = {
            maximized = false,
            floating = false
        }
    },
    -- Floating clients but centered in screen
    {
        rule_any = {
            class = {
                ".*gnome-authentication-agent.*",
                "Gcr-prompter",
                "Rofi"
            }
        },
        properties = {
            floating = true,
            opacity = 0.8
        },
        callback = function(c)
            awful.placement.centered(c, nil)
        end
    },
    -- Floating clients (overide non-floating rules bevor this)
    {
        rule_any = {
            instance = {},
            class = {},
            name = {
                "Wetter:.*",
                "XBindKey: Hit a key"
            },
            role = {
                "pop-up",
                "Preferences",
                "setup"
            }
        },
        properties = {
            floating = true
        }
    },
    -- Special applications mappings
    {
        rule_any = {
            class = {
                "Pavucontrol"
            }
        },
        properties = {
            screen = 2,
            tag = awful.util.tagnames[3],
            ontop = true,
            switchtotag = true,
            maximized = false,
            floating = true
        }
    },
    -- Audio Clients
    {
        rule_any = {
            class = {
                "Spotify",
                "Psst-gui",
                "Shortwave"
            }
        },
        properties = {
            screen = 2,
            tag = awful.util.tagnames[5],
            switchtotag = true,
            maximized = false,
            floating = false
        }
    },
    -- Steam
    {
        rule_any = {
            class = {"RuneScape"}
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
    },
    {
        rule_any = {
            class = {"Steam"}
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
    },
    {
        rule = {
            class = "Code"
        },
        properties = {
            screen = 1,
            tag = awful.util.tagnames[9],
            switchtotag = true,
            maximized = false,
            floating = false
        }
    },
    {
        rule_any = {
            name = {
                "AWMTT"
            }
        },
        properties = {
            screen = 2,
            tag = awful.util.tagnames[6],
            switchtotag = true,
            maximized = false,
            floating = false
        }
    },
    -- Set applications to always map on the tag 5 (Virtual/RDP) on screen 1.
    {
        rule_any = {
            class = {
                "Virt-manager"
            }
        },
        properties = {
            screen = 1,
            tag = awful.util.tagnames[5],
            switchtotag = false
        }
    },
    -- Set applications to always map on the tag 6 (RemoteDesktops) on screen 1.
    {
        rule_any = {
            class = {
                "org.remmina.Remmina"
            }
        },
        properties = {
            screen = 1,
            tag = awful.util.tagnames[6],
            switchtotag = true
        }
    },
    -- Set applications to always map on the tag 9 (Systemtools) on screen 1.
    {
        rule_any = {
            class = {
                editorgui
            }
        },
        properties = {
            screen = 1,
            tag = awful.util.tagnames[9],
            switchtotag = true
        }
    },
    -- Special handled applications

    {
        -- JetBrains InteliJ Hauptfenster
        rule = {
            class = "jetbrains-.*",
            type = "dialog"
        },
        properties = {
            maximized = false,
            float = true,
            opacity = 0.8
        }
    },
    {
        -- Teams Hauptfenster
        rule = {
            class = "Microsoft Teams.*",
            type = "normal"
        },
        properties = {
            maximized = true,
            floating = false,
            screen = 2,
            tag = awful.util.tagnames[4],
            switchtotag = false
        }
    },
    {
        -- Teams Messagebox
        rule = {
            class = "Microsoft Teams*",
            name = ".*Benachrichtigung.*"
            --~ type = "notification",
            -- name = "Microsoft Teams-Benachrichtigung"
        },
        properties = {
            maximized = false,
            floating = true,
            screen = 1,
            tag = awful.util.tagnames[1],
            switchtotag = true -- zur Nachricht springen
        }
    },
    -- Develop Consolen auf Screen 2 tag 2 schieben
    {
        rule_any = {
            name = {
                "OTC:*",
                "OMC:*"
            }
        },
        properties = {
            screen = 2,
            tag = awful.util.tagnames[2],
            switchtotag = true,
            maximized = false,
            floating = false
        }
    },
    {
        rule_any = {
            class = {
                "URxtv",
                "XTerm"
            }
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
    },
    -- Chromium Debugger Instanz auf Screen 2 tag 2 schieben
    {
        rule = {
            class = "Chromium",
            instance = "chromium *"
        },
        properties = {
            floating = false,
            tag = awful.util.tagnames[1],
            switchtotag = false
        }
    },
    -- Firefox Develop Edition auf Screen 2 tag 2 schieben
    {
        rule_any = {
            class = {
                "Google-chrome",
                "Vivaldi*",
                "firefox",
                "firefoxdeveloperedition"
            }
        },
        properties = {
            maximized = false,
            floating = false,
            tag = awful.util.tagnames[1],
            switchtotag = false
        }
    },
    -- System Monitor Consolen auf Screen 2 tag 9 schieben
    {
        rule_any = {
            name = {
                "SysMon:*",
                "Sys:*",
                "CF:*"
            },
            class = {"Gnome-system-monitor"}
        },
        properties = {
            screen = 2,
            tag = awful.util.tagnames[9],
            floating = false,
            switchtotag = true
        }
    },
    -- Finishings
    -- Dialogs
    {
        rule_any = {type = {"dialog"}},
        properties = {
            maximized = false,
            floating = true
        }
    }
}
-- }}}

-- {{{ Client Signals
-- Signal function to execute when a new client appears.
client.connect_signal(
    "manage",
    function(c)
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
    end
)

client.connect_signal(
    "swapped",
    function(c, source, is_source)
        if is_initialized and not is_source then
            sound("window-switch")
        end
    end
)

client.connect_signal(
    "raised",
    function(c)
        if is_initialized then
        -- sound("window-attention-active")
        end
    end
)

client.connect_signal(
    "lowered",
    function(c)
        if is_initialized then
        -- sound("window-attention-inactive")
        end
    end
)

client.connect_signal(
    "request::activate",
    function(c)
        if is_initialized then
        -- notify("Client", "'request::activate' event raised: " .. c.name)
        end
    end
)

client.connect_signal(
    "request::geometry",
    function(c, context, Additional)
        if is_initialized and context ~= "mouse.move" and context ~= "mouse.resize" then
            if context == "maximized" then
                notify("Client", "Maximized: " .. c.name)
            else
                -- notify("Client", "'request::geometry' event raised: " .. c.name .. " Context: " .. context)
            end
        end
    end
)

client.connect_signal(
    "property::window",
    function(c)
        if is_initialized then
        -- notify("Client", "'property::window' event raised: " .. c.name)
        end
    end
)

client.connect_signal(
    "property::size",
    function(c)
        if is_initialized then
        -- notify("Client", "'property::size' event raised: " .. c.name)
        end
    end
)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal(
    "request::titlebars",
    function(c)
        -- notify("Client", "'request::titlebars' event raised: " .. c.name)
        -- Custom
        if beautiful.titlebar_fun then
            beautiful.titlebar_fun(c)
            return
        end

        -- Default
        -- buttons for the titlebar
        local buttons =
            my_table.join(
            awful.button(
                {},
                1,
                function()
                    c:emit_signal("request::activate", "titlebar", {raise = true})
                    awful.mouse.client.move(c)
                end
            ),
            awful.button(
                {},
                3,
                function()
                    c:emit_signal("request::activate", "titlebar", {raise = true})
                    awful.mouse.client.resize(c)
                end
            )
        )

        awful.titlebar(c, {size = dpi(21)}):setup {
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
                    align = "center",
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
    end
)

-- -- Enable sloppy focus, so that focus follows mouse.
-- client.connect_signal(
--     "mouse::enter",
--     function(c)
--         c:emit_signal("request::activate", "mouse_enter", {raise = false})
--     end
-- )

client.connect_signal(
    "focus",
    function(c)
        if is_initialized then
            -- notify("Window", "'focus' event raised: " .. c.name)
            c.border_color = beautiful.border_focus
        -- c.border_color = '#ff000050'
        end
    end
)

client.connect_signal(
    "unfocus",
    function(c)
        if is_initialized then
            -- notify("Window", "'unfocus' event raised: " .. c.name)
            c.border_color = beautiful.border_normal
        end
    end
)

-- }}}

-- {{{ Awesome Signals
-- Handle runtime errors after startup
local in_error = false
awesome.connect_signal(
    "debug::error",
    function(err)
        if in_error then
            return
        end
        in_error = true

        notify("Oops, an error happened!", tostring(err), naughty.config.presets.critical, true)
        in_error = false
    end
)

awesome.connect_signal(
    "debug::deprecation",
    function(hint, see, args)
        notify("Deprecated Function called!", tostring(hint), naughty.config.presets.critical, true)
    end
)

awesome.connect_signal(
    "spawn::initiated",
    function(arg)
        -- notify("Awesome", "'spawn::initiated' event raised: ".. arg.id, naughty.config.presets.info)
        -- gdebug.dump(arg, "spawn::initiated".. arg.id, 2)
    end
)

awesome.connect_signal(
    "spawn::changed",
    function(arg)
        -- notify("Awesome", "'spawn::changed' event raised: ".. arg.id, naughty.config.presets.info)
        -- gdebug.dump(arg, "spawn::changed" .. arg.id, 2)
    end
)

awesome.connect_signal(
    "spawn::timeout",
    function(arg)
        -- notify("Awesome", "'spawn::timeout' event raised: ".. arg.id, naughty.config.presets.info)
        -- gdebug.dump(arg, "spawn::timeout".. arg.id, 2)
    end
)

awesome.connect_signal(
    "spawn::completed",
    function(arg)
        -- notify("Awesome", "'spawn::completed' event raised: ".. arg.id, naughty.config.presets.info)
        -- gdebug.dump(arg, "spawn::completed".. arg.id, 2)
    end
)

awesome.connect_signal(
    "startup",
    function(hint, see, args)
        sound("desktop-login")

        -- Autostart applications
        awful.spawn.with_shell("sh ~/.scripts/autostart-awesome.sh")

        -- notify("Awesome", "'Autostart' callback raised")
        is_initialized = true
        -- notify("Awesome Default", "Awesome Default erfolgreich gestartet !!", naughty.config.presets.critical, false)
    end
)
-- }}}

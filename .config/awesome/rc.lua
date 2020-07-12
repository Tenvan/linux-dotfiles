--[[

     Awesome WM configuration template
     https://github.com/awesomeWM

     Freedesktop : https://github.com/lcpz/awesome-freedesktop

     Copycats themes : https://github.com/lcpz/awesome-copycats

     lain : https://github.com/lcpz/lain

--]] -- {{{ Required libraries
local awesome, client, mouse, screen, tag = awesome, client, mouse, screen, tag
local ipairs, string, os, table, tostring, tonumber, type = ipairs, string, os, table, tostring, tonumber, type

-- https://awesomewm.org/doc/api/documentation/05-awesomerc.md.html
-- Standard awesome library
local gears = require("gears") -- Utilities such as color parsing and objects
local awful = require("awful") -- Everything related to window managment

local gdebug = require("gears.debug")

require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")

-- Theme handling library
local beautiful = require("beautiful")

-- Notification library
local naughty = require("naughty")
naughty.config.defaults["icon_size"] = 100

-- local menubar       = require("menubar")

local lain = require("lain")
local freedesktop = require("freedesktop")

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
local hotkeys_popup = require("awful.hotkeys_popup").widget
require("awful.hotkeys_popup.keys")
local my_table = awful.util.table or gears.table -- 4.{0,1} compatibility
local dpi = require("beautiful.xresources").apply_dpi
-- }}}

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify(
        {
            preset = naughty.config.presets.critical,
            title = "Oops, there were errors during startup!",
            text = awesome.startup_errors
        }
    )
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal(
        "debug::error",
        function(err)
            if in_error then
                return
            end
            in_error = true

            naughty.notify(
                {
                    preset = naughty.config.presets.critical,
                    title = "Oops, an error happened!",
                    text = tostring(err)
                }
            )
            in_error = false
        end
    )
end
-- }}}

-- {{{ Autostart windowless processes
local function run_once(cmd_arr)
    for _, cmd in ipairs(cmd_arr) do
        awful.spawn.with_shell(string.format("pgrep -u $USER -fx '%s' > /dev/null || (%s)", cmd, cmd))
    end
end

run_once({"unclutter -root"}) -- entries must be comma-separated
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

local themes = {
    "multicolor", -- 1
    "powerarrow", -- 2
    "powerarrow-blue", -- 3
    "blackburn", -- 4
    "mytheme" -- 5
}

-- choose your theme here
local chosen_theme = themes[5]

local theme_path = string.format("%s/.config/awesome/themes/%s/theme.lua", os.getenv("HOME"), chosen_theme)
beautiful.init(theme_path)

-- modkey or mod4 = super key
local modkey = "Mod4"
local altkey = "Mod1"
local controlkey = "Control"
local shiftkey = "Shift"
local returnkey = "Return"
local escapekey = "Escape"

-- personal variables
-- change these variables if you want
local browser =  os.getenv("BROWSER") or "firefox-developer-edition"
local editor = os.getenv("EDITOR") or "nvim"
local editorgui = "Geany"
local filemanager = "nemo"
local mailclient = "evolution"
local mediaplayer = "spotify"
local terminal = "termite"
local virtualmachine = "virtualbox"

-- key groups
local kgApplication = "application"
local kgAwesome = "awesome"
local kgClient = "client"
local kgLayout = "layout"
local kgMaster = "master"
local kgMenu = "menu"
local kgScreen = "screen"
local kgSound = "sound"
local kgSystem = "system"
local kgTag = "tag"

-- awesome variables
awful.util.terminal = terminal
awful.util.tagnames = { "➊", "➋", "➌", "➍", "➎", "➏", "➐", "➑", "➒", "➓" }
-- awful.util.tagnames = { "", "", "", "", "", "", "", "", "" }
-- awful.util.tagnames = {  "", "", "", "", "", "", "", "", "", "" }
-- awful.util.tagnames = { "⠐", "⠡", "⠲", "⠵", "⠻", "⠿" }
-- awful.util.tagnames = { "⌘", "♐", "⌥", "ℵ" }
-- awful.util.tagnames = { "www", "edit", "gimp", "inkscape", "music" }
-- Use this : https://fontawesome.com/cheatsheet
-- awful.util.tagnames = { "", "", "", "", "" }
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
    awful.layout.suit.magnifier,
    awful.layout.suit.corner.nw,
    awful.layout.suit.corner.ne,
    awful.layout.suit.corner.sw,
    awful.layout.suit.corner.se
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

beautiful.init(string.format("%s/.config/awesome/themes/%s/theme.lua", os.getenv("HOME"), chosen_theme))
-- }}}

-- {{{ Menu
local myawesomemenu = {
    {"hotkeys", function()
            return false, hotkeys_popup.show_help
        end},
    {"arandr", "arandr"}
}

awful.util.mymainmenu =
    freedesktop.menu.build(
    {
        before = {
            {"Awesome", myawesomemenu}
            -- { "Atom", "atom" },
            -- other triads can be put here
        },
        after = {
            {"Terminal", terminal},
            {"Log out", function()
                    awesome.quit()
                end},
            {"Sleep", "systemctl suspend"},
            {"Restart", "systemctl reboot"},
            {"Shutdown", "systemctl poweroff"}
            -- other triads can be put here
        }
    }
)
-- hide menu when mouse leaves it
-- awful.util.mymainmenu.wibox:connect_signal("mouse::leave", function() awful.util.mymainmenu:hide() end)

-- menubar.utils.terminal = terminal -- Set the Menubar terminal for applications that require it
-- }}}

-- {{{ Screen
-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal(
    "property::geometry",
    function(s)
        -- Wallpaper
        if beautiful.wallpaper then
            local wallpaper = beautiful.wallpaper
            -- If wallpaper is a function, call it with the screen
            if type(wallpaper) == "function" then
                wallpaper = wallpaper(s)
            end
            gears.wallpaper.maximized(wallpaper, s, true)
        end
    end
)

-- No borders when rearranging only 1 non-floating or maximized client
screen.connect_signal(
    "arrange",
    function(s)
        local only_one = #s.tiled_clients == 1
        for _, c in pairs(s.clients) do
            -- special Toolbar handling
            if (c.class == "Polybar") or (c.class == "Plank") then
            elseif only_one and not c.floating or c.maximized then
                c.border_width = 2
            else
                c.border_width = beautiful.border_width
            end
        end
    end
)

-- Create a wibox for each screen and add it
awful.screen.connect_for_each_screen(
    function(s)
        beautiful.at_screen_connect(s)
        s.systray = wibox.widget.systray()
        s.systray.visible = true
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
            end
        ),
        awful.button({}, 4, awful.tag.viewnext),
        awful.button({}, 5, awful.tag.viewprev)
    )
)
-- }}}

-- {{{ Key bindings
globalkeys =
    my_table.join(
    -- System Functions
    awful.key(
        {controlkey, modkey},
        "x",
        function()
            awful.spawn.spawn(string.format("%s -t XProp --hold -e %s", terminal, "xprop"))
        end,
        {description = "start xprop", group = kgSystem}
    ),

    awful.key(
        {modkey},
        "t",
        function()
            awful.spawn.with_shell("sh $HOME/Scripts/picom-toggle-awesome.sh")
        end,
        {description = "toggle picom", group = kgSystem}
    ),

    -- Function keys
    awful.key(
        {modkey},
        "F12",
        function()
            awful.util.spawn("xfce4-terminal --drop-down")
        end,
        {description = "dropdown terminal", group = kgApplication}
    ),

    -- super + ... function keys
    awful.key(
        {modkey},
        "F1",
        function()
            awful.util.spawn(browser)
        end,
        {description = browser, group = kgApplication}
    ),
    awful.key(
        {modkey},
        "F7",
        function()
            awful.util.spawn("virtualbox")
        end,
        {description = virtualmachine, group = kgApplication}
    ),
    awful.key(
        {modkey},
        "F8",
        function()
            awful.util.spawn(filemanager)
        end,
        {description = filemanager, group = kgApplication}
    ),
    awful.key(
        {modkey},
        "c",
        function()
            awful.util.spawn("conky-toggle")
        end,
        {description = "conky-toggle", group = "conky"}
    ),
    awful.key(
        {modkey},
        "x",
        function()
            awful.util.spawn("arcolinux-logout")
        end,
        {description = "exit", group = kgSystem}
    ),
    awful.key(
        {modkey},
        escapekey,
        function()
            awful.util.spawn("xkill")
        end,
        {description = "Kill proces", group = kgSystem}
    ), -- super + shift + ...
    awful.key(
        {modkey, shiftkey},
        returnkey,
        function()
            awful.util.spawn(filemanager)
        end
    ),
    awful.key({modkey, shiftkey}, "r", awesome.restart, {description = "reload awesome", group = kgAwesome}),
    awful.key({modkey, shiftkey}, "x", awesome.quit, {description = "quit awesome", group = kgAwesome}),
    awful.key(
        {controlkey, shiftkey},
        escapekey,
        function()
            awful.util.spawn("xfce4-taskmanager")
        end,
        {description = "taskmanager", group = kgSystem}
    ), -- ctrl+alt +  ...
    awful.key(
        {controlkey, altkey},
        "a",
        function()
            awful.util.spawn("xfce4-appfinder")
        end,
        {description = "Xfce appfinder", group = kgSystem}
    ), -- alt + ...
    awful.key(
        {altkey},
        "F3",
        function()
            awful.util.spawn("xfce4-appfinder")
        end,
        {description = "Xfce appfinder", group = kgSystem}
    ), -- screenshots
    -- Personal keybindings}}}
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
    ), -- By direction client focus with arrows
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
    ), -- Layout manipulation
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
    -- Show/Hide SystraySystray
    awful.key(
        {modkey},
        "-",
        function()
            awful.screen.focused().systray.visible = not awful.screen.focused().systray.visible
        end,
        {description = "Toggle systray visibility", group = kgSystem}
    ),
    -- Show/Hide Systray
    awful.key(
        {modkey},
        "KP_Subtract",
        function()
            awful.screen.focused().systray.visible = not awful.screen.focused().systray.visible
        end,
        {description = "Toggle systray visibility", group = kgSystem}
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
    -- Standard program
    awful.key(
        {modkey},
        returnkey,
        function()
            awful.spawn(terminal)
        end,
        {description = terminal, group = kgSystem}
    ),
    awful.key(
        {altkey, shiftkey},
        "Up",
        function()
            awful.tag.incmwfact(0.05)
        end,
        {
            description = "increase master width factor",
            group = kgMaster
        }
    ),
    awful.key(
        {altkey, shiftkey},
        "Down",
        function()
            awful.tag.incmwfact(-0.05)
        end,
        {
            description = "decrease master width factor",
            group = kgMaster
        }
    ),
    awful.key(
        {modkey, shiftkey},
        "Up",
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
        "l",
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
    awful.key(
        {modkey, shiftkey},
        "space",
        function()
            awful.layout.inc(1)
        end,
        {description = "select next", group = kgLayout}
    ),
    -- awful.key({ modkey, shiftkey   }, "space", function () awful.layout.inc(-1)                end,
    -- {description = "select previous", group = kgLayout}),

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
        {description = "dropdown application", group = kgSystem}
    ),
    -- Widgets popups
    -- awful.key({ altkey, }, "c", function () lain.widget.calendar.show(7) end,
    -- {description = "show calendar", group = "widgets"}),
    -- awful.key({ altkey, }, "h", function () if beautiful.fs then beautiful.fs.show(7) end end,
    -- {description = "show filesystem", group = "widgets"}),
    -- awful.key({ altkey, }, "w", function () if beautiful.weather then beautiful.weather.show(7) end end,
    -- {description = "show weather", group = "widgets"}),
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
        {},
        "XF86AudioLowerVolume",
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
        {},
        "XF86AudioPlay",
        function()
          awful.util.spawn("playerctl play-pause")
        end,
        {description = "Player Start/Pause", group = kgSound}
    ),
    awful.key(
        {},
        "XF86AudioNext",
        function()
          awful.util.spawn("playerctl next")
        end,
        {description = "Player Next", group = kgSound}
    ),
    awful.key(
        {},
        "XF86AudioPrev",
        function()
          awful.util.spawn("playerctl previous")
        end,
        {description = "Player Zurück", group = kgSound}
    ),
    awful.key(
        {},
        "XF86AudioStop",
        function()
            awful.util.spawn("Player Stop")
        end
    ),
    -- Default -- Menu Keys
    --[[ Menubar
    awful.key({ modkey }, "p", function() menubar.show() end,
            {description = "show the menubar", group = "super"})
    --]] awful.key(
        {modkey},
        "z",
        function()
            awful.spawn.with_shell("rofi -show run")
        end,
        {description = "rofi combi", group = kgMenu}
    ),
    awful.key(
        {modkey, shiftkey},
        "z",
        function()
            awful.spawn.with_shell("rofi -show combi")
        end,
        {description = "rofi combi", group = kgMenu}
    ),
    awful.key(
        {modkey},
        "F2",
        function()
            awful.spawn.with_shell("xfce4-appfinder")
        end,
        {description = "app finder", group = kgMenu}
    ),
    awful.key(
        {modkey},
        "w",
        function()
            awful.spawn.with_shell("bwmenu -- -location 2")
        end,
        {description = "bitwarden menu", group = kgMenu}
    ), -- Custom Menus
    awful.key(
        {modkey, altkey},
        "a",
        function()
            awful.spawn.with_shell("sh $HOME/Scripts/menu/app-menu.sh")
        end,
        {description = "start application", group = kgMenu}
    ),
    awful.key(
        {modkey},
        "d",
        function()
            awful.spawn.with_shell("sh $HOME/Scripts/menu/develop-menu.sh")
        end,
        {description = "developer actions", group = kgMenu}
    ),
    awful.key(
        {modkey, altkey},
        "d",
        function()
            awful.spawn.with_shell("sh $HOME/Scripts/menu/develop-menu.sh")
        end,
        {description = "developer actions", group = kgMenu}
    ),
    awful.key(
        {modkey, altkey},
        "e",
        function()
            awful.spawn.with_shell("sh $HOME/Scripts/menu/edit-configs.sh")
        end,
        {description = "edit config", group = kgMenu}
    ),
    awful.key(
        {modkey, altkey},
        "m",
        function()
            awful.spawn.with_shell("sh $HOME/Scripts/menu/system-monitor.sh")
        end,
        {description = "system monitors", group = kgMenu}
    ),
    awful.key(
        {modkey, altkey},
        "q",
        function()
            awful.spawn.with_shell("sh $HOME/Scripts/menu/system-menu.sh")
        end,
        {description = "system menu", group = kgMenu}
    )
)

clientkeys =
    my_table.join(
    awful.key({altkey, shiftkey}, "m", lain.util.magnify_client, {description = "magnify client", group = kgClient}),
    awful.key(
        {modkey, shiftkey},
        "f",
        awful.client.floating.fullscreen,
        -- function(c)
        --   c.fullscreen = not c.fullscreen
        --   c:raise()
        -- end,
        {description = "toggle fullscreen", group = kgClient}
    ),
    awful.key(
        {modkey},
        "q",
        function(c)
            c:kill()
        end,
        {description = "close", group = kgClient}
    ),
    awful.key({modkey}, "space", awful.client.floating.toggle, {description = "toggle floating", group = kgClient}),
    awful.key(
        {modkey, controlkey},
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

clientbuttons =
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
            size_hints_honor = false
        }
    },
    -- Toolbars
    {
        rule_any = { class = { "Polybar", "Plank" }},
        properties = {border_width = 0}
    },
    -- Titlebars
    {
        rule_any = {type = {"dialog", "normal"}},
        properties = {titlebars_enabled = true}
    },
    -- Dialogs
    {
        rule_any = {type = {"dialog"}},
        properties = {maximized = false, floating = true}
    },

    -- Set applications to always map on the tag 2 on screen 1.
    -- { rule = { class = "Subl3" },
    -- properties = { screen = 1, tag = awful.util.tagnames[2], switchtotag = true  } },

    -- Set applications to always map on the tag 1 on screen 1.
    -- find class or role via xprop command

    -- { rule = { class = browser },
    -- properties = { screen = 1, tag = awful.util.tagnames[1], switchtotag = true  } },

    -- { rule = { class = "Chromium" },
    -- properties = { screen = 1, tag = awful.util.tagnames[1], switchtotag = true  } },

    -- { rule = { class = "Opera" },
    -- properties = { screen = 1, tag = awful.util.tagnames[1],switchtotag = true  } },

    -- Set applications to always map on the tag 2 on screen 1.
    -- { rule = { class = "Subl3" },
    -- properties = { screen = 1, tag = awful.util.tagnames[2],switchtotag = true  } },

    -- { rule = { class = "Brackets" },
    -- properties = { screen = 1, tag = awful.util.tagnames[2], switchtotag = true  } },

    -- { rule = { class = "Code" },
    -- properties = { screen = 1, tag = awful.util.tagnames[2], switchtotag = true  } },

    --  { rule = { class = "Geany" },
    --  properties = { screen = 1, tag = awful.util.tagnames[2], switchtotag = true  } },

    -- Set applications to always map on the tag 3 (Git)) on screen 1.
    {
        rule_any = {
            class = {
                "SmartGit"
            }
        },
        properties = { screen = 1, tag = awful.util.tagnames[3] , switchtotag = true  }
    },

    -- Set applications to always map on the tag 4 on screen 2.

    -- Set applications to always map on the tag 5 (Virtual/RDP) on screen 1.
    {
        rule_any = {
            class = {
                "VirtualBox*"
            }
        },
        properties = { screen = 1, tag = awful.util.tagnames[5] , switchtotag = true  }
    },

    -- Set applications to always map on the tag 9 (Systemtools) on screen 1.
    {
        rule_any = {
            class = {
                editorgui
            }
        },
        properties = { screen = 1, tag = awful.util.tagnames[9] , switchtotag = true  }
    },


    -- Set applications to be maximized at startup.
    -- find class or role via xprop command

    {rule = {class = "Geany"}, properties = {maximized = false, floating = false}},
    {
        rule = {class = "Geany", type = "dialog"},
        properties = {maximized = false, floating = true}
    },

    {
        rule = {class = "Vivaldi-stable"},
        properties = {maximized = false, floating = false}
    },
    {
        rule = {class = "Vivaldi-stable"},
        properties = {callback = function(c)
                c.maximized = false
            end}
    },
    -- IF using Vivaldi snapshot you must comment out the rules above for Vivaldi-stable as they conflict
    --    { rule = { class = "Vivaldi-snapshot" },
    --          properties = { maximized = false, floating = false } },

    --    { rule = { class = "Vivaldi-snapshot" },
    --          properties = { callback = function (c) c.maximized = false end } },

    {rule = {class = "Xfce4-settings-manager"}, properties = {floating = false}},

    -- Maximized clients.
    {
        rule = {class = "Gimp*", role = "gimp-image-window"},
        properties = {maximized = true}
    },

    {
        rule_any = {
            instance = {
            },
            class = {
                mediaplayer,
                "inkscape",
                "VirtualBox Machine",
                "Vlc",
            },
            name = {
            },
            role = {
            }
        },
        properties = {maximized = true}
    },

    -- Floating clients.
    {
        rule_any = {
            instance = {
                "DTA", -- Firefox addon DownThemAll.
                "copyq" -- Includes session name in class.
            },
            class = {
                "Arandr",
                "Arcolinux-welcome-app.py",
                "Blueberry",
                "Galculator",
                "Gnome-disks",
                "Gnome-font-viewer",
                "Gpick",
                "Imagewriter",
                "Font-manager",
                "Kruler",
                "MessageWin", -- kalarm.
                "Nm-connection-editor",
                "arcolinux-logout",
                "Pavucontrol",
                "Peek",
                "Skype",
                "System-config-printer.py",
                "Sxiv",
                "Unetbootin.elf",
                "Wpa_gui",
                "pinentry",
                "veromix",
                "VirtualBox Manager",
                "VirtualBoxVM",
                "xtightvncviewer",
                "Xfce4-terminal",
                "Xfce4-taskmanager",
                "Xfce4-appfinder"
            },
            name = {
                "Event Tester", -- xev.
                "XProp"
            },
            role = {
                "AlarmWindow", -- Thunderbird's calendar.
                "pop-up", -- e.g. Google Chrome's (detached) Developer Tools.
                "Preferences",
                "setup"
            }
        },
        properties = {floating = true}
    },

    -- Floating clients but centered in screen
    {
        rule_any = {class = {"Polkit-gnome-authentication-agent-1"}},
        properties = {floating = true},
        callback = function(c)
            awful.placement.centered(c, nil)
        end
    },

    -- Special handled applications ()

    -- Teams
    {
        -- Teams Hauptfenster
        rule = {
            class = "Microsoft Teams - Preview",
            type = "normal"
        },
        properties = {
            maximized = true,               -- maximieren
            screen = 2,                     -- auf zweiten Monitor
            tag = awful.util.tagnames[4],   -- auf tag 4 verschieben
            switchtotag = false             -- auf aktuellem tag bleiben
        }
    },
    {
        -- Teams Messagebox
        rule = {
            class = "Microsoft Teams - Preview",
            name = "Microsoft Teams-Benachrichtigung"
            --~ type = "notification",
            -- name = "Microsoft Teams-Benachrichtigung"
        },
        properties = {
            maximized = false,              -- nicht maximieren
            floating = true,
            screen = 1,                     -- auf ersten Monitor
            tag = awful.util.tagnames[1],   -- auf tag 4 verschieben
            switchtotag = true,             -- zur Nachricht springen
        },
    },

    -- Develop Consolen auf Screen 2 tag 2 schieben
    {
        rule = { name = "OTC:*" },
        properties = {
            screen = 2,                     -- auf zweiten Monitor
            tag = awful.util.tagnames[2],   -- auf tag 2
            switchtotag = true,             -- zur Ausgabe springen
        },
    },

    -- Chromium Debugger Instanz Screen 2 tag 2 schieben
    {
        rule = {
            class = "Chromium",
            instance = "chromium *",
        },
        properties = {
            screen = 2,                     -- auf zweiten Monitor
            tag = awful.util.tagnames[2],   -- auf tag 2
            switchtotag = true,             -- zur Ausgabe springen
        },
    },


}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal(
    "manage",
    function(c)
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- if not awesome.startup then awful.client.setslave(c) end

        if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
            -- Prevent clients from being unreachable after screen count changes.
            awful.placement.no_offscreen(c)
        end
    end
)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal(
    "request::titlebars",
    function(c)
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

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal(
    "mouse::enter",
    function(c)
        c:emit_signal("request::activate", "mouse_enter", {raise = false})
    end
)

client.connect_signal(
    "focus",
    function(c)
        c.border_color = beautiful.border_focus
    end
)
client.connect_signal(
    "unfocus",
    function(c)
        c.border_color = beautiful.border_normal
    end
)

-- }}}

-- Autostart applications
awful.spawn.with_shell("sh ~/Scripts/autostart-global.sh")
awful.spawn.with_shell("sh ~/Scripts/autostart-awesome.sh")

local awful = require('awful')
local beautiful = require('beautiful')

require('awful.autofocus')

local hotkeys_popup = require('awful.hotkeys_popup').widget

local modkey = require('configuration.keys.mod').mod_key
local altkey = require('configuration.keys.mod').alt_key
local apps = require('configuration.apps')

-- modkey or mod4 = super key
local modkey = "Mod4"
local altkey = "Mod1"
local controlkey = "Control"
local shiftkey = "Shift"
local returnkey = "Return"
local escapekey = "Escape"
local tabkey = 'Tab'
local downkey = 'Down'
local upkey = 'Up'
local leftkey = 'Left'
local rightkey = 'Right'

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
local kgUtils = "Tools"
local kgHotkeys = "Hotkeys"
local kgLauncher = "Starter"

-- Key bindings
-- alt + ...
-- Hotkeys Awesome
-- Non-empty tag browsing
-- awful.key({ modkey }, "Left", function () lain.util.tag_view_nonempty(-1) end,
-- {description = "view  previous nonempty", group = kgTag}),
-- awful.key({ modkey }, "Right", function () lain.util.tag_view_nonempty(1) end,
-- {description = "view  previous nonempty", group = kgTag}),
-- Default client focus
-- Window Sizing
awful.key({modkey, shiftkey}, "Next", function()
    awful.client.moveresize(20, 20, -40, -40)
end, {
    description = "increase window size",
    group = kgClient
}), 

awful.key({modkey, shiftkey}, "Prior", function()
    awful.client.moveresize(-20, -20, 40, 40)
end, {
    description = "decrease window size",
    group = kgClient
}), 

awful.key({modkey, shiftkey}, "Down", function()
    awful.client.incwfact(0.05)
end, {
    description = "increase window size",
    group = kgClient
}), 

awful.key({modkey, shiftkey}, "Up", function()
    awful.client.incwfact(-0.05)
end, {
    description = "decrease window size",
    group = kgClient
}), 

awful.key({modkey, shiftkey}, "Left", function()
    awful.tag.incmwfact(-0.05)
end, {
    description = "increase master size",
    group = kgClient
}), awful.key({modkey, shiftkey}, "Right", function()
    awful.tag.incmwfact(0.05)
end, {
    description = "decrease master size",
    group = kgClient
}), awful.key({modkey}, "Right", function()
    awful.client.focus.byidx(1)
end, {
    description = "focus next by index",
    group = kgClient
}), awful.key({modkey}, "Left", function()
    awful.client.focus.byidx(-1)
end, {
    description = "focus previous by index",
    group = kgClient
}), -- By direction client focus with arrows
awful.key({controlkey, modkey}, "Down", function()
    awful.client.focus.global_bydirection("down")
    if client.focus then
        client.focus:raise()
    end
end, {
    description = "focus down",
    group = kgClient
}), awful.key({controlkey, modkey}, "Up", function()
    awful.client.focus.global_bydirection("up")
    if client.focus then
        client.focus:raise()
    end
end, {
    description = "focus up",
    group = kgClient
}), awful.key({controlkey, modkey}, "Left", function()
    awful.client.focus.global_bydirection("left")
    if client.focus then
        client.focus:raise()
    end
end, {
    description = "focus left",
    group = kgClient
}), awful.key({controlkey, modkey}, "Right", function()
    awful.client.focus.global_bydirection("right")
    if client.focus then
        client.focus:raise()
    end
end, {
    description = "focus right",
    group = kgClient
}), -- Layout manipulation
awful.key({modkey}, "Down", function()
    awful.client.swap.byidx(1)
end, {
    description = "swap with next client by index",
    group = kgClient
}), awful.key({modkey}, "Up", function()
    awful.client.swap.byidx(-1)
end, {
    description = "swap with previous client by index",
    group = kgClient
}), awful.key({modkey}, ".", function()
    awful.screen.focus_relative(1)
end, {
    description = "focus the next screen",
    group = kgScreen
}), awful.key({modkey}, ",", function()
    awful.screen.focus_relative(-1)
end, {
    description = "focus the previous screen",
    group = kgScreen
}), awful.key({modkey}, "u", awful.client.urgent.jumpto, {
    description = "jump to urgent client",
    group = kgClient
}), -- Show/Hide Wibox
awful.key({modkey}, "b", function()
    for s in screen do
        s.mywibox.visible = not s.mywibox.visible
        if s.mybottomwibox then
            s.mybottomwibox.visible = not s.mybottomwibox.visible
        end
    end
end, {
    description = "toggle wibox",
    group = kgLayout
}), -- On the fly useless gaps change
awful.key({modkey, altkey}, "Up", function()
    lain.util.useless_gaps_resize(1)
end, {
    description = "increment useless gaps",
    group = kgLayout
}), awful.key({modkey, altkey}, "Down", function()
    lain.util.useless_gaps_resize(-1)
end, {
    description = "decrement useless gaps",
    group = kgLayout
}), -- On the fly master handling
awful.key({modkey, controlkey}, "m", function()
    awful.tag.incnmaster(1, nil, true)
end, {
    description = "increase the number of master clients",
    group = kgMaster
}), awful.key({modkey, shiftkey}, "m", function()
    awful.tag.incnmaster(-1, nil, true)
end, {
    description = "decrease the number of master clients",
    group = kgMaster
}), awful.key({modkey, controlkey}, "h", function()
    awful.tag.incncol(1, nil, true)
end, {
    description = "increase the number of columns",
    group = kgLayout
}), awful.key({modkey, controlkey}, "l", function()
    awful.tag.incncol(-1, nil, true)
end, {
    description = "decrease the number of columns",
    group = kgLayout
}), 
-- On the fly layout handling

awful.key({}, "XF86MonBrightnessUp", function()
    os.execute("xbacklight -inc 10")
end, {
    description = "+10% Helligkeit",
    group = kgSound
}), awful.key({}, "XF86MonBrightnessDown", function()
    os.execute("xbacklight -dec 10")
end, {
    description = "-10% Helligkeit",
    group = kgSound
}), 

-- ALSA volume control


awful.key({modkey}, "F2", function()
    awful.spawn("xfce4-appfinder")
end, {
    description = "Anwendungs Finder",
    group = kgMenus
}),


-- Shortcuts to Applications
-- System Tools
)

local clientkeys = my_table.join(awful.key({altkey, shiftkey}, "m", lain.util.magnify_client, {
    description = "magnify client",
    group = kgClient
}), awful.key({modkey}, "q", function(c)
    c:kill()
end, {
    description = "close",
    group = kgClient
}), awful.key({modkey, altkey}, "m", function(c)
    c:swap(awful.client.getmaster())
end, {
    description = "move to master",
    group = kgMaster
}), awful.key({modkey, shiftkey}, "t", function(c)
    c.ontop = not c.ontop
end, {
    description = "toggle keep on top",
    group = kgClient
}), awful.key({modkey, shiftkey}, "i", function(c)
    notify("Oops, dies ist eine Test-Benachrichtung!",
        "Dolor et est dolor sed labore dolores, lorem sea kasd sed accusam.\nNonumy ipsum elitr aliquyam eirmod.\nNo sit lorem.",
        naughty.config.presets.critical, true)
end, {
    description = "toggle keep on top",
    group = kgClient
}))

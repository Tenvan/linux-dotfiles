local awesome, client, screen = awesome, client, screen

local awful = require("awful")
local freedesktop = require('freedesktop')
local hotkeys_popup = require('awful.hotkeys_popup').widget
local apps = require("configuration.apps")
local beautiful = require('beautiful')
local gears = require("gears")

local awesomemenu = {
    {
        "Hotkeys",
        function()
            hotkeys_popup.show_help(nil, awful.screen.focused())
        end,
    },
    { "Manual", apps.default.terminal .. " -e man awesome" },
    { "Edit Config", apps.default.text_editor .. " " .. awesome.conffile },
    { "Restart", awesome.restart },
    {
        "Quit",
        function()
            awesome.quit()
        end,
    },
}
local mainmenu = freedesktop.menu.build({
    before = { { 'Awesome', awesomemenu, beautiful.awesome_icon } -- { "Atom", "atom" },
        -- other triads can be put here
    },
    after = {
        { 'Terminal', apps.default.terminal },
        { 'Log out', function()
            awesome.quit()
        end },
        { 'Sleep', 'systemctl suspend' },
        { 'Restart', 'systemctl reboot' },
        { 'Shutdown', 'systemctl poweroff' } -- other triads can be put here
    }
})

mymainmenu = mainmenu

return {
    mainmenu = mainmenu
}
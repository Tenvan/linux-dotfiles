local log = require('utilities.debug').log
log("Enter Module => utilities/notify.lua" )

local beautiful = require("beautiful")
local dpi = require('beautiful.xresources').apply_dpi

-- ░█▀█░█▀█░▀█▀░▀█▀░█▀▀░▀█▀░█▀▀░█▀█░▀█▀░▀█▀░█▀█░█▀█░░░█░░░▀█▀░█▀▄░█▀▄░█▀█░█▀▄░█░█
-- ░█░█░█░█░░█░░░█░░█▀▀░░█░░█░░░█▀█░░█░░░█░░█░█░█░█░░░█░░░░█░░█▀▄░█▀▄░█▀█░█▀▄░░█░
-- ░▀░▀░▀▀▀░░▀░░▀▀▀░▀░░░▀▀▀░▀▀▀░▀░▀░░▀░░▀▀▀░▀▀▀░▀░▀░░░▀▀▀░▀▀▀░▀▀░░▀░▀░▀░▀░▀░▀░░▀░
local naughty = require('naughty')
naughty.config.defaults['border_width'] = beautiful.border_width
naughty.config.defaults['position'] = 'bottom_right'
naughty.config.defaults['margin'] = dpi(10)
naughty.config.spacing = dpi(10)
naughty.config.icon_dirs = { '/usr/share/icons/Adwaita/32x32/actions/', '/usr/share/icons/Adwaita/32x32/apps/',
    '/usr/share/icons/Adwaita/32x32/categories/', '/usr/share/icons/Adwaita/32x32/devices/',
    '/usr/share/icons/Adwaita/32x32/emblems/', '/usr/share/icons/Adwaita/32x32/emotes/',
    '/usr/share/icons/Adwaita/32x32/legacy/', '/usr/share/icons/Adwaita/32x32/mimetypes/',
    '/usr/share/icons/Adwaita/32x32/places/', '/usr/share/icons/Adwaita/32x32/status/',
    '/usr/share/icons/Adwaita/32x32/ui/' }


local function notify(titel, message, presets, playSound, icon)
    -- Icon Definitions: https://specifications.freedesktop.org/icon-naming-spec/latest/ar01s04.html

    naughty.notify({
        timeout = 2,
        icon_size = 32,
        presets = presets,
        text = message,
        title = titel,
        icon = icon
    })
end

-- return {
--     notify = notify,
-- }
return  notify

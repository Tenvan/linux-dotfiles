local naughty = require("naughty")

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

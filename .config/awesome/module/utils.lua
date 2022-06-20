local awful = require("awful") -- Everything related to window managment
local naughty = require("naughty")

local sound_path = string.format("%s/.scripts/play-sound.zsh", os.getenv("HOME"))

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

    if playSound == true then
        if category == naughty.config.presets.critical then
            awful.spawn(sound_path .. " " .. "notify-error")
        end
    end
end

local function sound(soundFile)
    notify("Play Sound", soundFile, naughty.config.presets.info)
    awful.spawn(sound_path .. " " .. soundFile)
end

local function makeColorTransparent(colorkey, opacity)
    -- gdebug.print_warning("ColorKey: " .. colorkey)
    local colorMain = string.sub(colorkey, 2, 7)
    local transColor = "#" .. colorMain .. opacity
    -- gdebug.print_warning("Color: " .. transColor)
    return transColor
end

return {
    notify = notify,
    sound = sound,
    makeColorTransparent = makeColorTransparent
}

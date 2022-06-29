local log = require('utilities.debug').log
log("Enter Module => utilities/sound.lua" )

local awful = require("awful") -- Everything related to window managment

local sound_path = string.format("%s/.scripts/play-sound.zsh", os.getenv("HOME"))

local function sound(soundFile)
    awful.spawn(sound_path .. " " .. soundFile)
end

return sound

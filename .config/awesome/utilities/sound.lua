log("Enter Module => " .. ... )

local awful = require('awful') -- Everything related to window managment
local sound_path = string.format('%s/.scripts/play-sound.zsh', os.getenv('HOME'))
local config = require('configuration.config')

local function sound(soundFile)
  if config.sounds then
    awful.spawn(sound_path .. ' ' .. soundFile)
  end
end

return sound

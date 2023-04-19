log('Enter Module => ' .. ...)

local awful = require('awful') -- Everything related to window managment
local sound_path = string.format('%s/.bin/playsound', os.getenv('HOME'))
local config = require('configuration.config')

local function sound(soundFile)
  log('play sound: ' .. soundFile)
  if config.sounds then
    awful.spawn(sound_path .. ' ' .. soundFile)
  end
end

local function increaseVol(vol, source)
  log(string.format('volume increase(%s): %s', source, tostring(vol)))
  awful.spawn('ponymix increase ' .. vol .. '%')
  sound('audio-volume-change')
end

local function decreaseVol(vol, source)
  log(string.format('volume decrease(%s): %s', source, tostring(vol)))
  awful.spawn('ponymix decrease ' .. vol .. '%')
  sound('audio-volume-change')
end

local function setVol(vol, source)
  log(string.format('set volume(%s): %d', source, tostring(vol)))
  awful.spawn('ponymix set-volume ' .. vol .. '%')
  sound('audio-volume-change')
end

local function toggleVol()
  log('volume toggle')
  awful.spawn('ponymix toggle')
end

local function muteVol()
  log('volume mute')
  awful.spawn('ponymix mute')
end

local function unmuteVol()
  log('volume unmute')
  awful.spawn('ponymix unmute')
end

local function muteCapture()
  log('source mute')
  awful.spawn('ponymix mute --source')
end

local function unmuteCapture()
  log('source unmute')
  awful.spawn('ponymix unmute --source')
end

return {
  sound = sound,
  increaseVol = increaseVol,
  decreaseVol = decreaseVol,
  setVol = setVol,
  unmuteVol = unmuteVol,
  muteVol = muteVol,
  toggleVol = toggleVol,
  muteCapture = muteCapture,
  unmuteCapture = unmuteCapture
}

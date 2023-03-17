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

local function increaseVol(vol)
  awful.spawn('ponymix increase ' .. vol .. '%')
  sound('audio-volume-change')
end

local function decreaseVol(vol)
  awful.spawn('ponymix decrease ' .. vol .. '%')
  sound('audio-volume-change')
end

local function setVol(vol)
  awful.spawn('ponymix set-volume ' .. vol)
  sound('audio-volume-change')
end

local function toggleVol()
  awful.spawn('ponymix toggle')
  log('Laustärke umgeschaltet')
end

local function muteVol()
  awful.spawn('ponymix mute')
  log('Laustärke abgeschaltet')
end

local function unmuteVol()
  awful.spawn('ponymix unmute')
  log('Laustärke eingeschaltet')
end

local function muteCapture()
  awful.spawn('ponymix mute --source')
  log('Mikrofon ausgeschaltet')
end

local function unmuteCapture()
  awful.spawn('ponymix unmute --source')
  log('Mikrofon ausgeschaltet')
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

log('Enter Module => ' .. ...)

local metaHelper = require('module.spotify')
local json = require('library.json')

-- Provides:
-- service::spotify
--      metadata (table) complete metadata as table
local awful = require('awful')

local function emit_info(playerctl_output)
  local metaData = json.parse(playerctl_output) or {}
  metaData.lastPlayDate = os.time()
  metaHelper.ValidateImage(metaData)
  awesome.emit_signal('service::spotify::meta', metaData)
end

local spotify_listener = string.format('%s/.scripts/services/spotify-listener.sh', os.getenv('HOME'))

local spotify_script = "sh -c '" .. spotify_listener .. "'"

awful.spawn.with_line_callback(spotify_script, {
  stdout = function(line)
    emit_info(line)
  end
})

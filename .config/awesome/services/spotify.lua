log('Enter Module => ' .. ...)

local json = require('library.json')

-- Provides:
-- service::spotify
--      artist (string)
--      song (string)
--      status (string) [playing | paused | stopped]
--      metadata (table) complete metadata as table
local awful = require('awful')

local function emit_info(playerctl_output)
  local metaData = json.parse(playerctl_output) or {}

  local artist = metaData.artist
  local title = metaData.title
  local status = metaData.status

  awesome.emit_signal('service::spotify', artist, title, status, metaData)
end

local HOME_DIR = os.getenv('HOME')
local spotify_listener = HOME_DIR .. '/.scripts/services/spotify-listener.sh'

local spotify_script = "sh -c '" .. spotify_listener .. "'"

log('=> spawn spotify daemon: ' .. spotify_script)

awful.spawn.with_line_callback(spotify_script, {
  stdout = function(line)
    emit_info(line)
  end
})

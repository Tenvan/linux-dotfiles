log('Enter Module => ' .. ...)

local makeColorTransparent = require('utilities.utils').makeColorTransparent
local json = require('library.json')
local readJson = require('utilities.json').readJsonFile
local writeJson = require('utilities.json').writeJsonFile

-- Declare widgets

-- local function getMetaText(lastMetaData)
--   return '<b>Titel</b>: ' .. lastMetaData.title .. '\n' ..
--     '<b>Künstler</b>: ' .. lastMetaData.artist .. '\n' ..
--     '<b>Album</b>: ' .. lastMetaData.album .. '\n' ..
--     '<b>Disk</b>: ' .. lastMetaData.discNumber .. '\n' ..
--     '<b>Track</b>: ' .. lastMetaData.trackNumber .. '\n' ..
--     '<b>Track Link</b>: ' .. lastMetaData.url .. '\n'
-- end

-- local popup_text_widget = wibox.widget {
--   markup = getMetaText(),
--   widget = wibox.widget.textbox(),
-- }

-- local popup_artimage_widget = wibox.widget {
--   forced_height = dpi(200),
--   forced_width = dpi(200),
--   widget = wibox.widget.imagebox(),
-- }

--- spotify tooltip widget (extended album infos)
local popup = awful.popup {
  ontop = true,
  visible = false,
  widget = {
    {
      markup = '<h1>Bookmarks</h1>',
      widget = wibox.widget.textbox()
    },
    layout = wibox.layout.fixed.vertical,
  },
  valign = 'center',
  border_color = beautiful.popup_border,
  border_width = beautiful.popup_border_width,
  shape = gears.shape.rounded_rect,
}

-- process meta data cache
local function cacheMetaData(meta)
  local stateDir = os.getenv('HOME') .. '/.local/state/awesome'
  local stateDirSpotify = stateDir .. '/spotify'
  awful.spawn.with_shell('mkdir -p ' .. stateDirSpotify)

  log('image cache dir ===> ' .. stateDirSpotify)
  awful.spawn.with_shell('mkdir -p ' .. stateDirSpotify)

  writeJson(stateDirSpotify .. '/lastemetadata.json', meta)
end

-- Subcribe to spotify updates
awesome.connect_signal('service::spotify', function(artist, title, status, meta)
  dump(meta, 'bookmark <- meta data')
  cacheMetaData(meta)
end)

return popup

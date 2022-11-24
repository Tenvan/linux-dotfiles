log('Enter Module => ' .. ...)

local awesomebuttons = require('awesome-buttons.awesome-buttons')
local file = require('utilities.file')
local json = require('library.json')
local makeColorTransparent = require('utilities.utils').makeColorTransparent
local metaHelper = require('widget.spotify.meta')
local readJson = require('utilities.json').readJsonFile
local writeJson = require('utilities.json').writeJsonFile

local bookmarks = {
  {
    artist = 'Michael Marcus Thurner',
    title = 'Kapitel 3 - Unter dem Nabel von Zou Skost - Perry Rhodan Erstauflage 3192',
    status = 'Paused',
    discNumber = 1,
    trackNumber = 3,
    album = 'Unter dem Nabel von Zou Skost [Perry Rhodan Erstauflage 3192 (Ungekürzt)]',
    url = 'https://open.spotify.com/track/09TF6JA1ZGGxbr54PBMkhw',
    trackid = '/com/spotify/track/09TF6JA1ZGGxbr54PBMkhw',
    artUrl = 'https://i.scdn.co/image/ab67616d0000b273b353daa8e93d0b2403315ff7'
  },
  {
    title = 'Track 4 - Der erste Thort - Perry Rhodan - Neo 18',
    artist = 'Michelle Stern',
    trackid = '/com/spotify/track/0HGtuUVTOCVs5Sd4l5uDzN',
    status = 'Playing',
    trackNumber = 4,
    artUrl = 'https://i.scdn.co/image/ab67616d0000b273652b53f2f7f750b4acd2f486',
    url = 'https://open.spotify.com/track/0HGtuUVTOCVs5Sd4l5uDzN',
    discNumber = 1,
    album = 'Der erste Thort [Perry Rhodan - Neo 18 (Ungekürzt)]'
  },
  {
    title = 'Track 115 - Der Administrator - Perry Rhodan - Neo 17',
    artist = 'Frank Borsch',
    trackid = '/com/spotify/track/62oOO3ZEtxxbZyOxJGN644',
    status = 'Playing',
    trackNumber = 115,
    artUrl = 'https://i.scdn.co/image/ab67616d0000b2731b1e172d3854ff6099b83894',
    url = 'https://open.spotify.com/track/62oOO3ZEtxxbZyOxJGN644',
    discNumber = 1,
    album = 'Der Administrator [Perry Rhodan - Neo 17 (Ungekürzt)]'
  }
}

local function mergeMetaToBookmarks(meta)
  for i = 1, #bookmarks do
    local bookmark = bookmarks[i]
    if bookmark.album == meta.album then
      bookmarks[i] = meta
      dump(bookmarks, 'replaced bookmarks', 1)
      return
    end
  end

  table.insert(bookmarks, meta)
  dump(bookmarks, 'added bookmark', 1)
end

-- Subcribe to spotify updates
awesome.connect_signal('service::spotify', function(meta)
  dump(meta, 'bookmark <- meta data')
  mergeMetaToBookmarks(meta)
end)

--- spotify tooltip widget (extended album infos)

local function createBookmarkPopups()
  local bookmarkList = {
    layout = wibox.layout.fixed.vertical,
    {
      markup = '<span size="x-large">Bookmarks</span>',
      widget = wibox.widget.textbox()
    },
  }
  for i = 1, #bookmarks do
    local meta = bookmarks[i]
    local item = wibox.widget {
      markup = metaHelper.GetSmallMetaText(meta),
      widget = wibox.widget.textbox()
    }

    local popup_artimage_widget = wibox.widget {
      forced_height = dpi(96),
      forced_width = dpi(96),
      widget = wibox.widget.imagebox(),
    }

    local imagePath = metaHelper.GetImagePath(meta)
    if file.file_exists(imagePath) then
      popup_artimage_widget.image = imagePath
    else
      metaHelper.ValidateImage(meta)
    end

    table.insert(bookmarkList, {
      popup_artimage_widget,
      item,
      layout = wibox.layout.fixed.horizontal,
    })
  end

  -- Declare widgets
  local bookmarks_widget = awful.popup {
    ontop = true,
    widget = bookmarkList,
    visible = false,
    valign = 'top',
    border_color = beautiful.popup_border,
    border_width = beautiful.popup_border_width,
    forced_height = dpi(600),
    forced_width = dpi(800),
    shape = gears.shape.rounded_rect,
  }
  return bookmarks_widget
end

return createBookmarkPopups

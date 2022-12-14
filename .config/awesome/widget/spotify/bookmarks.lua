log('Enter Module => ' .. ...)

local awesomebuttons = require('awesome-buttons.awesome-buttons')
local clickable_container = require('widget.clickable-container')
local file = require('utilities.file')
local makeColorTransparent = require('utilities.utils').makeColorTransparent
local metaHelper = require('module.spotify')
local find_clients = require('helpers').find_clients

local currentBookmarks = {}

-- emitter
local function remove_bookmark(meta)
  log('widget::spotify-bookmarks => service::spotify::bookmarks::remove : ' .. meta.title)
  emit('service::spotify::bookmarks::remove', meta)
end

local function createBookmarkList(bookmarks)
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
      {
        markup = metaHelper.GetSmallMetaText(meta),
        widget = wibox.widget.textbox()
      },
      margins = dpi(2),
      widget = wibox.container.margin
    }

    local popup_artimage_widget = wibox.widget {
      {
        forced_height = dpi(96),
        forced_width = dpi(96),
        widget = wibox.widget.imagebox(),
      },
      margins = dpi(2),
      widget = wibox.container.margin
    }

    local imagePath = metaHelper.GetImagePath(meta)
    if file.file_exists(imagePath) then
      popup_artimage_widget.widget.image = imagePath
    else
      metaHelper.CacheMetaImage(meta)
    end

    local open_button = wibox.widget {
      {
        align = 'center',
        text = '',
        font = 'sans 20',
        widget = wibox.widget.textbox(),
        forced_width = dpi(48),
      },
      widget = clickable_container
    }

    open_button:buttons(
      gears.table.join(
        awful.button(
          {},
          1,
          nil,
          function()
            local spotify = find_clients({ class = 'Spotify' }, true)
            if spotify ~= nil then
              spotify:jump_to()
            else
              awful.spawn('spotify')
            end

            local cmd = string.format('playerctl open --player=spotify %s', meta.url)
            log('play command: ' .. cmd)
            -- awful.spawn(string.format('playerctl open open spotify:track:%s', metaHelper.GetTrackId(meta)))
            awful.spawn(cmd)
          end
        )
      )
    )

    local delete_button = wibox.widget {
      {
        align = 'center',
        text = '',
        font = 'sans 24',
        widget = wibox.widget.textbox(),
        forced_width = dpi(48),
      },
      widget = clickable_container
    }

    delete_button:buttons(
      gears.table.join(
        awful.button(
          {},
          1,
          nil,
          function()
            log('remove bookmark: ' .. meta.title)
            remove_bookmark(meta)
          end
        )
      )
    )

    local listItem = wibox.widget {
      popup_artimage_widget,
      item,
      {
        open_button,
        delete_button,
        layout = wibox.layout.align.horizontal,
      },
      layout = wibox.layout.align.horizontal,
    }

    table.insert(bookmarkList, listItem)
  end

  return bookmarkList
end

local function createPopup()
  local container = wibox.widget {
    createBookmarkList(currentBookmarks),
    strategy = 'max',
    height = dpi(600),
    widget = wibox.container.constraint,
  }

  local popup = awful.popup {
    widget = container,
    ontop = true,
    visible = false,
    valign = 'top',
    border_color = beautiful.popup_border,
    border_width = beautiful.popup_border_width,
    shape = gears.shape.rounded_rect,
  }

  return popup
end

-- Subcribe to spotify updates
connect('service::spotify::bookmarks', function(bookmarks)
  log('widget::spotify-bookmarks <- bookmarks: ' .. tostring(#bookmarks))
  currentBookmarks = bookmarks
end)

return createPopup

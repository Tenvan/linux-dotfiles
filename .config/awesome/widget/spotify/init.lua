log('Enter Module => ' .. ...)

local awesomebuttons = require('awesome-buttons.awesome-buttons')
local bookmarkPopups = require('widget.spotify.bookmarks')
local clickable_container = require('widget.clickable-container')
local file = require('utilities.file')
local makeColorTransparent = require('utilities.utils').makeColorTransparent
local metaHelper = require('module.spotify')
local myButton = require('module.buttons')

local spotify_artist = wibox.widget.textbox()
local spotify_title = wibox.widget.textbox()

local bookmarksWidget = nil
local lastMetaData = metaHelper.emptyMetaData

local TOGGLE_MPD_CMD = 'playerctl play-pause'
local PLAY_MPD_CMD = 'playerctl play'
local PAUSE_MPD_CMD = 'playerctl pause'
local NEXT_MPD_CMD = 'playerctl next'
local PREV_MPD_CMD = 'playerctl previous'
local START_SPOTIFY_CMD = 'spotify'
local OPEN_SPOTIFY_TRACK_CMD = 'playerctl open'

-- create cache folder, if not exists
local cacheDirSpotify = metaHelper.GetCachePath()
file.ensureDir(cacheDirSpotify)

local button = myButton {
    text = 'button'
}

local status_button = wibox.widget {
    {
        align = 'center',
        text = metaHelper.stoppedText,
        font = 'sans 14',
        widget = wibox.widget.textbox(),
        forced_width = dpi(24),
    },
    widget = clickable_container
}

status_button:buttons(
    gears.table.join(
        awful.button(
            {},
            1,
            nil,
            function()
                awful.spawn(TOGGLE_MPD_CMD)
            end
        )
    )
)

local prev_button = wibox.widget {
    {
        align = 'center',
        text = metaHelper.prevText,
        font = 'sans 14',
        widget = wibox.widget.textbox(),
        forced_width = dpi(24),
    },
    widget = clickable_container
}

prev_button:buttons(
    gears.table.join(
        awful.button(
            {},
            1,
            nil,
            function()
                awful.spawn(PREV_MPD_CMD)
            end
        )
    )
)

local next_button = wibox.widget {
    {
        align = 'center',
        text = metaHelper.nextText,
        font = 'sans 14',
        widget = wibox.widget.textbox(),
        forced_width = dpi(24),
    },
    widget = clickable_container
}

next_button:buttons(
    gears.table.join(
        awful.button(
            {},
            1,
            nil,
            function()
                awful.spawn(NEXT_MPD_CMD)
            end
        )
    )
)

local bookmark_button = myButton {
    text = ' ',
    onLeftClick = function()
        if bookmarksWidget ~= nil then
            bookmarksWidget.visible = false
            bookmarksWidget = nil
        else
            bookmarksWidget = bookmarkPopups()
            bookmarksWidget:move_next_to(mouse.current_widget_geometry)
            bookmarksWidget.visible = true
        end
    end
}

-- Main widget that includes all others
local widget = wibox.widget {
    -- Status Icon
    prev_button,
    status_button,
    next_button,
    bookmark_button,
    -- Title widget
    {
        align = 'center',
        text = '---',
        font = 'sans 12',
        widget = spotify_title
    },
    spacing = dpi(1),
    layout = wibox.layout.fixed.horizontal
}

local popup_text_widget = wibox.widget {
    markup = metaHelper.GetLargeMetaText(lastMetaData),
    widget = wibox.widget.textbox(),
}

local popup_artimage_widget = wibox.widget {
    forced_height = dpi(200),
    forced_width = dpi(200),
    widget = wibox.widget.imagebox(),
}

--- spotify tooltip widget (extended album infos)
local popup = awful.popup {
    ontop = true,
    visible = false,
    widget = {
        {
            {
                {
                    {
                        widget = popup_artimage_widget,
                    },
                    margins = dpi(10),
                    widget = wibox.container.margin
                },
                {
                    widget = popup_text_widget,
                },
                forced_height = dpi(220),
                forced_width = dpi(1000),
                spacing = dpi(10),
                valign = 'center',
                layout = wibox.layout.fixed.horizontal,
            },
            bg     = makeColorTransparent(beautiful.spotify_bg, '80'),
            font   = beautiful.font,
            fg     = beautiful.fg,
            widget = wibox.container.background,
        },
        layout = wibox.layout.fixed.vertical,
    },
    valign = 'center',
    border_color = beautiful.popup_border,
    border_width = beautiful.popup_border_width,
    shape = gears.shape.rounded_rect,
}

local function refreshPopup()
    if bookmarksWidget ~= nil then

        local coords = { x = bookmarksWidget.x, y = bookmarksWidget.y }

        bookmarksWidget.visible = false
        bookmarksWidget = nil
        bookmarksWidget = bookmarkPopups()

        bookmarksWidget.x = coords.x
        bookmarksWidget.y = coords.y
        bookmarksWidget.visible = true
    end
end

spotify_title:connect_signal(
    'mouse::enter',
    function(w)
        popup:move_next_to(mouse.current_widget_geometry)
        popup.visible = true
    end
)

spotify_title:connect_signal(
    'mouse::leave',
    function(w)
        popup.visible = false
    end
)

-- Subcribe to spotify updates
awesome.connect_signal('service::spotify::meta', function(meta)
    meta.lastPlayDate = os.time()
    spotify_artist.text = meta.artist
    spotify_title.text = meta.title

    lastMetaData = meta


    local imageFile = metaHelper.GetImagePath(meta)
    if file.file_exists(imageFile) then
        popup_artimage_widget.image = metaHelper.GetImagePath(meta)
    end
    popup_text_widget.markup = metaHelper.GetLargeMetaText(meta)
end)

-- Subcribe to spotify updates
awesome.connect_signal('service::spotify::status', function(status)
    log('widget::spotify <- status: ' .. status)
    status_button.widget.text = status
end)

-- Subcribe to bookmarks updates
awesome.connect_signal('service::spotify::bookmarks', function(bookmarks)
    log('widget::spotify-bookmarks <- bookmarks: ' .. tostring(#bookmarks))
    refreshPopup()
end)

-- Subcribe to imageupdates
awesome.connect_signal('service::spotify::image', function(image)
    log('widget::spotify <- image: ' .. image)
    refreshPopup()
end)

return widget

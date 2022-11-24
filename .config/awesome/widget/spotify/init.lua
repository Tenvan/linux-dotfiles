log('Enter Module => ' .. ...)

local makeColorTransparent = require('utilities.utils').makeColorTransparent
local awesomebuttons = require('awesome-buttons.awesome-buttons')

local spotify_artist = wibox.widget.textbox()
local spotify_title = wibox.widget.textbox()

local createBookmarkPopups = require('widget.spotify.bookmarks')
local metaHelper = require('widget.spotify.meta')

local bookmarksWidget = nil
local lastMetaData = metaHelper.emptyMetaData

-- create cache folder, if not exists
local cacheDirSpotify = metaHelper.GetCachePath()
awful.spawn.with_shell('mkdir -p ' .. cacheDirSpotify)

local status_button = wibox.widget {
    align = 'center',
    text = metaHelper.stoppedText,
    font = 'sans 14',
    widget = wibox.widget.textbox(),
    forced_width = dpi(32)
}

local bookmark_button = awesomebuttons.with_text {
    text = ' ',
    onclick = function()
        if bookmarksWidget ~= nil then
            bookmarksWidget.visible = false
            bookmarksWidget = nil
        else
            bookmarksWidget = createBookmarkPopups()
            bookmarksWidget:move_next_to(mouse.current_widget_geometry)
            bookmarksWidget.visible = true
        end
    end
}

-- Main widget that includes all others
local widget = wibox.widget {
    -- Status Icon
    status_button,
    -- Title widget
    {
        align = 'center',
        text = '---',
        font = 'sans 12',
        widget = spotify_title
    },
    bookmark_button,
    spacing = dpi(4),
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
awesome.connect_signal('service::spotify', function(meta)
    -- close bookmarks when received new metadata
    if bookmarksWidget ~= nil then
        bookmarksWidget.visible = false
        bookmarksWidget = nil
    end

    dump(meta, 'spotify widget <- meta data')
    local status = meta.status

    lastMetaData = meta
    metaHelper.CacheMetaData(meta)

    local status_text = metaHelper.stoppedText
    if status == 'Paused' then
        status_text = metaHelper.pausedText
    elseif status == 'Playing' then
        status_text = metaHelper.playingText
    end

    status_button.text = status_text
    spotify_artist.text = meta.artist
    spotify_title.text = meta.title

    popup_artimage_widget.image = metaHelper.GetImagePath(meta)
    popup_text_widget.markup = metaHelper.GetLargeMetaText(meta)
end)

return widget

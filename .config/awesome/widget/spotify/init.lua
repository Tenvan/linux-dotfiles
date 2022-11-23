log('Enter Module => ' .. ...)

local makeColorTransparent = require('utilities.utils').makeColorTransparent

-- Declare widgets
local spotify_status = wibox.widget.textbox()
local spotify_artist = wibox.widget.textbox()
local spotify_title = wibox.widget.textbox()

local pausedText = ''
local stoppedText = ''
local playingText = ''

local bookmarks_widget = require("widget.spotify.bookmarks")

local lastMetaData = {
    artist = '--',
    title = '--',
    status = stoppedText,
    discNumber = '--',
    trackNumber = '--',
    album = '--',
    url = '--',
    trackid = '--',
    artUrl = '--',
}

-- Main widget that includes all others
local widget = wibox.widget {
    -- Status Icon
    {
        align = 'center',
        text = stoppedText,
        font = 'sans 14',
        widget = spotify_status,
        forced_width = dpi(32)
    },
    -- Title widget
    {
        align = 'center',
        text = '---',
        font = 'sans 12',
        widget = spotify_title
    },
    spacing = dpi(4),
    layout = wibox.layout.fixed.horizontal
}

local function getMetaText()
    return '<span size="x-large">Titel: ' .. lastMetaData.title .. '</span>\n' ..
      '<b>Künstler</b>: ' .. lastMetaData.artist .. '\n' ..
      '<b>Album</b>: ' .. lastMetaData.album .. '\n' ..
      '<b>Disk</b>: ' .. lastMetaData.discNumber .. '\n' ..
      '<b>Track</b>: ' .. lastMetaData.trackNumber .. '\n' ..
      '<b>Track Link</b>: ' .. lastMetaData.url .. '\n'
end

local popup_text_widget = wibox.widget {
    markup = getMetaText(),
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

widget:connect_signal(
    'mouse::enter',
    function(w)
        popup:move_next_to(mouse.current_widget_geometry)
        popup.visible = true
    end
)

widget:connect_signal(
    'mouse::leave',
    function(w)
        popup.visible = false
    end
)

-- process meta data cache
local function cacheMetaData(meta)
    local cacheDir = os.getenv('HOME') .. '/.cache/awesome'
    local cacheDirSpotify = cacheDir .. '/com/spotify/track'

    log('image cache dir ===> ' .. cacheDirSpotify)
    awful.spawn.with_shell('mkdir -p ' .. cacheDirSpotify)

    local artCacheFile = cacheDir .. meta.trackid .. '.jpeg'
    log('image cache file ===> ' .. artCacheFile)

    local get_art_script = 'curl -sf ' .. meta.artUrl .. ' --output ' .. artCacheFile
    awful.spawn.easy_async_with_shell(get_art_script, function()
        log("link '" .. meta.artUrl .. "' fetched")
        popup_artimage_widget.image = artCacheFile
    end)
end

-- Subcribe to spotify updates
awesome.connect_signal('service::spotify', function(artist, title, status, meta)
    dump(meta, 'spotify widget <- meta data')

    lastMetaData = meta
    cacheMetaData(meta)

    if status == 'Paused' then
        spotify_status.text = pausedText
    elseif status == 'Playing' then
        spotify_status.text = playingText
    else
        spotify_status.text = stoppedText
    end

    spotify_artist.text = artist
    spotify_title.text = title

    popup_text_widget.markup = getMetaText()
end)

return widget

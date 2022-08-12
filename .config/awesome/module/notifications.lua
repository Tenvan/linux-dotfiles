log('Enter Module => ' .. ...)

local gears = require('gears')
local wibox = require('wibox')
local awful = require('awful')
local ruled = require('ruled')
local naughty = require('naughty')
local menubar = require('menubar')
local beautiful = require('beautiful')

local dpi = beautiful.xresources.apply_dpi
local makeColorTransparent = require('utilities.utils').makeColorTransparent

local clickable_container = require('widget.clickable-container')
local iconUtils = require('utilities.icon-utils')

-- Defaults
naughty.config.defaults.ontop = true
naughty.config.defaults.icon_size = beautiful.notification_icon_size
naughty.config.defaults.timeout = 5
naughty.config.defaults.implicit_timeout = 5
naughty.config.defaults.title = 'System Notification'
naughty.config.defaults.margin = dpi(16)
naughty.config.defaults.border_width = beautiful.tooltip_border_width
naughty.config.defaults.position = beautiful.notification_position
naughty.config.defaults.shape = function(cr, w, h)
  gears.shape.rounded_rect(cr, w, h, dpi(6))
end

-- Apply theme variables
naughty.config.padding = dpi(8)
naughty.config.spacing = dpi(8)
naughty.config.icon_dirs = {}
naughty.config.icon_formats = { 'svg', 'png', 'jpg', 'gif' }


-- Presets / rules

ruled.notification.connect_signal(
  'request::rules',
  function()
    -- log('spawn::request::rules -> module/notifications.lua')
    -- Critical notifs
    ruled.notification.append_rule {
      rule       = { urgency = 'critical' },
      properties = {
        font             = beautiful.font_bold,
        bg               = beautiful.error_bg,
        fg               = beautiful.error_fg,
        margin           = dpi(16),
        position         = 'top_right',
        implicit_timeout = 15
      }
    }

    -- Normal notifs
    ruled.notification.append_rule {
      rule       = { urgency = 'normal' },
      properties = {
        font             = beautiful.font,
        bg               = beautiful.transparent,
        fg               = beautiful.fg_normal,
        margin           = dpi(16),
        implicit_timeout = 10
      }
    }

    -- Low notifs
    ruled.notification.append_rule {
      rule       = { urgency = 'low' },
      properties = {
        font             = beautiful.font,
        bg               = beautiful.transparent,
        fg               = beautiful.fg_normal,
        margin           = dpi(16),
        implicit_timeout = 5,
        icon_size        = 200
      }
    }

    -- Spotify notifs
    ruled.notification.append_rule {
      rule       = { app_name = 'Spotify' },
      properties = {
        font             = beautiful.font,
        bg               = makeColorTransparent(beautiful.spotify_bg, '80'),
        fg               = beautiful.fg,
        implicit_timeout = 20,
        margin           = dpi(16),
        icon_size        = dpi(200)
      }
    }

    -- Teams notifs
    ruled.notification.append_rule {
      rule       = { app_name = 'teams-for-linux' },
      properties = {
        font             = beautiful.font,
        bg               = makeColorTransparent(beautiful.teams_bg, '80'),
        fg               = beautiful.fg,
        implicit_timeout = 20,
        margin           = dpi(16),
        icon_size        = dpi(200)
      }
    }

    -- Vivaldi notifs
    ruled.notification.append_rule {
      rule       = { app_name = 'vivaldi-stable' },
      properties = {
        font             = beautiful.font,
        bg               = makeColorTransparent(beautiful.error_bg, '70'),
        fg               = beautiful.fg,
        implicit_timeout = 20,
        margin           = dpi(16),
        icon_size        = dpi(64)
      }
    }
  end
)

-- Error handling
naughty.connect_signal(
  'request::display_error',
  function(message, startup)
    -- log('spawn::request::display_error -> module/notifications.lua')
    naughty.notification {
      urgency  = 'critical',
      title    = 'Oops, an error happened' .. (startup and ' during startup!' or '!'),
      message  = message,
      app_name = 'System Notification',
      icon     = 'face-worried'
    }
  end
)

-- XDG icon lookup
naughty.connect_signal(
  'request::icon',
  function(n, context, hints)
    -- log('spawn::request::icon> module/notifications.lua')
    if context ~= 'app_icon' then return end

    local path = menubar.utils.lookup_icon(hints.app_icon) or
      menubar.utils.lookup_icon(hints.app_icon:lower())

    if path then
      n.icon = path
    end
  end
)

-- Connect to naughty on display signal
naughty.connect_signal(
  'request::display',
  function(n)
    -- log('spawn::request::display -> module/notifications.lua')

    -- dump({
    --   text = n.text,
    --   icon = n.icon,
    --   timeout = n.timeout,
    --   title = n.title,
    --   app_name = n.app_name,
    --   urgency = n.urgency,
    --   icon_size = n.icon_size,
    -- }, 'notification', 1)

    local path = iconUtils.lookup_icon(n.icon)
    n.icon = path


    -- Actions Blueprint
    local actions_template = wibox.widget {
      notification = n,
      base_layout = wibox.widget {
        spacing = dpi(0),
        layout  = wibox.layout.flex.horizontal
      },
      widget_template = {
        {
          {
            {
              {
                id     = 'text_role',
                font   = beautiful.font_small,
                widget = wibox.widget.textbox
              },
              widget = wibox.container.place
            },
            widget = clickable_container
          },
          bg            = beautiful.groups_bg,
          shape         = gears.shape.rounded_rect,
          forced_height = dpi(30),
          widget        = wibox.container.background
        },
        margins = dpi(4),
        widget  = wibox.container.margin
      },
      style = { underline_normal = false, underline_selected = true },
      widget = naughty.list.actions
    }

    -- Notifbox Blueprint
    naughty.layout.box {
      notification = n,
      type = 'notification',
      screen = 1, --awful.screen.preferred(),
      shape = gears.shape.rectangle,
      widget_template = {
        {
          {
            {
              {
                {
                  {
                    {
                      {
                        {
                          {
                            markup = n.app_name or 'System Notification',
                            font = beautiful.font,
                            align = 'center',
                            valign = 'center',
                            widget = wibox.widget.textbox

                          },
                          margins = beautiful.notification_margin,
                          widget  = wibox.container.margin,
                        },
                        bg     = beautiful.background,
                        widget = wibox.container.background,
                      },
                      {
                        {
                          {
                            -- resize_strategy = 'scale',
                            widget = naughty.widget.icon,
                          },
                          margins = beautiful.notification_margin,
                          widget  = wibox.container.margin,
                        },
                        {
                          {
                            layout = wibox.layout.align.vertical,
                            expand = 'none',
                            nil,
                            {
                              {
                                align = 'left',
                                widget = naughty.widget.title
                              },
                              {
                                align = 'left',
                                widget = naughty.widget.message,
                              },
                              layout = wibox.layout.fixed.vertical
                            },
                            nil
                          },
                          margins = beautiful.notification_margin,
                          widget  = wibox.container.margin,
                        },
                        layout = wibox.layout.fixed.horizontal,
                      },
                      fill_space = true,
                      spacing    = beautiful.notification_margin,
                      layout     = wibox.layout.fixed.vertical,
                    },
                    -- Margin between the fake background
                    -- Set to 0 to preserve the 'titlebar' effect
                    margins = dpi(0),
                    widget  = wibox.container.margin,
                  },
                  bg     = beautiful.transparent,
                  widget = wibox.container.background,
                },
                -- Actions
                actions_template,
                spacing = dpi(4),
                layout  = wibox.layout.fixed.vertical,
              },
              bg     = beautiful.transparent,
              id     = 'background_role',
              widget = naughty.container.background,
            },
            strategy = 'min',
            width    = dpi(160),
            widget   = wibox.container.constraint,
          },
          strategy = 'max',
          width    = beautiful.notification_max_width or dpi(500),
          widget   = wibox.container.constraint
        },
        bg = beautiful.background,
        shape = gears.shape.rounded_rect,
        widget = wibox.container.background
      }
    }

    -- Destroy popups if dont_disturb mode is on
    -- Or if the right_panel is visible
    local focused = awful.screen.focused()
    if _G.dont_disturb or
      (focused.right_panel and focused.right_panel.visible) then
      naughty.destroy_all_notifications(nil, 1)
    end
  end
)

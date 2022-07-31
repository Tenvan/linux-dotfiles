log('Enter Module => ' .. ...)

local beautiful = require('beautiful')
local wibox = require('wibox')
local awful = require('awful')
local gears = require('gears')

local dpi = beautiful.xresources.apply_dpi

local weather_curl_widget = require('awesome-wm-widgets.weather-widget.weather')
local config = require('configuration.config').widget

local clickable_container = require('widget.clickable-container')

local widget = wibox.widget {
  {
    {
      text = 'ColorPalette',
      widget = wibox.widget.textbox
    },
    -- wibox.widget.textclock(),
    margins = dpi(5),
    widget = wibox.container.margin
  },
  id = 'button_role',
  bg = '#ff000070',
  widget = wibox.container.background
}

widget.widget_palette_popup = awful.popup {
  type              = 'dropdown_menu',
  visible           = false,
  ontop             = true,
  input_passthrough = true,

  widget = require('widget.color-palette'),

  border_color = '#00ff00',
  border_width = 5,

  shape = gears.shape.rounded_rect,

  placement = function(c)
    local p = awful.placement.bottom_right(c, { parent = widget })
    dump(p, 'placement color popup')
  end,
}

widget:connect_signal(
  'mouse::enter',
  function(w)
    awful.placement.next_to(w.widget_palette_popup)
    w.widget_palette_popup.visible = true
  end
)

widget:connect_signal(
  'mouse::leave',
  function(w)
    w.widget_palette_popup.visible = false
  end
)

return widget

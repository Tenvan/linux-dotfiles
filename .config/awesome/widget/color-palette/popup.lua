log('Enter Module => ' .. ...)

local beautiful = require('beautiful')
local wibox = require('wibox')
local awful = require('awful')
local gears = require('gears')

local dpi = beautiful.xresources.apply_dpi

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

local popup = awful.popup {
  type              = 'dropdown_menu',
  visible           = false,
  ontop             = true,
  input_passthrough = true,

  widget = require('widget.color-palette'),

  border_color = '#00ff00',
  border_width = 5,

  shape = gears.shape.rounded_rect,
}

widget:buttons(
  awful.util.table.join(
    awful.button({}, 1, function()
      popup.visible = not popup.visible
      if popup.visible then
        popup:move_next_to(mouse.current_widget_geometry)
      end
    end)
  )
)

widget:connect_signal(
  'mouse::enter',
  function(w)
    -- awful.placement.next_to(w.widget_palette_popup)
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

return widget

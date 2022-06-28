local awful = require('awful')
local wibox = require('wibox')
local gears = require('gears')
local beautiful = require('beautiful')
local icons = require('theme.icons')
local clickable_container = require('widget.clickable-container')

local dpi = beautiful.xresources.apply_dpi

return function(_, panel)
  return wibox.widget {
    {
      {
        layout = wibox.layout.fixed.vertical,
        spacing = dpi(7),
        -- require('layout.left-panel.dashboard.hardware-monitor'),
        -- require('layout.left-panel.dashboard.quick-settings'),
        {
          layout = wibox.layout.fixed.vertical,
          {
            {
              wibox.widget {
                text = 'Quick Settings',
                font = 'Inter Regular 12',
                align = 'left',
                valign = 'center',
                widget = wibox.widget.textbox
              },
              left = dpi(24),
              right = dpi(24),
              widget = wibox.container.margin
            },
            forced_height = dpi(35),
            bg = beautiful.groups_title_bg,
            shape = function(cr, width, height)
              gears.shape.partially_rounded_rect(cr, width, height, true, true, false, false,
                beautiful.groups_radius)
            end,
            widget = wibox.container.background
          }
        }

      },
      nil,
      require('widget.end-session')(),
      layout = wibox.layout.align.vertical
    },
    margins = dpi(16),
    widget = wibox.container.margin
  }
end

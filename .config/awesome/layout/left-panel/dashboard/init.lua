local log = require('utilities.debug').log
local dump = require('utilities.debug').dump
log("Enter Module => " .. ... )

local wibox = require('wibox')
local beautiful = require('beautiful')

local dpi = beautiful.xresources.apply_dpi

return function(_, panel)
  return wibox.widget {
    {
      {
        require('layout.left-panel.dashboard.hardware-monitor'),
        require('layout.left-panel.dashboard.quick-settings'),
        spacing = dpi(7),
        layout = wibox.layout.fixed.vertical,
      },
      nil,
      require('widget.end-session')(),
      layout = wibox.layout.align.vertical
    },
    margins = dpi(16),
    widget = wibox.container.margin
  }
end

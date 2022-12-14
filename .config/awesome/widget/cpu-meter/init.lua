local log = require('utilities.debug').log
local dump = require('utilities.debug').dump
log('Enter Module => ' .. ...)

local wibox = require('wibox')
local gears = require('gears')
local beautiful = require('beautiful')
local watch = require('awful.widget.watch')
local dpi = beautiful.xresources.apply_dpi
local icons = require('theme.icons')

local slider = wibox.widget {
  nil,
  {
    id               = 'cpu_usage',
    max_value        = 100,
    value            = 29,
    forced_height    = dpi(10),
    color            = beautiful.fg_normal,
    background_color = beautiful.groups_bg,
    shape            = gears.shape.rounded_rect,
    widget           = wibox.widget.progressbar
  },
  nil,
  expand = 'none',
  layout = wibox.layout.align.vertical
}

connect('service::cpu',
  function(total, diff_idle, diff_total, diff_usage,
           user, nice, system, idle, iowait, irq, softirq, steal, guest, guest_nice)
    --      total, diff_idle, diff_total, diff_usage,
    --      user, nice, system, idle, iowait, irq, softirq, steal, guest, guest_nice
    slider.cpu_usage:set_value(diff_usage)

    collectgarbage('collect')
  end)


local cpu_meter = wibox.widget {
  {
    {
      {
        image = icons.chart,
        resize = true,
        widget = wibox.widget.imagebox
      },
      top = dpi(12),
      bottom = dpi(12),
      widget = wibox.container.margin
    },
    slider,
    spacing = dpi(24),
    layout = wibox.layout.fixed.horizontal
  },
  left = dpi(24),
  right = dpi(24),
  forced_height = dpi(48),
  widget = wibox.container.margin
}

return cpu_meter

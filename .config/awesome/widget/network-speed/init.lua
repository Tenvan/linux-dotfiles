log('Enter Module => ' .. ...)

local bar_width = dpi(9)
local bar_spacing = dpi(1)

local icon = require('widget.icon')
local seperator = require('widget.seperator')

local function convert_to_h(bytes)
  local speed
  local dim
  local bits = bytes * 8
  if bits < 1000 then
    speed = bits
    dim = 'b/s'
  elseif bits < 1000000 then
    speed = bits / 1000
    dim = 'kb/s'
  elseif bits < 1000000000 then
    speed = bits / 1000000
    dim = 'mb/s'
  elseif bits < 1000000000000 then
    speed = bits / 1000000000
    dim = 'gb/s'
  else
    speed = tonumber(bits)
    dim = 'b/s'
  end
  return math.floor(speed + 0.5) .. dim
end

local speed_text_rx = wibox.widget {
  id = 'rx_speed',
  forced_width = dpi(60),
  align = 'right',
  widget = wibox.widget.textbox
}

local speed_text_tx = wibox.widget {
  id = 'tx_speed',
  forced_width = dpi(60),
  align = 'right',
  widget = wibox.widget.textbox
}

local chart_rx = wibox.widget {
  id               = 'chart_role_rx',
  max_value        = 1 * 1024 * 1024,
  stack            = false,
  scale            = true,
  background_color = beautiful.transparent,
  stack_colors     = {
    beautiful.xres_vars.color2,
  },
  forced_width     = dpi(50),
  step_width       = dpi(1),
  step_spacing     = 0,
  widget           = wibox.widget.graph
}

local chart_tx = wibox.widget {
  id               = 'chart_role_tx',
  max_value        = 1 * 1024 * 1024,
  stack            = false,
  scale            = true,
  background_color = beautiful.transparent,
  stack_colors     = {
    beautiful.xres_vars.color3,
  },
  forced_width     = dpi(50),
  step_width       = dpi(1),
  step_spacing     = 0,
  widget           = wibox.widget.graph
}

local current_speed = wibox.widget {
  spacing = bar_spacing,
  icon({ icon = '⬇️' }),
  speed_text_rx,
  chart_rx,
  seperator,
  icon({ icon = '⬆️' }),
  speed_text_tx,
  chart_tx,
  layout = wibox.layout.fixed.horizontal,
}

local widget = wibox.widget {
  current_speed,
  -- forced_width = dpi(hardware.core_count * (bar_width + bar_spacing)),
  widget = wibox.container.background
}

connect('service::network', function(speed_rx, speed_tx)
  speed_text_rx:set_text(convert_to_h(speed_rx))
  speed_text_tx:set_text(convert_to_h(speed_tx))

  chart_rx:add_value(speed_rx)
  chart_tx:add_value(speed_tx)
end)

return {
  widget = widget
}

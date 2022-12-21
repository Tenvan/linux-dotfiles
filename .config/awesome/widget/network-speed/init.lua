log('Enter Module => ' .. ...)

local bar_width = dpi(9)
local bar_spacing = dpi(1)

local icon = require('widget.icon')
local seperator = require('widget.seperator')

local bar_color = gears.color({
  type = 'linear',
  from = { 0, 0 },
  to = { 40, 0 },
  stops = {
    { 0, beautiful.xres_vars.color2 },
    { 0.4, beautiful.xres_vars.color3 },
    { 0.9, beautiful.xres_vars.color5 },
    { 1, beautiful.xres_vars.color1 },
  }
})

local function create_speed_col(id)
  return wibox.widget {
    wibox.widget {
      id               = id,
      max_value        = 100,
      value            = 25,
      direction        = 'north',
      background_color = beautiful.transparent,
      color            = bar_color,
      widget           = wibox.widget.progressbar
    },
    forced_width = bar_width,
    direction    = 'east',
    widget       = wibox.container.rotate
  }
end

local speed_bar_rx = create_speed_col('speed_bar_rx')
local speed_bar_tx = create_speed_col('speed_bar_tx')

local chart_rx = wibox.widget {
  id               = 'chart_role_rx',
  max_value        = 1 * 1024 * 1024,
  stack            = false,
  scale            = true,
  background_color = beautiful.transparent,
  stack_colors     = {
    beautiful.xres_vars.color2,
  },
  forced_width     = dpi(40),
  step_width       = dpi(2),
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
  forced_width     = dpi(40),
  step_width       = dpi(2),
  step_spacing     = 0,
  widget           = wibox.widget.graph
}

local current_speed = wibox.widget {
  spacing = bar_spacing,
  speed_bar_rx,
  seperator,
  speed_bar_tx,
  seperator,
  chart_rx,
  seperator,
  chart_tx,
  layout = wibox.layout.fixed.horizontal,
}

local widget = wibox.widget {
  current_speed,
  -- forced_width = dpi(hardware.core_count * (bar_width + bar_spacing)),
  widget = wibox.container.background
}

local prev_speed_rx = -1
local prev_speed_tx = -1

connect('service::network', function(speed_rx, speed_tx)
  local diff_rx = math.abs((speed_rx - prev_speed_rx) / speed_rx * 100)
  local diff_tx = math.abs((speed_tx - prev_speed_tx) / speed_tx * 100)

  speed_bar_rx.widget:set_value(diff_rx)
  speed_bar_tx.widget:set_value(diff_tx)

  if not (prev_speed_rx == -1) then
    chart_rx:add_value(speed_rx)
    chart_tx:add_value(speed_tx)
  end

  prev_speed_rx = speed_rx
  prev_speed_tx = speed_tx
end)

return {
  widget = widget
}

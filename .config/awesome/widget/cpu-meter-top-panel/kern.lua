local bar_width = dpi(9)
local bar_spacing = dpi(1)

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

local cpu_cols = {
  spacing = bar_spacing,
  layout = wibox.layout.fixed.horizontal,
}

local function create_cpu_col()
  return wibox.widget {
    {
      id               = 'progress_role',
      max_value        = 100,
      value            = 0,
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

for i = 1, hardware.core_count do
  cpu_cols[i] = create_cpu_col()
end

local widget = wibox.widget {
  cpu_cols,
  forced_width = dpi(hardware.core_count * (bar_width + bar_spacing)),
  widget = wibox.container.background
}

connect('service::cpu', function(core_usage)
  for i = 1, hardware.core_count do
    local progress = cpu_cols[i]:get_children_by_id('progress_role')[1]
    progress:set_value(core_usage[i])
  end
end)

return {
  widget = widget
}

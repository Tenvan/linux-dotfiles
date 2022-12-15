local function create_cpu_col()
  return wibox.widget {
    {
      id               = 'progress_role',
      max_value        = 100,
      value            = 0,
      direction        = 'north',
      background_color = beautiful.bg_normal,
      color            = 'linear:150,0:0,0:0,#D08770:0.3,#BF616A:0.6,' .. beautiful.fg_normal,
      widget           = wibox.widget.progressbar
    },
    forced_width = dpi(10),
    direction    = 'east',
    widget       = wibox.container.rotate
  }
end

local cpu_cols = {
  spacing = dpi(1),
  layout = wibox.layout.fixed.horizontal,
}

local result = io.popen("cat /proc/stat | grep '^cpu'"):read('*all')
log('result: ' .. result)

local cpudata = gears.string.split(result, '\n')
local cpu_count = #cpudata - 2

for i = 1, cpu_count do
  cpu_cols[i] = create_cpu_col()
end


local widget = wibox.widget {
  cpu_cols,
  forced_width = dpi(cpu_count * 11),
  widget = wibox.container.background
}

connect('service::cpu', function(cores, core_usage)
  for i = 1, cpu_count do
    local progress = cpu_cols[i]:get_children_by_id('progress_role')[1]
    progress:set_value(core_usage[i])
  end
end)

return widget

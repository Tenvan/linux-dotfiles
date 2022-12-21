log('Enter Module => ' .. ...)

local widget = {}

local function worker(user_args)
 local args = user_args or {}
  widget = wibox.widget {
    orientation = args.orientation or 'vertical',
    forced_width = args.size or dpi(5),
    color = beautiful.bg_focus,
    widget = wibox.widget.separator
  }
  return widget
end

return setmetatable(widget, { __call = function(_, ...)
  return worker(...)
end })

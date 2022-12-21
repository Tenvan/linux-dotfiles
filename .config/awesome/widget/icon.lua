log('Enter Module => ' .. ...)

local widget = {}

local function worker(user_args)
  widget = wibox.widget {
    {
      text         = user_args.icon or '',
      forced_width = user_args.size or dpi(32),
      align        = 'center',
      valign       = 'center',
      font         = beautiful.font_large,
      widget       = wibox.widget.textbox
    },
    margins = dpi(2),
    widget  = wibox.container.margin
  }

  return widget
end

return setmetatable(widget, { __call = function(_, ...)
  return worker(...)
end })

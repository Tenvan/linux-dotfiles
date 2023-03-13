log('Enter Module => ' .. ...)

local text   = require('widget.text')

local widget = {}

local function worker(user_args)
  widget = wibox.widget {
    {
      {
        text         = user_args.icon or '⚠️',
        forced_width = user_args.size or dpi(32),
        align        = 'center',
        valign       = 'center',
        font         = beautiful.font_symbol,
        widget       = wibox.widget.textbox
      },
      text({ text = user_args.label or '' }),
      layout = wibox.layout.fixed.horizontal
    },
    margins = dpi(2),
    widget  = wibox.container.margin
  }

  return widget
end

return setmetatable(widget, {
  __call = function(_, ...)
    return worker(...)
  end
})

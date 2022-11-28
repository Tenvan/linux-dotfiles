log('Enter Module => ' .. ...)

local beautiful = require('beautiful')
local gears = require('gears')
local wibox = require('wibox')

---create a custom button
---@param args table
---@return table
local function myButton(args)
  local text = args.text
  local onLeftClick = args.onLeftClick or function() end
  local onRightClick = args.onRightClick or function() end
  local color = args.color or beautiful.bg_normal
  local text_size = args.text_size or 10
  local margin_h = args.margin_h or dpi(5)
  local margin_v = args.margin_v or dpi(1)

  local result = wibox.widget {
    {
      {
        -- markup = '<span size="' .. text_size .. '000" ">' .. text .. '</span>',
        text = text,
        font = 'sans 14',
        id = 'textbox',
        widget = wibox.widget.textbox
      },
      top = margin_v,
      bottom = margin_v,
      left = margin_h,
      right = margin_h,
      widget = wibox.container.margin
    },
    shape = function(cr, width, height) gears.shape.rounded_rect(cr, width, height, 4) end,
    widget = wibox.container.background
  }

  result.set_color = function(color)
    result:set_bg(color)
  end

  result.set_text = function(text)
    result.widget.widget.text = text
  end

  local old_cursor, old_wibox, backup_bg, backup_fg

  result:connect_signal('mouse::enter', function(c)
    backup_bg = c.bg
    backup_fg = c.fg
    c:set_bg(beautiful.accent:sub(1, 7) .. '80')
    c:set_fg(beautiful.bg_normal)
    local wb = mouse.current_wibox
    if wb ~= nil then
      old_cursor, old_wibox = wb.cursor, wb
      wb.cursor = 'hand2'
    end
  end)

  result:connect_signal('mouse::leave', function(c)
    c:set_bg(backup_bg)
    c:set_fg(backup_fg)
    if old_wibox then
      old_wibox.cursor = old_cursor
      old_wibox = nil
    end
  end)

  result:connect_signal('button::press', function() onLeftClick() end)

  return result
end

return myButton

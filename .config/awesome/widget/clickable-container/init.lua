local log = require('utilities.debug').log
local dump = require('utilities.debug').dump
log('Enter Module => ' .. ...)

local wibox = require('wibox')
local beautiful = require('beautiful')

-- $ cd ~/.icons/theme/cursors/
-- $ right_ptr arrow
-- $ cross crosshair
-- $ right_ptr draft_large
-- $ right_ptr draft_small
-- $ cross plus
-- $ left_ptr top_left_arrow
-- $ cross tcross
-- $ hand hand1
-- $ hand hand2
-- $ left_side left_tee
-- $ left_ptr ul_angle
-- $ left_ptr ur_angle
-- $ left_ptr_watch 08e8e1c95fe2fc01f976f1e063a24ccd

local create_click_events = function(widget)
  local container = wibox.widget {
    widget,
    bg = beautiful.transparent,
    widget = wibox.container.background
  }

  -- Old and new widget
  local old_cursor, old_wibox

  -- Mouse hovers on the widget
  container:connect_signal(
    'mouse::enter',
    function()
      container.backup_bg = container.bg
      container.bg = beautiful.gtk_vars.header_button_bg_color
      -- Hm, no idea how to get the wibox from this signal's arguments...
      local w = mouse.current_wibox
      if w then
        old_cursor, old_wibox = w.cursor, w
        w.cursor = 'hand2'
      end
    end
  )

  -- Mouse leaves the widget
  container:connect_signal(
    'mouse::leave',
    function()
      container.bg = container.backup_bg
      if old_wibox then
        old_wibox.cursor = old_cursor
        old_wibox = nil
      end
    end
  )

  -- Mouse pressed the widget
  container:connect_signal(
    'button::press',
    function()
      container.bg = beautiful.gtk_vars.header_button_bg_color
    end
  )

  -- Mouse releases the widget
  container:connect_signal(
    'button::release',
    function()
      container.bg = beautiful.gtk_vars.header_button_bg_color
    end
  )

  return container
end

return create_click_events

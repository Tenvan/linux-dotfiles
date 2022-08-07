local awful = require('awful')
local beautiful = require('beautiful')
local gears = require('gears')
local watch = require('awful.widget.watch')
local wibox = require('wibox')

local dpi = require('beautiful.xresources').apply_dpi

local widget = {}

local function worker(user_args)
  local args = user_args or {}

  local step_width = args.step_width or dpi(4)
  local step_spacing = args.step_spacing or dpi(1)
  local color = args.color or beautiful.graph_fg
  local background_color = args.background_color or beautiful.graph_bg

  local timeout    = args.timeout or 1
  local color_buf  = args.color_buf or beautiful.xres_vars.color1
  local color_used = args.color_used or beautiful.xres_vars.color2
  local color_free = args.color_free or beautiful.transparent

  local widget_show_buf = args.widget_show_buf or false

  local widget_height = args.widget_height or beautiful.element_size
  local widget_width  = args.widget_width or beautiful.element_size * 5

  --- Main ram widget shown on wibar
  widget = wibox.widget {

    max_value = 100,
    stack = true,
    background_color = background_color,
    border_color = beautiful.graph_border_color,

    forced_width = widget_width,
    step_width = step_width,
    step_spacing = step_spacing,
    stack_colors = {
      color_buf,
      color_used,
      color_free
    },
    id = 'graph_role',
    widget = wibox.widget.graph,
  }

  local total, used, free, shared, buff_cache, available, total_swap, used_swap, free_swap

  local function getPercentage(value)
    return math.floor(value / (total + total_swap) * 100 + 0.5)
  end

  watch('bash -c "LANGUAGE=en_US.UTF-8 free | grep -z Mem.*Swap.*"', timeout,
    function(widget, stdout)
      total, used, free, shared, buff_cache, available, total_swap, used_swap, free_swap = stdout:match('(%d+)%s*(%d+)%s*(%d+)%s*(%d+)%s*(%d+)%s*(%d+)%s*Swap:%s*(%d+)%s*(%d+)%s*(%d+)')

      local p_used = getPercentage(used + used_swap)
      local p_free = getPercentage(free + free_swap)
      local p_buff_cache = getPercentage(buff_cache)

      widget:add_value(p_buff_cache, 1)
      widget:add_value(p_used, 2)
      widget:add_value(p_free, 3)

      log('Memory Watcher => Used: ' .. p_used .. ' Free: ' .. p_free .. ' Cache: ' .. p_buff_cache)
    end,
    widget:get_children_by_id('graph_role')[1]
  )

  --- Widget which is shown when user clicks on the ram widget
  local popup = awful.popup {
    ontop = true,
    visible = false,
    widget = {
      widget = wibox.widget.piechart,
      forced_height = dpi(400),
      forced_width = dpi(600),
      colors = {
        color_buf,
        color_used,
        color_free,
      },
    },
    shape = gears.shape.rounded_rect,
    border_color = beautiful.popup_border,
    border_width = beautiful.popup_border_width,
    offset = { y = dpi(5) },
  }

  widget:connect_signal(
    'mouse::enter',
    function(w)
      popup:get_widget().data_list = {
        { 'buff_cache ' .. getPercentage(buff_cache), buff_cache },
        { 'used ' .. getPercentage(used + used_swap), used + used_swap },
        { 'free ' .. getPercentage(free + free_swap), free + free_swap },
      }

      popup:move_next_to(mouse.current_widget_geometry)
      popup.visible = true
    end
  )

  widget:connect_signal(
    'mouse::leave',
    function(w)
      popup.visible = false
    end
  )

  return widget
end

return setmetatable(widget, { __call = function(_, ...)
  return worker(...)
end })

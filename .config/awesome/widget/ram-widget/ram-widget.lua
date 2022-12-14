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

  local value_colors = {
    beautiful.xres_vars.color2,
    beautiful.xres_vars.color3,
    beautiful.xres_vars.color4,
    beautiful.xres_vars.color1,
    beautiful.xres_vars.color5,
  }

  local chart_text = [[
<span size="large" color="]] .. value_colors[1] .. [[">Free Memory     </span>: <b>%3d %% / %6.1f MB</b>
<span size="large" color="]] .. value_colors[2] .. [[">Free Swap Memory</span>: <b>%3d %% / %6.1f MB</b>
<span size="large" color="]] .. value_colors[3] .. [[">Buffer Cache    </span>: <b>%3d %% / %6.1f MB</b>
<span size="large" color="]] .. value_colors[4] .. [[">Used Memory     </span>: <b>%3d %% / %6.1f MB</b>
<span size="large" color="]] .. value_colors[5] .. [[">Used Swap Memory</span>: <b>%3d %% / %6.1f MB</b>
]]

  --- Main ram widget shown on wibar
  widget = wibox.widget {

    max_value = 100,
    stack = true,
    background_color = background_color,
    border_color = beautiful.graph_border_color,

    forced_width = widget_width,
    step_width = step_width,
    step_spacing = step_spacing,
    stack_colors = value_colors,
    id = 'graph_role',
    widget = wibox.widget.graph,
  }

  local total, used, free, shared, buff_cache, available, total_swap, used_swap, free_swap

  local function getPercentage(value)
    return math.floor(value / (total + total_swap) * 100 + 0.5)
  end

  --- Widget which is shown when user clicks on the ram widget
  local popup = awful.popup {
    ontop = true,
    visible = false,
    widget = {
      {
        widget = wibox.widget {
          markup = chart_text,
          widget = wibox.widget.textbox
        },
        id = 'text_role',
      },
      {
        widget = wibox.container.arcchart,
        id = 'chart_role',
        min_value = 0,
        max_value = 100,
        paddings = dpi(20),
        rounded_edge = true,
        colors = value_colors,
        background_color = beautiful.transparent,
        border_color = beautiful.graph_border_color,
        border_width = dpi(10),
        thickness = dpi(50),
        forced_width  = dpi(300),
        forced_height  = dpi(300),
      },
      forced_height = dpi(500),
      forced_width  = dpi(400),
      layout        = wibox.layout.align.vertical,
    },
    shape = gears.shape.rounded_rect,
    border_color = beautiful.popup_border,
    border_width = beautiful.popup_border_width,
    offset = { y = dpi(5) },
  }

  local old_cursor, old_wibox
  widget:connect_signal(
    'mouse::enter',
    function(self)
      local wb = mouse.current_wibox
      if wb ~= nil then
        old_cursor, old_wibox = wb.cursor, wb
        wb.cursor = 'hand2'
      end
      -- { string.format('free mem %s %%', getPercentage(free)), free },
      -- { string.format('free swap %s %%', getPercentage(free_swap)), free_swap },
      -- { string.format('buffer cache %s %%', getPercentage(buff_cache)), buff_cache },
      -- { string.format('used mem %s %%', getPercentage(used)), used },
      -- { string.format('used swap %s %%', getPercentage(used_swap)), used_swap },

      popup:move_next_to(mouse.current_widget_geometry)
      popup.visible = true
    end
  )

  widget:connect_signal(
    'mouse::leave',
    function(self)
      if old_wibox then
        old_wibox.cursor = old_cursor
        old_wibox = nil
      end
      popup.visible = false
    end
  )

  widget:connect_signal('button::press',
    function()
      awful.spawn.with_shell('sync')
      awful.spawn.with_shell('echo 1 | sudo tee /proc/sys/vm/drop_caches')
    end)

  local top_widget_graph = widget:get_children_by_id('graph_role')[1]
  local popup_widget_graph = popup.widget:get_children_by_id('chart_role')[1]
  local popup_widget_text = popup.widget:get_children_by_id('text_role')[1]

  connect('service::ram',
    function(values)
      total = values[1]
      used = values[2]
      free = values[3]
      buff_cache = values[5]
      total_swap = values[7]
      used_swap = values[8]
      free_swap = values[9]

      local p_used = getPercentage(used + used_swap)
      local p_free = getPercentage(free + free_swap)
      local p_buff_cache = getPercentage(buff_cache)

      -- top_widget_graph:add_value(p_buff_cache, 1)
      -- top_widget_graph:add_value(p_used, 2)
      -- top_widget_graph:add_value(p_free, 3)
      top_widget_graph:add_value(getPercentage(free), 1)
      top_widget_graph:add_value(getPercentage(free_swap), 2)
      top_widget_graph:add_value(getPercentage(buff_cache), 3)
      top_widget_graph:add_value(getPercentage(used), 4)
      top_widget_graph:add_value(getPercentage(used_swap), 5)

      popup_widget_graph.values = {
        getPercentage(free),
        getPercentage(free_swap),
        getPercentage(buff_cache),
        getPercentage(used),
        getPercentage(used_swap),
      }

      popup_widget_text.markup = string.format(chart_text,
        getPercentage(free), free / 1024,
        getPercentage(free_swap), free_swap / 1024,
        getPercentage(buff_cache), buff_cache / 1024,
        getPercentage(used), used / 1024,
        getPercentage(used_swap), used_swap / 1024)

      -- poup_widget_graph:add_value(getPercentage(free), 1)
      -- poup_widget_graph:add_value(getPercentage(free_swap), 2)
      -- poup_widget_graph:add_value(getPercentage(buff_cache), 3)
      -- poup_widget_graph:add_value(getPercentage(used), 4)
      -- poup_widget_graph:add_value(getPercentage(used_swap), 5)
    end)

  return widget
end

return setmetatable(widget, { __call = function(_, ...)
  return worker(...)
end })

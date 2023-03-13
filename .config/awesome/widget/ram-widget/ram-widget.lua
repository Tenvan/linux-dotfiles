log('Enter Module => ' .. ...)

local watch = require('awful.widget.watch')
local dpi = require('beautiful.xresources').apply_dpi
local icon = require('widget.icon')

local seperator = wibox.widget {
  orientation = 'vertical',
  forced_width = dpi(5),
  color = beautiful.bg_focus,
  widget = wibox.widget.separator
}

local ram_widgets = {}

local function worker(user_args)
  local args = user_args or {}

  local step_width = args.step_width or dpi(4)
  local step_spacing = args.step_spacing or dpi(1)
  local background_color = args.background_color or beautiful.graph_bg

  local widget_height = args.widget_height or beautiful.element_size
  local widget_width  = args.widget_width or beautiful.element_size * 5

  local value_colors_mem_widget = {
    beautiful.xres_vars.color3,
    beautiful.xres_vars.color4,
    beautiful.xres_vars.color2,
  }

  local value_colors_swap_widget = {
    beautiful.xres_vars.color3,
    beautiful.xres_vars.color2,
  }

  local chart_text_memory = [[
<span size="large" color="]] .. value_colors_mem_widget[1] .. [[">Used Memory     </span>: <b>%3d %% / %6.1f MB</b>
<span size="large" color="]] .. value_colors_mem_widget[2] .. [[">Buffer Cache    </span>: <b>%3d %% / %6.1f MB</b>
<span size="large" color="]] .. value_colors_mem_widget[3] .. [[">Free Memory     </span>: <b>%3d %% / %6.1f MB</b>
]]

  local chart_text_swap = [[
<span size="large" color="]] .. value_colors_swap_widget[1] .. [[">Used Swap Memory</span>: <b>%3d %% / %6.1f MB</b>
<span size="large" color="]] .. value_colors_swap_widget[2] .. [[">Free Swap Memory</span>: <b>%3d %% / %6.1f MB</b>
]]

  local memory_widget_graph = wibox.widget {
    --- Main ram widget shown on wibar
    max_value = 100,
    stack = true,
    background_color = background_color,
    border_color = beautiful.graph_border_color,
    forced_width = widget_width / 2,
    step_width = step_width,
    step_spacing = step_spacing,
    stack_colors = value_colors_mem_widget,
    id = 'memory_graph_role',
    widget = wibox.widget.graph,
  }

  local swap_widget_graph = wibox.widget {
    --- swap ram widget shown on wibar
    max_value = 100,
    stack = true,
    background_color = background_color,
    border_color = beautiful.graph_border_color,
    forced_width = widget_width / 2,
    step_width = step_width,
    step_spacing = step_spacing,
    stack_colors = value_colors_swap_widget,
    id = 'swap_graph_role',
    widget = wibox.widget.graph,
  }


  ram_widgets = wibox.widget {
    memory_widget_graph,
    seperator,
    swap_widget_graph,
    layout = wibox.layout.fixed.horizontal
  }

  local total_memory, used_memory, free_memory, shared, buff_cache, available, total_swap, used_swap, free_swap

  local function getPercentageMemory(value)
    return math.floor(value / total_memory * 100 + 0.5)
  end

  local function getPercentageSwap(value)
    return math.floor(value / total_swap * 100 + 0.5)
  end

  local popup_widget_memory_chart = wibox.widget {
    {
      {
        widget = wibox.widget {
          markup = chart_text_memory,
          widget = wibox.widget.textbox
        },
        id = 'text_role',
      },
      {
        min_value        = 1,
        max_value        = 100,
        paddings         = dpi(20),
        colors           = value_colors_mem_widget,
        background_color = beautiful.transparent,
        border_color     = beautiful.graph_border_color,
        border_width     = dpi(10),
        thickness        = dpi(30),
        forced_width     = dpi(200),
        forced_height    = dpi(200),
        id               = 'role_chart',
        widget           = wibox.container.arcchart,
      },
      id = 'role_memory_chart',
      layout = wibox.layout.align.vertical,
    },
    margins = dpi(10),
    widget = wibox.container.margin
  }

  local popup_widget_memory_text = popup_widget_memory_chart:get_children_by_id('text_role')[1]
  local popup_widget_graph_memory = popup_widget_memory_chart:get_children_by_id('role_chart')[1]

  local widget_swap_chart = wibox.widget {
    {
      {
        widget = wibox.widget {
          markup = chart_text_swap,
          widget = wibox.widget.textbox
        },
        id = 'text_role',
      },
      {
        min_value        = 1,
        max_value        = 100,
        paddings         = dpi(20),
        colors           = value_colors_swap_widget,
        background_color = beautiful.transparent,
        border_color     = beautiful.graph_border_color,
        border_width     = dpi(10),
        thickness        = dpi(30),
        forced_width     = dpi(200),
        forced_height    = dpi(200),
        id               = 'role_chart',
        widget           = wibox.container.arcchart,
      },
      id = 'role_memory_chart',
      layout = wibox.layout.align.vertical,
    },
    margins = dpi(10),
    widget = wibox.container.margin
  }

  local widget_swap_text = widget_swap_chart:get_children_by_id('text_role')[1]
  local widget_graph_swap = widget_swap_chart:get_children_by_id('role_chart')[1]

  --- Widget which is shown when user clicks on the ram widget
  local popup = awful.popup {
    ontop = true,
    visible = false,
    widget = {
      popup_widget_memory_chart,
      widget_swap_chart,
      -- forced_height = dpi(500),
      -- forced_width  = dpi(400),
      layout = wibox.layout.align.horizontal,
    },
    shape = gears.shape.rounded_rect,
    border_color = beautiful.popup_border,
    border_width = beautiful.popup_border_width,
    offset = { y = dpi(5) },
  }

  local old_cursor, old_wibox
  ram_widgets:connect_signal(
    'mouse::enter',
    function(self)
      local wb = mouse.current_wibox
      if wb ~= nil then
        old_cursor, old_wibox = wb.cursor, wb
        wb.cursor = 'hand2'
      end
      popup:move_next_to(mouse.current_widget_geometry)
      popup.visible = true
    end
  )

  ram_widgets:connect_signal(
    'mouse::leave',
    function(self)
      if old_wibox then
        old_wibox.cursor = old_cursor
        old_wibox = nil
      end
      popup.visible = false
    end
  )

  ram_widgets:connect_signal('button::press',
    function()
      awful.spawn.with_shell('sync')
      awful.spawn.with_shell('echo 1 | sudo tee /proc/sys/vm/drop_caches')
    end)

  connect('service::ram',
    function(values)
      -- calculate values
      total_memory = values[1]
      used_memory = values[2]
      free_memory = values[3]

      buff_cache = values[5]

      total_swap = values[7]
      used_swap = values[8]
      free_swap = values[9]

      -- refresh top widget chart
      memory_widget_graph:add_value(getPercentageMemory(used_memory), 1)
      memory_widget_graph:add_value(getPercentageMemory(buff_cache), 2)
      memory_widget_graph:add_value(getPercentageMemory(free_memory), 3)

      swap_widget_graph:add_value(getPercentageSwap(used_swap), 1)
      swap_widget_graph:add_value(getPercentageSwap(free_swap), 2)

      if popup.visible then

        -- refresh memory chart
        popup_widget_memory_text.markup = string.format(chart_text_memory,
          getPercentageMemory(used_memory), used_memory / 1024,
          getPercentageMemory(buff_cache), buff_cache / 1024,
          getPercentageMemory(free_memory), free_memory / 1024)
        popup_widget_graph_memory.values = {
          getPercentageMemory(used_memory),
          getPercentageMemory(buff_cache),
          getPercentageMemory(free_memory),
        }

        -- refresh swap chart
        widget_swap_text.markup = string.format(chart_text_swap,
          getPercentageSwap(used_swap), used_swap / 1024,
          getPercentageSwap(free_swap), free_swap / 1024)
        widget_graph_swap.values = {
          getPercentageSwap(used_swap),
          getPercentageSwap(free_swap),
        }
      end
    end)

  return ram_widgets
end

return setmetatable(ram_widgets, { __call = function(_, ...)
  return worker(...)
end })

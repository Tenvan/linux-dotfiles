log('Enter Module => ' .. ...)

local beautiful = require('beautiful')
local dpi = beautiful.xresources.apply_dpi
local wibox = require('wibox')
local clickable_container = require('widget.clickable-container')
local task_list = require('widget.task-list')
local specs = require('layout.specs')
local config = require('configuration.json')

local offsetx = specs.leftPanel.actionBarWidth

local bottom_panel = function(s, offset)
  local panel = wibox {
    ontop = true,
    screen = s,
    type = 'dock',
    height = specs.bottomPanel.height,
    width = s.geometry.width,
    x = s.geometry.x,
    y = s.geometry.height - specs.bottomPanel.height,
    stretch = false,
    bg = beautiful.background,
    fg = beautiful.fg_normal
  }

  panel:struts {
    bottom = specs.bottomPanel.height
  }

  panel:connect_signal('mouse::enter', function()
    local w = mouse.current_wibox
    if w then
      w.cursor = 'left_ptr'
    end
  end)

  panel:setup {
    {
      -- left widgets
      task_list(s),
      spacing = dpi(2),
      layout = wibox.layout.fixed.horizontal,
    },
    -- middle widgets
    nil,
    nil,
    expand = 'none',
    spacing = 4,
    layout = wibox.layout.align.horizontal,
  }

  return panel
end

return bottom_panel

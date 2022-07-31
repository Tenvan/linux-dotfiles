-- work in progress

log('Enter Module => ' .. ...)

local awful = require('awful')
local wibox = require('wibox')
local specs = require('layout.specs')

local dpi = require('beautiful').xresources.apply_dpi

-- ░█▀▄░█░░░▀█▀░█▀█░█▀▀
-- ░█▀▄░█░░░░█░░█░█░█░█
-- ░▀▀░░▀▀▀░▀▀▀░▀░▀░▀▀▀
local bling = require('module.bling')

local posY = specs.topPanel.height

bling.widget.task_preview.enable {
  x = dpi(20), -- The x-coord of the popup
  y = posY, -- The y-coord of the popup
  height = dpi(400), -- The height of the popup
  width = dpi(600), -- The width of the popup

  honor_padding = true, -- Honor padding when creating widget size
  honor_workarea = true, -- Honor work area when creating widget size

  placement_fn = function(c) -- Place the widget using awful.placement (this overrides x & y)
    if c and c.tag then
      log('==> placement task preview')
      return awful.placement.next_to(
        c,
        {
          preferred_positions = 'right',
          preferred_anchors   = 'front',
          geometry            = c.tag.geometry,
        }
      )
    end
  end,

  -- Your widget will automatically conform to the given size due to a constraint container.
  widget_structure = {
    {
      {
        {
          id = 'icon_role',
          widget = awful.widget.clienticon, -- The client icon
        },
        {
          id = 'name_role', -- The client name / title
          widget = wibox.widget.textbox,
        },
        layout = wibox.layout.flex.horizontal
      },
      widget = wibox.container.margin,
      margins = dpi(5)
    },
    {
      id = 'image_role', -- The client preview
      resize = true,
      valign = 'center',
      halign = 'center',
      widget = wibox.widget.imagebox,
    },
    layout = wibox.layout.fixed.vertical
  }
}

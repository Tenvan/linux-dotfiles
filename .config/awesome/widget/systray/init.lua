log('Enter Module => widget/systray/init.lua')

local wibox = require("wibox")
local specs = require('layout.specs')
local beautiful = require("beautiful")

local dpi = require("beautiful.xresources").apply_dpi

local offsetx = specs.leftPanel.actionBarWidth

local systray = {
  wibox.widget {
    visible = true,
    -- base_size = dpi(offsetx - 4),
    horizontal = true,
    screen = 'primary',
    widget = wibox.widget.systray
  },
  margins = dpi(5),
  widget = wibox.container.margin
}

return {
  widget = systray
}
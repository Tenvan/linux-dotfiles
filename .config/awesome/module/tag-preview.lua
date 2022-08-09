log('Enter Module => ' .. ...)

local awful      = require('awful')
local wibox      = require('wibox')
local filesystem = require('gears.filesystem')

local config_dir = filesystem.get_configuration_dir()
local specs = require('layout.specs')

local dpi = require('beautiful').xresources.apply_dpi

-- ░█▀▄░█░░░▀█▀░█▀█░█▀▀
-- ░█▀▄░█░░░░█░░█░█░█░█
-- ░▀▀░░▀▀▀░▀▀▀░▀░▀░▀▀▀
local bling = require('module.bling')

local posY = specs.topPanel.height
local posX = specs.leftPanel.actionBarWidth

bling.widget.tag_preview.enable {
  show_client_content = true, -- Whether or not to show the client content

  x = posX + dpi(1), -- The x-coord of the popup
  y = posY + dpi(1), -- The y-coord of the popup

  scale = 0.25, -- The scale of the previews compared to the screen

  honor_padding = true, -- Honor padding when creating widget size
  honor_workarea = true, -- Honor work area when creating widget size

  placement_fn = awful.placement.next_to,

  background_widget = wibox.widget {
    -- Set a background image (like a wallpaper) for the widget
    image                 = config_dir .. '/theme/wallpapers/locksreen-bg.jpg',
    horizontal_fit_policy = 'fit',
    vertical_fit_policy   = 'fit',
    widget                = wibox.widget.imagebox
  }
}

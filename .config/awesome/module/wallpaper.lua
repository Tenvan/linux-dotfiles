-- work in progress

log('Enter Module => ' .. ...)

local bling = require('module.bling')
local config = require('configuration.config')

local wall_dir = config.module.dynamic_wallpaper.wall_dir

-- wallpapers based on a schedule, like awesome-glorious-widgets dynamic wallpaper
-- https://github.com/manilarome/awesome-glorious-widgets/tree/master/dynamic-wallpaper
bling.module.wallpaper.setup {
  set_function = bling.module.wallpaper.setters.simple_schedule,
  wallpaper = config.module.dynamic_wallpaper.wallpaper_schedule,
  position = 'maximized',
}

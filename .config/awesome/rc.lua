-- ░▀█▀░█▀▀░█▀█░█░█░█▀█░█▀█░█▀▀░░░█▀█░█░█░█▀▀░█▀▀░█▀█░█▄█░█▀▀
-- ░░█░░█▀▀░█░█░▀▄▀░█▀█░█░█░▀▀█░░░█▀█░█▄█░█▀▀░▀▀█░█░█░█░█░█▀▀
-- ░░▀░░▀▀▀░▀░▀░░▀░░▀░▀░▀░▀░▀▀▀░░░▀░▀░▀░▀░▀▀▀░▀▀▀░▀▀▀░▀░▀░▀▀▀
-- Banner generated using `toilet -f pagga AwesomeWM"

local gdebug = require('gears.debug')
gdebug.print_warning('[STARTUP]: ==============================')
gdebug.print_warning('[STARTUP]: ==> start of loading rc.lua ==')
gdebug.print_warning('[STARTUP]: ==============================')

log = require('utilities.debug').log
dump = require('utilities.debug').dump
notify = require('utilities.notify')

log('Enter Module => rc.lua')

pcall(require, 'luarocks.loader')
local gears = require('gears')
local beautiful = require('beautiful')
local awful = require('awful')

-- Zwölf Boxkämpfer  ==>

local test = (1 == 2)

-- Everything related to window managment

-- ░█▀▀░█░█░█▀▀░█░░░█░░
-- ░▀▀█░█▀█░█▀▀░█░░░█░░
-- ░▀▀▀░▀░▀░▀▀▀░▀▀▀░▀▀▀
awful.util.shell = 'zsh'

-- ░▀█▀░█░█░█▀▀░█▄█░█▀▀
-- ░░█░░█▀█░█▀▀░█░█░█▀▀
-- ░░▀░░▀░▀░▀▀▀░▀░▀░▀▀▀
beautiful.init(require('theme'))

-- ░█░░░█▀█░█░█░█▀█░█░█░▀█▀
-- ░█░░░█▀█░░█░░█░█░█░█░░█░
-- ░▀▀▀░▀░▀░░▀░░▀▀▀░▀▀▀░░▀░
require('layout')

-- ░█▀▀░█▀█░█▀█░█▀▀░▀█▀░█▀▀░█░█░█▀▄░█▀█░▀█▀░▀█▀░█▀█░█▀█░█▀▀
-- ░█░░░█░█░█░█░█▀▀░░█░░█░█░█░█░█▀▄░█▀█░░█░░░█░░█░█░█░█░▀▀█
-- ░▀▀▀░▀▀▀░▀░▀░▀░░░▀▀▀░▀▀▀░▀▀▀░▀░▀░▀░▀░░▀░░▀▀▀░▀▀▀░▀░▀░▀▀▀
require('configuration.client')
require('configuration.root')
require('configuration.tags')
root.keys(require('configuration.keys.global'))

-- ░█▄█░█▀█░█▀▄░█░█░█░░░█▀▀░█▀▀
-- ░█░█░█░█░█░█░█░█░█░░░█▀▀░▀▀█
-- ░▀░▀░▀▀▀░▀▀░░▀▀▀░▀▀▀░▀▀▀░▀▀▀
require('module')

-- ░█▀▄░█░█░█▀▄░█▀█░▀█▀░█▀█
-- ░█▀▄░█░█░█▀▄░█▀█░░█░░█░█
-- ░▀░▀░▀▀▀░▀▀░░▀░▀░░▀░░▀▀▀
require('module.rubato')

-- ░█░░░█▀█░█░█░█▀█░█░█░▀█▀░░░░░█▄█░█▀█░█▀▀░█░█░▀█▀
-- ░█░░░█▀█░░█░░█░█░█░█░░█░░▄▄▄░█░█░█▀█░█░░░█▀█░░█░
-- ░▀▀▀░▀░▀░░▀░░▀▀▀░▀▀▀░░▀░░░░░░▀░▀░▀░▀░▀▀▀░▀░▀░▀▀▀
require('module.layout-machi')

-- ░█▀▀░█░░░█▀█░█▀▄░█▀█░█░░░░░█▀▀░▀█▀░█▀▀░█▀█░█▀█░█░░░█▀▀
-- ░█░█░█░░░█░█░█▀▄░█▀█░█░░░░░▀▀█░░█░░█░█░█░█░█▀█░█░░░▀▀█
-- ░▀▀▀░▀▀▀░▀▀▀░▀▀░░▀░▀░▀▀▀░░░▀▀▀░▀▀▀░▀▀▀░▀░▀░▀░▀░▀▀▀░▀▀▀
require('configuration.signals')

-- ░█▀█░█░█░▀█▀░█▀█░█▀▀░▀█▀░█▀█░█▀▄░▀█▀
-- ░█▀█░█░█░░█░░█░█░▀▀█░░█░░█▀█░█▀▄░░█░
-- ░▀░▀░▀▀▀░░▀░░▀▀▀░▀▀▀░░▀░░▀░▀░▀░▀░░▀░
require('module.auto-start')

-- ░█░█░█▀█░█░░░█░░░█▀█░█▀█░█▀█░█▀▀░█▀▄
-- ░█▄█░█▀█░█░░░█░░░█▀▀░█▀█░█▀▀░█▀▀░█▀▄
-- ░▀░▀░▀░▀░▀▀▀░▀▀▀░▀░░░▀░▀░▀░░░▀▀▀░▀░▀
require('module.wallpaper')

screen.connect_signal(
  'request::wallpaper',
  function(s)
    log('spawn::request::wallpaper')
    -- If wallpaper is a function, call it with the screen
    if beautiful.wallpaper then
      if type(beautiful.wallpaper) == 'string' then
        log('(s) wallpaper ==> ' ..  beautiful.wallpaper)
        
        -- Check if beautiful.wallpaper is color/image
        if beautiful.wallpaper:sub(1, #'#') == '#' then
          -- If beautiful.wallpaper is color
          gears.wallpaper.set(beautiful.wallpaper)
          
        elseif beautiful.wallpaper:sub(1, #'/') == '/' then
          -- If beautiful.wallpaper is path/image
          gears.wallpaper.maximized(beautiful.wallpaper, s)
        end
      else
        beautiful.wallpaper(s)
        dump('(o) wallpaper ==> ' .. tostring(s))
      end
    end
  end
)

gdebug.print_warning('[STARTUP]: ==============================')
gdebug.print_warning('[STARTUP]: == finish of loading rc.lua ==')
gdebug.print_warning('[STARTUP]: ==============================')

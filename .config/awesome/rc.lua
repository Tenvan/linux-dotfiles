-- ░▀█▀░█▀▀░█▀█░█░█░█▀█░█▀█░█▀▀░░░█▀█░█░█░█▀▀░█▀▀░█▀█░█▄█░█▀▀
-- ░░█░░█▀▀░█░█░▀▄▀░█▀█░█░█░▀▀█░░░█▀█░█▄█░█▀▀░▀▀█░█░█░█░█░█▀▀
-- ░░▀░░▀▀▀░▀░▀░░▀░░▀░▀░▀░▀░▀▀▀░░░▀░▀░▀░▀░▀▀▀░▀▀▀░▀▀▀░▀░▀░▀▀▀
-- Banner generated using `toilet -f pagga AwesomeWM"

local gdebug = require('gears.debug')
gdebug.print_warning('[STARTUP]: ==============================')
gdebug.print_warning('[STARTUP]: ==> start of loading rc.lua ==')
gdebug.print_warning('[STARTUP]: ==============================')

-- often used globals
awful = require('awful')
beautiful = require('beautiful')
gears = require('gears')
wibox = require('wibox')

log = require('utilities.debug').log
logd = require('utilities.debug').logd
trace = require('utilities.debug').trace
dump = require('utilities.debug').dump
dpi = beautiful.xresources.apply_dpi
notify = require('utilities.notify')
emit = require('services.utils').emit
connect = require('services.utils').connect

log('Enter Module => rc.lua')

pcall(require, 'luarocks.loader')

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

-- ░█▀▀░█▀▀░█▀▄░█░█░▀█▀░█▀▀░█▀▀░█▀▀
-- ░▀▀█░█▀▀░█▀▄░▀▄▀░░█░░█░░░█▀▀░▀▀█
-- ░▀▀▀░▀▀▀░▀░▀░░▀░░▀▀▀░▀▀▀░▀▀▀░▀▀▀
require('services')

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

if awesome.startup_errors then
  notify('Oops, there were errors during startup!', awesome.startup_errors, 'critical')
  log('Startup Errors: ' .. awesome.startup_errors)
end


gdebug.print_warning('[STARTUP]: ==============================')
gdebug.print_warning('[STARTUP]: == finish of loading rc.lua ==')
gdebug.print_warning('[STARTUP]: ==============================')

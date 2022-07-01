local log = require('utilities.debug').log
log("Enter Module => module/init.lua" )

-- ░█▀▀░█▀█░█▀█░█▀▀░▀█▀░█▀▀░░░█▄█░█▀█░█▀▄░█░█░█░░░█▀▀░█▀▀
-- ░█░░░█░█░█░█░█▀▀░░█░░█░█░░░█░█░█░█░█░█░█░█░█░░░█▀▀░▀▀█
-- ░▀▀▀░▀▀▀░▀░▀░▀░░░▀▀▀░▀▀▀░░░▀░▀░▀▀▀░▀▀░░▀▀▀░▀▀▀░▀▀▀░▀▀▀
require('module.auto-start')
require('module.brightness-osd')
require('module.dynamic-wallpaper')
require('module.exit-screen')
-- require('module.lockscreen')
require('module.menu')
require('module.notifications')
require('module.quake-terminal')
require('module.titlebar')
require('module.volume-osd')
require("module.better-resize")

-- ░█░█░█▀▀░█▀█░█▀▄░█▀█░█▀▄░░░█▄█░█▀█░█▀▄░█░█░█░░░█▀▀░█▀▀
-- ░▀▄▀░█▀▀░█░█░█░█░█░█░█▀▄░░░█░█░█░█░█░█░█░█░█░░░█▀▀░▀▀█
-- ░░▀░░▀▀▀░▀░▀░▀▀░░▀▀▀░▀░▀░░░▀░▀░▀▀▀░▀▀░░▀▀▀░▀▀▀░▀▀▀░▀▀▀require("module.vicious")
require("module.bling")
require("module.rubato")
require("module.layout-machi")


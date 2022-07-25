log("Enter Module => " .. ... )

-- ░█░█░█▀▀░█▀█░█▀▄░█▀█░█▀▄░░░█▄█░█▀█░█▀▄░█░█░█░░░█▀▀░█▀▀
-- ░▀▄▀░█▀▀░█░█░█░█░█░█░█▀▄░░░█░█░█░█░█░█░█░█░█░░░█▀▀░▀▀█
-- ░░▀░░▀▀▀░▀░▀░▀▀░░▀▀▀░▀░▀░░░▀░▀░▀▀▀░▀▀░░▀▀▀░▀▀▀░▀▀▀░▀▀▀

-- ░█▀▄░█░░░▀█▀░█▀█░█▀▀
-- ░█▀▄░█░░░░█░░█░█░█░█
-- ░▀▀░░▀▀▀░▀▀▀░▀░▀░▀▀▀
local bling = require("module.bling")

-- Flash focus
bling.module.flash_focus.enable()

require("module.rubato")
require("module.layout-machi")
-- require("module.vicious")

-- ░█▀▀░█▀█░█▀█░█▀▀░▀█▀░█▀▀░░░█▄█░█▀█░█▀▄░█░█░█░░░█▀▀░█▀▀
-- ░█░░░█░█░█░█░█▀▀░░█░░█░█░░░█░█░█░█░█░█░█░█░█░░░█▀▀░▀▀█
-- ░▀▀▀░▀▀▀░▀░▀░▀░░░▀▀▀░▀▀▀░░░▀░▀░▀▀▀░▀▀░░▀▀▀░▀▀▀░▀▀▀░▀▀▀
require('module.brightness-osd')
require('module.dynamic-wallpaper')
require('module.exit-screen')
-- require('module.lockscreen')
require('module.notifications')
require('module.quake-terminal')
require('module.titlebar')
require('module.volume-osd')
require("module.better-resize")

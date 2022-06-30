local log = require('utilities.debug').log
log("Enter Module => module/init.lua" )

local awful = require("awful")

-- ░█▀▀░█▀█░█▀█░█▀▀░▀█▀░█▀▀░░░█▄█░█▀█░█▀▄░█░█░█░░░█▀▀░█▀▀
-- ░█░░░█░█░█░█░█▀▀░░█░░█░█░░░█░█░█░█░█░█░█░█░█░░░█▀▀░▀▀█
-- ░▀▀▀░▀▀▀░▀░▀░▀░░░▀▀▀░▀▀▀░░░▀░▀░▀▀▀░▀▀░░▀▀▀░▀▀▀░▀▀▀░▀▀▀
require('module.quake-terminal')
require('module.titlebar')
require('module.dynamic-wallpaper')
require("module.better-resize")

-- ░█░█░█▀▀░█▀█░█▀▄░█▀█░█▀▄░░░█▄█░█▀█░█▀▄░█░█░█░░░█▀▀░█▀▀
-- ░▀▄▀░█▀▀░█░█░█░█░█░█░█▀▄░░░█░█░█░█░█░█░█░█░█░░░█▀▀░▀▀█
-- ░░▀░░▀▀▀░▀░▀░▀▀░░▀▀▀░▀░▀░░░▀░▀░▀▀▀░▀▀░░▀▀▀░▀▀▀░▀▀▀░▀▀▀require("module.vicious")
require("module.bling")
require("module.rubato")
require("module.layout-machi")

require("module.bling.widget.window_switcher").enable({
    filterClients = awful.widget.tasklist.filter.allscreen
})

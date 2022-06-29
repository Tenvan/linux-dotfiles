local log = require('utilities.debug').log
log("Enter Module => configuration/init.lua" )

local awesome, client, screen = awesome, client, screen

-- local awful = require("awful")
-- local gears = require("gears")
-- local naughty = require("naughty")
-- local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
-- local dpi = xresources.apply_dpi
-- local helpers = require("helpers")
-- local hotkeys_popup = require("awful.hotkeys_popup")
-- local apps = require("configuration.apps")

-- Bling Module
local bling = require("module.bling")

-- Layout Machi
local machi = require("module.layout-machi")
beautiful.layout_machi = machi.get_icon()

-- Import configuration stuff
require("configuration.client")
require("configuration.menu")
require("configuration.autostart")
require("configuration.keys")

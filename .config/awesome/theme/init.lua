local log = require('utilities.debug').log
log("Enter Module => theme/init.lua" )

local gtable = require('gears.table')
local default_theme = require('theme.default-theme')
log("Module loaded => default_theme" )

-- PICK THEME HERE
local theme = require('theme.theme')
log("Module loaded => theme.theme" )

local final_theme = {}

gtable.crush(final_theme, default_theme.theme)
log("table crushed => default_theme.theme" )
gtable.crush(final_theme, theme.theme)
log("table crushed => theme.theme" )

default_theme.awesome_overrides(final_theme)
log("overrides => default_theme" )
theme.awesome_overrides(final_theme)
log("overrides => theme" )

return final_theme

log('Enter Module => ' .. ...)

local gtable = require('gears.table')
local beautiful = require('beautiful')

local gtk_variable        = beautiful.gtk.get_theme_variables
local xresources_variable = beautiful.xresources.get_current_theme

-- get default theme (gtk based)
local default_theme = require('theme.default-theme')
-- get custom theme
local theme = require('theme.theme')

local final_theme = {
  gtk_vars = gtk_variable(),
  xres_vars = xresources_variable()
}

gtable.crush(final_theme, default_theme.theme)
gtable.crush(final_theme, theme.theme)

default_theme.awesome_overrides(final_theme)
theme.awesome_overrides(final_theme)

-- dump(final_theme.gtk_vars, 'gtk variables')
-- dump(final_theme.xres_vars, 'xres variables')

return final_theme

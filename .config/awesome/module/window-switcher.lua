-- work in progress

log('Enter Module => ' .. ...)

local awful     = require('awful')
local beautiful = require('beautiful')
local bling     = require('module.bling')

local keys = require('configuration.keys.mod')

local find_clients = require('helpers').find_clients

local modkey = keys.mod_key
local altkey = keys.alt_key

-- modkey or mod4 = super key
local controlkey = keys.control_key
local shiftkey = keys.shift_key
local returnkey = keys.return_key
local escapekey = keys.escape_key
local tabkey = keys.tab_key
local downkey = keys.down_key
local upkey = keys.up_key
local leftkey = keys.left_key
local rightkey = keys.right_key

local switcher = require('module.awesome-switcher')

local switcher_switch = switcher.switch
local switcher_settings = switcher.settings

switcher.settings.preview_box = true -- display preview-box
switcher.settings.preview_box_bg = beautiful.popup_bg -- background color
switcher.settings.preview_box_border = beautiful.popup_border -- border-color
switcher.settings.preview_box_fps = 30 -- refresh framerate
switcher.settings.preview_box_delay = 150 -- delay in ms
switcher.settings.preview_box_title_font = { 'sans', 'italic', 'normal' } -- the font for cairo
switcher.settings.preview_box_title_font_size_factor = 0.8 -- the font sizing factor
switcher.settings.preview_box_title_color = { 250, 250, 250, 1 } -- the font color

switcher.settings.client_opacity = true -- opacity for unselected clients
switcher.settings.client_opacity_value = 0.8 -- alpha-value for any client
switcher.settings.client_opacity_value_in_focus = 0.9 -- alpha-value for the client currently in focus
switcher.settings.client_opacity_value_selected = 1 -- alpha-value for the selected client

return switcher_switch

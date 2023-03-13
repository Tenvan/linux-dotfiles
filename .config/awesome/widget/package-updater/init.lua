local awful = require('awful')
local naughty = require('naughty')
local wibox = require('wibox')
local gears = require('gears')
local watch = awful.widget.watch
local apps = require('configuration.apps')
local icon = require('widget.icon')

local clickable_container = require('widget.clickable-container')

local config_dir = gears.filesystem.get_configuration_dir()
local widget_icon_dir = config_dir .. 'widget/package-updater/icons/'

local update_available = false
local number_of_updates_available = 0
local update_package = nil

local return_button = function()
	local widget = wibox.widget {
		icon({ icon = '📭' }),
		{
			id = 'icon',
			widget = wibox.widget.imagebox,
			image = widget_icon_dir .. 'package.svg',
			resize = true
		},
		layout = wibox.layout.align.horizontal
	}

	local widget_button = wibox.widget {
		{
			widget,
			margins = dpi(7),
			widget = wibox.container.margin
		},
		widget = clickable_container
	}

	widget_button:buttons(
		gears.table.join(
			awful.button(
				{},
				1,
				nil,
				function()
					if update_available then
						awful.spawn(apps.default.package_manager .. ' --updates', false)
					else
						awful.spawn(apps.default.package_manager, false)
					end
				end
			)
		)
	)

	local update_tooltip = awful.tooltip(
		{
			objects = { widget_button },
			mode = 'outside',
			align = 'right',
			margin_leftright = dpi(8),
			margin_topbottom = dpi(8),
			preferred_positions = { 'right', 'left', 'top', 'bottom' }
		}
	)

	connect('system:updates',
		function(packages, count)
			number_of_updates_available = count
			log('Update count:' .. tostring(number_of_updates_available))

			update_package = packages
			-- update_package = tostring(number_of_updates_available) .. ' updates.'

			local icon_name = nil
			if number_of_updates_available ~= nil then
				update_tooltip.text = update_package
				icon_name = 'package-up'
			else
				update_tooltip.text = 'No updates current available.'
				icon_name = 'package'
			end

			widget.icon:set_image(widget_icon_dir .. icon_name .. '.svg')
			collectgarbage('collect')
		end
	)

	return widget_button
end

return return_button

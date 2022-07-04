local log = require('utilities.debug').log
log("Enter Module => configuration/client/signals.lua")

local awesome, client = awesome, client

local awful = require('awful')
local beautiful = require('beautiful')

local notify = require("utilities.notify")
local sound = require("utilities.sound")

local update_client = function(c)
	-- Set client's shape based on its tag's layout and status (floating, maximized, etc.)
	local current_layout = awful.tag.getproperty(c.first_tag, 'layout')
	if current_layout == awful.layout.suit.max and (not c.floating) then
		c.shape = beautiful.client_shape_rectangle
	elseif c.maximized or c.fullscreen then
		c.shape = beautiful.client_shape_rectangle
	elseif (not c.round_corners) then
		c.shape = beautiful.client_shape_rectangle
	else
		c.shape = beautiful.client_shape_rounded
	end
end

-- Signal function to execute when a new client appears.
client.connect_signal(
	'manage',
	function(c)
		-- Focus, raise and activate
		c:emit_signal(
			'request::activate',
			'mouse_enter',
			{
				raise = true
			}
		)

		-- Set the windows at the slave,
		-- i.e. put it at the end of others instead of setting it master.
		if not awesome.startup then
			awful.client.setslave(c)
		end

		if awesome.startup and not c.size_hints.user_position and
			not c.size_hints.program_position then
			-- Prevent clients from being unreachable after screen count changes.
			awful.placement.no_offscreen(c)
		end

		-- Update client shape
		update_client(c)
	end
)

-- Enable sloppy focus, so that focus follows mouse then raises it.
client.connect_signal(
	'mouse::enter',
	function(c)
		c:emit_signal(
			'request::activate',
			'mouse_enter',
			{
				-- raise = true
			}
		)
	end
)

client.connect_signal(
	'focus',
	function(c)
		c.border_color = beautiful.border_focus
	end
)

client.connect_signal(
	'unfocus',
	function(c)
		c.border_color = beautiful.border_normal
	end
)

-- Manipulate client shape on fullscreen/non-fullscreen
client.connect_signal(
	'property::fullscreen',
	function(c)
		if c.fullscreen then
			c.shape = beautiful.client_shape_rectangle
		else
			update_client(c)
		end
	end
)

-- Manipulate client shape on maximized
client.connect_signal(
	'property::maximized',
	function(c)
		local current_layout = awful.tag.getproperty(c.first_tag, 'layout')
		if c.maximized then
			c.shape = beautiful.client_shape_rectangle
		else
			update_client(c)
		end
	end
)

-- Manipulate client shape on floating
client.connect_signal(
	'property::floating',
	function(c)
		local current_layout = awful.tag.getproperty(c.first_tag, 'layout')
		if c.floating and not c.maximized then
			c.shape = beautiful.client_shape_rounded
		else
			if current_layout == awful.layout.suit.max then
				c.shape = beautiful.client_shape_rectangle
			end
		end
	end
)

client.connect_signal('swapped', function(c, source, is_source)
	-- sound('window-switch')
  log("'client:swapped' event raised: " .. c.name)
end)

client.connect_signal('raised', function(c)
	-- sound("window-attention-active")
  log("'client:raised' event raised: " .. c.name)
end)

client.connect_signal('lowered', function(c)
	-- sound("window-attention-inactive")
  log("'client:lowered' event raised: " .. c.name)
end)

client.connect_signal('request::activate', function(c)
	-- notify("Client", "'request::activate' event raised: " .. c.name)
	log("'client:request::activate' event raised: " .. c.name)
end)

client.connect_signal('request::geometry', function(c, context, Additional)
	log("'client:request::geometry' event raised: " .. c.name .. " Context: " .. context)
	-- if context == 'maximized' then
	-- 	notify('Client', 'Maximized: ' .. c.name)
	-- else
	-- 	notify("Client", "'request::geometry' event raised: " .. c.name .. " Context: " .. context)
	-- end
end)

client.connect_signal('property::window', function(c)
	-- notify("Client", "'property::window' event raised: " .. c.name)
	log("'client:property::window' event raised: " .. c.name)
end)

client.connect_signal('property::size', function(c)
	-- notify("Client", "'property::size' event raised: " .. c.name)
	log("'client:property::size' event raised: " .. c.name)
end)

log("Enter Module => " .. ... )

local awesome, client, screen = awesome, client, screen

local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup")
local beautiful = require("beautiful")
local naughty = require("naughty")
local bling = require("module.bling")
local playerctl = bling.signal.playerctl.lib()
local machi = require("module.layout-machi")
local helpers = require("helpers")
local apps = require("configuration.apps")
local QuakeTerminal = require("module.scratchpad").quake

local dpi = beautiful.xresources.apply_dpi

local move_client = require('helpers.client').move_client

local keys  = require('configuration.keys.mod')

local modkey = keys.mod_key
local altkey = keys.alt_key

-- ░█░█░█▀▀░█░█░░░█▄█░█▀█░█▀▄░▀█▀░█▀▀░▀█▀░█▀▀░█▀▄░█▀▀░█▀▀
-- ░█▀▄░█▀▀░░█░░░░█░█░█░█░█░█░░█░░█▀▀░░█░░█▀▀░█▀▄░█▀▀░▀▀█
-- ░▀░▀░▀▀▀░░▀░░░░▀░▀░▀▀▀░▀▀░░▀▀▀░▀░░░▀▀▀░▀▀▀░▀░▀░▀▀▀░▀▀▀
local controlkey = keys.control_key
local shiftkey   = keys.shift_key
local returnkey  = keys.return_key
local spacekey   = keys.space_key
local escapekey  = keys.escape_key
local printkey   = keys.print_key
local tabkey     = keys.tab_key
local downkey    = keys.down_key
local upkey      = keys.up_key
local leftkey    = keys.left_key
local rightkey   = keys.right_key

-- Make key easier to call
----------------------------
local keys  = require('configuration.keys.mod')

local mod = modkey
local alt = altkey
local ctrl = controlkey
local shift = shiftkey

-- key groups
local kgAwesome = 'AwesomeWM'
local kgApps = 'Anwendungen'
local kgMenus = 'Menüs'
local kgClient = 'Client Aktionen'
local kgLayout = 'Layout Aktionen'
local kgMaster = 'Master Aktionen'
local kgScreen = 'Screen Aktionen'
local kgScreenshot = 'Screenshot'
local kgSound = 'Audio'
local kgSystem = 'System'
local kgTag = 'Tags'
local kgTab = 'Tabs'
local kgUtils = 'Tools'
local kgHotkeys = 'Hotkeys'
local kgLauncher = 'Starter'

-- Global key bindings
------------------------
awful.keyboard.append_global_keybindings({

	---- App
	----------

  ---- Client
	---------------

	---- Bling
	-------------
	-- Add client to tabbed layout
	awful.key({ alt }, "a", function()
		bling.module.tabbed.pick_with_dmenu()
	end, { description = "pick client to add to tab group", group = kgTab }),

	-- Remove client from tabbed layout
	awful.key({ alt }, "d", function()
		bling.module.tabbed.pop()
	end, { description = "remove focused client from tabbing group", group = kgTab }),

	-- Cycle through client in tabbed layout
	awful.key({ alt }, "s", function()
		bling.module.tabbed.iter()
	end, { description = "iterate through tabbing group", group = kgTab }),

	---- Hotkeys
	---------------
	-- Toggle Dashboard
	awful.key({ mod, shift }, "d", function()
		dashboard:toggle()
	end, { description = "toggle dashboard", group = kgHotkeys }),

	-- Music player
	awful.key({ mod }, "grave", function()
		awful.spawn.with_shell(apps.default.music_player)
	end, { description = "open music client", group = kgHotkeys }),

	-- Brightness Control
	awful.key({}, "XF86MonBrightnessUp", function()
		awful.spawn("brightnessctl set 5%+ -q")
	end, { description = "increase brightness", group = kgHotkeys }),
	awful.key({}, "XF86MonBrightnessDown", function()
		awful.spawn("brightnessctl set 5%- -q")
	end, { description = "decrease brightness", group = kgHotkeys }),

	--
	-- ░█▀█░█░░░█▀▀░█▀█░░░█░█░█▀█░█░░░█░█░█▄█░█▀▀░░░█▀▀░█▀█░█▀█░▀█▀░█▀▄░█▀█░█░░
	-- ░█▀█░█░░░▀▀█░█▀█░░░▀▄▀░█░█░█░░░█░█░█░█░█▀▀░░░█░░░█░█░█░█░░█░░█▀▄░█░█░█░░
	-- ░▀░▀░▀▀▀░▀▀▀░▀░▀░░░░▀░░▀▀▀░▀▀▀░▀▀▀░▀░▀░▀▀▀░░░▀▀▀░▀▀▀░▀░▀░░▀░░▀░▀░▀▀▀░▀▀▀
	awful.key({}, 'XF86AudioRaiseVolume', function()
		awful.spawn('amixer -D pulse sset Master 5%+', false)
		awesome.emit_signal('widget::volume')
		awesome.emit_signal('module::volume_osd:show', true)
	end, {
		description = 'increase volume up by 5%',
		group = kgHotkeys
	}),
	awful.key({ mod, alt }, 'KP_Add', function()
		awful.spawn('amixer -D pulse sset Master 5%+', false)
		awesome.emit_signal('widget::volume')
		awesome.emit_signal('module::volume_osd:show', true)
	end, {
		description = '+5% Volume',
		group = kgSound
	}),
	awful.key({}, 'XF86AudioLowerVolume', function()
		awful.spawn('amixer -D pulse sset Master 5%-', false)
		awesome.emit_signal('widget::volume')
		awesome.emit_signal('module::volume_osd:show', true)
	end, {
		description = '-5% Volume',
		group = kgHotkeys
	}),
	awful.key({ mod, alt }, 'KP_Subtract', function()
		awful.spawn('amixer -D pulse sset Master 5%-', false)
		awesome.emit_signal('widget::volume')
		awesome.emit_signal('module::volume_osd:show', true)
	end, {
		description = '-5% Volume',
		group = kgSound
	}),
	awful.key({}, 'XF86AudioMute', function()
		awful.spawn('amixer -D pulse set Master 1+ toggle', false)
		awesome.emit_signal('widget::volume')
		awesome.emit_signal('module::volume_osd:show', true)
	end, {
		description = 'toggle mute',
		group = kgHotkeys
	}),
	awful.key({ mod, alt }, 'KP_Multiply', function()
		awful.spawn('amixer -D pulse set Master 1+ toggle', false)
		awesome.emit_signal('widget::volume')
		awesome.emit_signal('module::volume_osd:show', true)
	end, {
		description = 'Mute Volume',
		group = kgSound
	}),
	awful.key({}, 'XF86AudioMicMute', function()
		awful.spawn('amixer set Capture toggle', false)
	end, {
		description = 'mute microphone',
		group = kgHotkeys
	}),

	--
	-- ░█▀█░█░░░█▀█░█░█░█▀▀░█▀▄░░░█▀▀░█▀█░█▀█░▀█▀░█▀▄░█▀█░█░░
	-- ░█▀▀░█░░░█▀█░░█░░█▀▀░█▀▄░░░█░░░█░█░█░█░░█░░█▀▄░█░█░█░░
	-- ░▀░░░▀▀▀░▀░▀░░▀░░▀▀▀░▀░▀░░░▀▀▀░▀▀▀░▀░▀░░▀░░▀░▀░▀▀▀░▀▀▀
	awful.key({}, "XF86AudioPlay", function()
		playerctl:play_pause()
	end, {
		description = "play pause music",
		group = kgHotkeys }),
	awful.key({}, "XF86AudioPrev", function()
		playerctl:previous()
	end, {
		description = "previous music",
		group = kgHotkeys }),
	awful.key({}, "XF86AudioNext", function()
		playerctl:next()
	end, {
		description = "next music",
		group = kgHotkeys }),
	awful.key({}, 'XF86AudioStop', function()
		-- notify('playerctl stop')
		-- awful.spawn('playerctl stop', false)
		playerctl:stop()
	end, {
		description = 'Player Stop',
		group = kgHotkeys
	}),

	-- Color picker
	awful.key({ mod, shift }, "x", function()
		awful.spawn.easy_async_with_shell(apps.utils.color_picker, function() end)
	end, { description = "open color picker", group = kgHotkeys }),

	-- Screenshots
	awful.key({}, "Print", function()
		awful.spawn.easy_async_with_shell(apps.utils.full_screenshot, function() end)
	end, { description = "take a full screenshot", group = kgHotkeys }),

	awful.key({ alt }, "Print", function()
		awful.spawn.easy_async_with_shell(apps.utils.area_screenshot, function() end)
	end, { description = "take a area screenshot", group = kgHotkeys }),

	-- Lockscreen
	awful.key({ mod, alt }, "l", function()
		lock_screen_show()
	end, { description = "lock screen", group = kgHotkeys }),

	-- Exit screen
	awful.key({ mod }, "Escape", function()
		awesome.emit_signal("module::exit_screen:show")
	end, { description = "exit screen", group = kgHotkeys }),
})

-- Client key bindings
------------------------
client.connect_signal("request::default_keybindings", function()
	awful.keyboard.append_client_keybindings({
		-- Move or swap by direction
		awful.key({ mod, shift }, "k", function(c)
			move_client(c, "up")
		end),
		awful.key({ mod, shift }, "j", function(c)
			move_client(c, "down")
		end),
		awful.key({ mod, shift }, "h", function(c)
			move_client(c, "left")
		end),
		awful.key({ mod, shift }, "l", function(c)
			move_client(c, "right")
		end),

		awful.key({ mod, shift }, "Up", function(c)
			move_client(c, "up")
		end),
		awful.key({ mod, shift }, "Down", function(c)
			move_client(c, "down")
		end),
		awful.key({ mod, shift }, "Left", function(c)
			move_client(c, "left")
		end),
		awful.key({ mod, shift }, "Right", function(c)
			move_client(c, "right")
		end),

		-- Relative move client
		awful.key({ mod, shift, ctrl }, "j", function(c)
			c:relative_move(0, dpi(20), 0, 0)
		end),

		awful.key({ mod, shift, ctrl }, "k", function(c)
			c:relative_move(0, dpi(-20), 0, 0)
		end),

		awful.key({ mod, shift, ctrl }, "h", function(c)
			c:relative_move(dpi(-20), 0, 0, 0)
		end),

		awful.key({ mod, shift, ctrl }, "l", function(c)
			c:relative_move(dpi(20), 0, 0, 0)
		end),

		-- Toggle floating
		awful.key({ mod, ctrl }, spacekey, function(c)
			c.fullscreen = false
			c.maximized = false
			c.floating = not c.floating
			c:raise()
		end, {
			description = 'toggle floating',
			group = kgClient
		}),

		-- Toggle fullscreen
		awful.key({ mod }, "f", function()
			client.focus.fullscreen = not client.focus.fullscreen
			client.focus:raise()
		end),

		-- Maximize windows
		awful.key({ mod }, "m", function(c)
			c.maximized = not c.maximized
		end, { description = "toggle maximize", group = kgClient }),
		awful.key({ mod, ctrl }, "m", function(c)
			c.maximized_vertical = not c.maximized_vertical
			c:raise()
		end, { description = "(un)maximize vertically", group = kgClient }),
		awful.key({ mod, shift }, "m", function(c)
			c.maximized_horizontal = not c.maximized_horizontal
			c:raise()
		end, { description = "(un)maximize horizontally", group = kgClient }),

		-- Minimize windows
		awful.key({ mod }, "n", function(c)
			-- The client currently has the input focus, so it cannot be
			-- minimized, since minimized clients can't have the focus.
			c.minimized = true
		end, { description = "minimize", group = kgClient }),

		-- Un-minimize windows
		awful.key({ mod, ctrl }, "n", function()
			local c = awful.client.restore()
			-- Focus restored client
			if c then
				c:emit_signal("request::activate", "key.unminimize", { raise = true })
			end
		end, { description = "restore minimized", group = kgClient }),

		-- Keep on top
		awful.key({ mod }, "p", function(c)
			c.ontop = not c.ontop
		end),

		-- Sticky
		awful.key({ mod, shift }, "p", function(c)
			c.sticky = not c.sticky
		end),

		-- Close window
		awful.key({ mod }, "q", function()
			client.focus:kill()
		end),

		-- Center window
		awful.key({ mod }, "c", function(c)
			awful.placement.centered(c, { honor_workarea = true, honor_padding = true })
		end),

		-- Window switcher
		awful.key({ mod }, "Tab", function()
			awesome.emit_signal("bling::window_switcher::turn_on")
		end),
	})
end)

-- Layout
------------
awful.keyboard.append_global_keybindings({
	-- Set tilling layout
	awful.key({ mod }, "s", function()
		awful.layout.set(awful.layout.suit.tile)
	end, { description = "set tile layout", group = kgLayout }),

	-- Set floating layout
	awful.key({ mod, shift }, "s", function()
		awful.layout.set(awful.layout.suit.floating)
	end, { description = "set floating layout", group = kgLayout }),

	-- Layout machi
	awful.key({ mod }, ".", function()
		machi.default_editor.start_interactive()
	end, { description = "edit the current layout if it is a machi layout", group = kgLayout }),
	awful.key({ mod }, "/", function()
		machi.switcher.start(client.focus)
	end, { description = "switch between windows for a machi layout", group = kgLayout }),

	-- Number of columns
	awful.key({ mod, alt }, "k", function()
		awful.tag.incncol(1, nil, true)
	end, { description = "increase the number of columns", group = kgLayout }),
	awful.key({ mod, alt }, "j", function()
		awful.tag.incncol(-1, nil, true)
	end, { description = "decrease the number of columns", group = kgLayout }),
	awful.key({ mod, alt }, "Up", function()
		awful.tag.incncol(1, nil, true)
	end, { description = "increase the number of columns", group = kgLayout }),
	awful.key({ mod, alt }, "Down", function()
		awful.tag.incncol(-1, nil, true)
	end, { description = "decrease the number of columns", group = kgLayout }),

	-- On the fly padding change
	awful.key({ mod, shift }, "=", function()
		helpers.resize_padding(5)
	end, { description = "add padding", group = kgLayout }),
	awful.key({ mod, shift }, "-", function()
		helpers.resize_padding(-5)
	end, { description = "subtract padding", group = kgLayout }),

	-- On the fly useless gaps change
	awful.key({ mod }, "=", function()
		helpers.resize_gaps(5)
	end, { description = "add gaps", group = kgLayout }),

	awful.key({ mod }, "-", function()
		helpers.resize_gaps(-5)
	end, { description = "subtract gaps", group = kgLayout }),
})

-- Move through workspaces
----------------------------
awful.keyboard.append_global_keybindings({
	awful.key({ mod, alt }, "Left", awful.tag.viewprev, { description = "view previous", group = kgTag }),
	awful.key({ mod, alt }, "Right", awful.tag.viewnext, { description = "view next", group = kgTag }),
	awful.key({
		modifiers = { mod },
		keygroup = "numrow",
		description = "only view tag",
		group = kgTag,
		on_press = function(index)
			local screen = awful.screen.focused()
			local tag = screen.tags[index]
			if tag then
				tag:view_only()
			end
		end,
	}),
	awful.key({
		modifiers = { mod, ctrl },
		keygroup = "numrow",
		description = "toggle tag",
		group = kgTag,
		on_press = function(index)
			local screen = awful.screen.focused()
			local tag = screen.tags[index]
			if tag then
				awful.tag.viewtoggle(tag)
			end
		end,
	}),
	awful.key({
		modifiers = { mod, shift },
		keygroup = "numrow",
		description = "move focused client to tag",
		group = kgTag,
		on_press = function(index)
			if client.focus then
				local tag = client.focus.screen.tags[index]
				if tag then
					client.focus:move_to_tag(tag)
				end
			end
		end,
	}),
})

-- Screen
-----------
--awful.keyboard.append_global_keybindings({
-- No need for these (single screen setup)
--awful.key({ superkey, ctrlkey }, "j", function () awful.screen.focus_relative( 1) end,
--{description = "focus the next screen", group = "screen"}),
--awful.key({ superkey, ctrlkey }, "k", function () awful.screen.focus_relative(-1) end,
--{description = "focus the previous screen", group = "screen"}),
--})

-- Mouse bindings on desktop
------------------------------
local mainmenu = require("configuration.menu").mainmenu

awful.mouse.append_global_mousebindings({

	-- Left click
	awful.button({}, 1, function()
		naughty.destroy_all_notifications()
		if mainmenu then
			mainmenu:hide()
		end
	end),

	-- Right click
	awful.button({}, 3, function()
		mainmenu:toggle()
	end),

	-- Side key
	awful.button({}, 8, awful.tag.viewprev),
	awful.button({}, 9, awful.tag.viewnext),
})

-- Mouse buttons on the client
--------------------------------
client.connect_signal("request::default_mousebindings", function()
	awful.mouse.append_client_mousebindings({
		awful.button({}, 1, function(c)
			c:activate({ context = "mouse_click" })
		end),
		awful.button({ mod }, 1, function(c)
			c:activate({ context = "mouse_click", action = "mouse_move" })
		end),
		awful.button({ mod }, 3, function(c)
			c:activate({ context = "mouse_click", action = "mouse_resize" })
		end),
	})
end)

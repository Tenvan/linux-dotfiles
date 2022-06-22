local awesome, client, screen = awesome, client, screen

local gears = require("gears")
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local naughty = require("naughty")
local bling = require("module.bling")
local playerctl = bling.signal.playerctl.lib()
local machi = require("module.layout-machi")
local helpers = require("helpers")
local apps = require("configuration.apps")

-- Make key easier to call
----------------------------

local modkey = "Mod4"
local altkey = "Mod1"
local controlkey = "Control"
local shiftkey = "Shift"
local spacekey = "space"
local returnkey = 'Return'
local escapekey = 'Escape'
local tabkey = 'Tab'
local downkey = 'Down'
local upkey = 'Up'
local leftkey = 'Left'
local rightkey = 'Right'

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
	-- Terminal
	awful.key({ modkey }, "Return", function()
		awful.spawn(apps.default.terminal)
	end, { description = "open terminal", group = kgApps }),

	-- App launcher
	awful.key({ modkey }, "d", function()
		awful.spawn.with_shell(apps.default.app_launcher)
	end, { description = "open app launcher", group = kgApps }),

	-- Code editor
	awful.key({ modkey, shiftkey }, "e", function()
		awful.spawn(apps.default.code_editor)
	end, { description = "open code editor", group = kgApps }),

	-- File manager
	awful.key({ modkey, shiftkey }, "f", function()
		awful.spawn(apps.default.file_manager)
	end, { description = "open file manager", group = kgApps }),

	-- Web browser
	awful.key({ modkey, shiftkey }, "w", function()
		awful.spawn(apps.default.web_browser)
	end, { description = "open web browser", group = kgApps }),

	awful.key({ modkey }, 'a', function()
        awful.spawn.with_shell('sh ~/.scripts/menu/app-menu.sh')
    end, {
        description = 'Applikations Menü',
        group = kgMenus
    }), 
    awful.key({ modkey }, 'd', function()
        awful.spawn.with_shell('sh ~/.scripts/menu/develop-menu.sh')
    end, {
        description = 'Developer Menü',
        group = kgMenus
    }), 
    awful.key({ modkey }, 'e', function()
        awful.spawn.with_shell('sh ~/.scripts/menu/edit-configs.sh')
    end, {
        description = 'System Edit Menü',
        group = kgMenus
    }), 
    awful.key({ modkey }, 't', function()
        awful.spawn.with_shell('sh ~/.scripts/menu/system-tools.sh')
    end, {
        description = 'System Tools Menü',
        group = kgMenus
    }), 
    awful.key({ modkey }, 'm', function()
        awful.spawn.with_shell('sh ~/.scripts/menu/system-monitor.sh')
    end, {
        description = 'System Monitors Menü',
        group = kgMenus
    }), 
    awful.key({ modkey }, 'x', function()
        awful.spawn.with_shell('sh ~/.scripts/menu/system-menu.sh')
    end, {
        description = 'System Power Menü',
        group = kgMenus
    }), 
   

	---- WM
	---------
	-- Restart awesome
	awful.key({ modkey, controlkey }, "r", awesome.restart, { description = "reload awesome", group = kgAwesome }),

	-- Quit awesome
	awful.key({ modkey, controlkey }, "q", awesome.quit, { description = "quit awesome", group = kgAwesome }),

	-- Show help
	awful.key({ modkey }, "F1", hotkeys_popup.show_help, { description = "show Help", group = kgAwesome }),

	---- Client
	---------------
	-- Focus client by direction
	awful.key({ modkey }, "k", function()
		awful.client.focus.bydirection("up")
		bling.module.flash_focus.flashfocus(client.focus)
	end, { description = "focus up", group = kgClient }),
	awful.key({ modkey }, "j", function()
		awful.client.focus.bydirection("down")
		bling.module.flash_focus.flashfocus(client.focus)
	end, { description = "focus down", group = kgClient }),
	awful.key({ modkey }, "h", function()
		awful.client.focus.bydirection("left")
		bling.module.flash_focus.flashfocus(client.focus)
	end, { description = "focus left", group = kgClient }),
	awful.key({ modkey }, "l", function()
		awful.client.focus.bydirection("right")
		bling.module.flash_focus.flashfocus(client.focus)
	end, { description = "focus right", group = kgClient }),

	awful.key({ modkey }, "Up", function()
		awful.client.focus.bydirection("up")
		bling.module.flash_focus.flashfocus(client.focus)
	end, { description = "focus up", group = kgClient }),
	awful.key({ modkey }, "Down", function()
		awful.client.focus.bydirection("down")
		bling.module.flash_focus.flashfocus(client.focus)
	end, { description = "focus down", group = kgClient }),
	awful.key({ modkey }, "Left", function()
		awful.client.focus.bydirection("left")
		bling.module.flash_focus.flashfocus(client.focus)
	end, { description = "focus left", group = kgClient }),
	awful.key({ modkey }, "Right", function()
		awful.client.focus.bydirection("right")
		bling.module.flash_focus.flashfocus(client.focus)
	end, { description = "focus right", group = kgClient }),

	-- Resize focused client
	awful.key({ modkey, controlkey }, "k", function(c)
		helpers.resize_client(client.focus, "up")
	end, { description = "resize to the up", group = kgClient }),
	awful.key({ modkey, controlkey }, "j", function(c)
		helpers.resize_client(client.focus, "down")
	end, { description = "resize to the down", group = kgClient }),
	awful.key({ modkey, controlkey }, "h", function(c)
		helpers.resize_client(client.focus, "left")
	end, { description = "resize to the left", group = kgClient }),
	awful.key({ modkey, controlkey }, "l", function(c)
		helpers.resize_client(client.focus, "right")
	end, { description = "resize to the right", group = kgClient }),

	awful.key({ modkey, controlkey }, "Up", function(c)
		helpers.resize_client(client.focus, "up")
	end, { description = "resize to the up", group = kgClient }),
	awful.key({ modkey, controlkey }, "Down", function(c)
		helpers.resize_client(client.focus, "down")
	end, { description = "resize to the down", group = kgClient }),
	awful.key({ modkey, controlkey }, "Left", function(c)
		helpers.resize_client(client.focus, "left")
	end, { description = "resize to the left", group = kgClient }),
	awful.key({ modkey, controlkey }, "Right", function(c)
		helpers.resize_client(client.focus, "right")
	end, { description = "resize to the right", group = kgClient }),

	---- Bling
	-------------
	-- Add client to tabbed layout
	awful.key({ altkey }, "a", function()
		bling.module.tabbed.pick_with_dmenu()
	end, { description = "pick client to add to tab group", group = kgTab }),

	-- Remove client from tabbed layout
	awful.key({ altkey }, "d", function()
		bling.module.tabbed.pop()
	end, { description = "remove focused client from tabbing group", group = kgTab }),

	-- Cycle through client in tabbed layout
	awful.key({ altkey }, "s", function()
		bling.module.tabbed.iter()
	end, { description = "iterate through tabbing group", group = kgTab }),

	---- Hotkeys
	---------------
	-- Toggle Dashboard
	awful.key({ modkey, shiftkey }, "d", function()
		dashboard:toggle()
	end, { description = "toggle dashboard", group = kgHotkeys }),

	-- Music player
	awful.key({ modkey }, "grave", function()
		awful.spawn.with_shell(apps.default.music_player)
	end, { description = "open music client", group = kgHotkeys }),

	-- Brightness Control
	awful.key({}, "XF86MonBrightnessUp", function()
		awful.spawn("brightnessctl set 5%+ -q")
	end, { description = "increase brightness", group = kgHotkeys }),
	awful.key({}, "XF86MonBrightnessDown", function()
		awful.spawn("brightnessctl set 5%- -q")
	end, { description = "decrease brightness", group = kgHotkeys }),

	-- Volume control
	awful.key({}, "XF86AudioRaiseVolume", function()
		awful.spawn("pamixer -i 5")
	end, { description = "increase volume", group = kgHotkeys }),
	awful.key({}, "XF86AudioLowerVolume", function()
		awful.spawn("pamixer -d 5")
	end, { description = "decrease volume", group = kgHotkeys }),
	awful.key({}, "XF86AudioMute", function()
		awful.spawn("pamixer -t")
	end, { description = "mute volume", group = kgHotkeys }),

	-- Music
	awful.key({}, "XF86AudioPlay", function()
		playerctl:play_pause()
	end, { description = "play pause music", group = kgHotkeys }),
	awful.key({}, "XF86AudioPrev", function()
		playerctl:previous()
	end, { description = "previous music", group = kgHotkeys }),
	awful.key({}, "XF86AudioNext", function()
		playerctl:next()
	end, { description = "next music", group = kgHotkeys }),

	-- Color picker
	awful.key({ modkey, shiftkey }, "x", function()
		awful.spawn.easy_async_with_shell(apps.utils.color_picker, function() end)
	end, { description = "open color picker", group = kgHotkeys }),

	-- Screenshots
	awful.key({}, "Print", function()
		awful.spawn.easy_async_with_shell(apps.utils.full_screenshot, function() end)
	end, { description = "take a full screenshot", group = kgHotkeys }),

	awful.key({ altkey }, "Print", function()
		awful.spawn.easy_async_with_shell(apps.utils.area_screenshot, function() end)
	end, { description = "take a area screenshot", group = kgHotkeys }),

	-- Lockscreen
	awful.key({ modkey, altkey }, "l", function()
		lock_screen_show()
	end, { description = "lock screen", group = kgHotkeys }),

	-- Exit screen
	awful.key({ modkey }, "Escape", function()
		awesome.emit_signal("module::exit_screen:show")
	end, { description = "exit screen", group = kgHotkeys }),
})

-- Client key bindings
------------------------
client.connect_signal("request::default_keybindings", function()
	awful.keyboard.append_client_keybindings({
		-- Move or swap by direction
		awful.key({ modkey, shiftkey }, "k", function(c)
			helpers.move_client(c, "up")
		end),
		awful.key({ modkey, shiftkey }, "j", function(c)
			helpers.move_client(c, "down")
		end),
		awful.key({ modkey, shiftkey }, "h", function(c)
			helpers.move_client(c, "left")
		end),
		awful.key({ modkey, shiftkey }, "l", function(c)
			helpers.move_client(c, "right")
		end),

		awful.key({ modkey, shiftkey }, "Up", function(c)
			helpers.move_client(c, "up")
		end),
		awful.key({ modkey, shiftkey }, "Down", function(c)
			helpers.move_client(c, "down")
		end),
		awful.key({ modkey, shiftkey }, "Left", function(c)
			helpers.move_client(c, "left")
		end),
		awful.key({ modkey, shiftkey }, "Right", function(c)
			helpers.move_client(c, "right")
		end),

		-- Relative move client
		awful.key({ modkey, shiftkey, controlkey }, "j", function(c)
			c:relative_move(0, dpi(20), 0, 0)
		end),

		awful.key({ modkey, shiftkey, controlkey }, "k", function(c)
			c:relative_move(0, dpi(-20), 0, 0)
		end),

		awful.key({ modkey, shiftkey, controlkey }, "h", function(c)
			c:relative_move(dpi(-20), 0, 0, 0)
		end),

		awful.key({ modkey, shiftkey, controlkey }, "l", function(c)
			c:relative_move(dpi(20), 0, 0, 0)
		end),

		-- Toggle floating
		awful.key({ modkey, controlkey }, spacekey, awful.client.floating.toggle),

		-- Toggle fullscreen
		awful.key({ modkey }, "f", function()
			client.focus.fullscreen = not client.focus.fullscreen
			client.focus:raise()
		end),

		-- Maximize windows
		awful.key({ modkey }, "m", function(c)
			c.maximized = not c.maximized
		end, { description = "toggle maximize", group = kgClient }),
		awful.key({ modkey, controlkey }, "m", function(c)
			c.maximized_vertical = not c.maximized_vertical
			c:raise()
		end, { description = "(un)maximize vertically", group = kgClient }),
		awful.key({ modkey, shiftkey }, "m", function(c)
			c.maximized_horizontal = not c.maximized_horizontal
			c:raise()
		end, { description = "(un)maximize horizontally", group = kgClient }),

		-- Minimize windows
		awful.key({ modkey }, "n", function(c)
			-- The client currently has the input focus, so it cannot be
			-- minimized, since minimized clients can't have the focus.
			c.minimized = true
		end, { description = "minimize", group = kgClient }),

		-- Un-minimize windows
		awful.key({ modkey, controlkey }, "n", function()
			local c = awful.client.restore()
			-- Focus restored client
			if c then
				c:emit_signal("request::activate", "key.unminimize", { raise = true })
			end
		end, { description = "restore minimized", group = kgClient }),

		-- Keep on top
		awful.key({ modkey }, "p", function(c)
			c.ontop = not c.ontop
		end),

		-- Sticky
		awful.key({ modkey, shiftkey }, "p", function(c)
			c.sticky = not c.sticky
		end),

		-- Close window
		awful.key({ modkey }, "q", function()
			client.focus:kill()
		end),

		-- Center window
		awful.key({ modkey }, "c", function()
			awful.placement.centered(c, { honor_workarea = true, honor_padding = true })
		end),

		-- Window switcher
		awful.key({ altkey }, "Tab", function()
			awesome.emit_signal("bling::window_switcher::turn_on")
		end),
	})
end)

-- Layout
------------
awful.keyboard.append_global_keybindings({
	-- Set tilling layout
	awful.key({ modkey }, "s", function()
		awful.layout.set(awful.layout.suit.tile)
	end, { description = "set tile layout", group = kgLayout }),

	-- Set floating layout
	awful.key({ modkey, shiftkey }, "s", function()
		awful.layout.set(awful.layout.suit.floating)
	end, { description = "set floating layout", group = kgLayout }),

	-- Layout machi
	awful.key({ modkey }, ".", function()
		machi.default_editor.start_interactive()
	end, { description = "edit the current layout if it is a machi layout", group = kgLayout }),
	awful.key({ modkey }, "/", function()
		machi.switcher.start(client.focus)
	end, { description = "switch between windows for a machi layout", group = kgLayout }),

	-- Number of columns
	awful.key({ modkey, altkey }, "k", function()
		awful.tag.incncol(1, nil, true)
	end, { description = "increase the number of columns", group = kgLayout }),
	awful.key({ modkey, altkey }, "j", function()
		awful.tag.incncol(-1, nil, true)
	end, { description = "decrease the number of columns", group = kgLayout }),
	awful.key({ modkey, altkey }, "Up", function()
		awful.tag.incncol(1, nil, true)
	end, { description = "increase the number of columns", group = kgLayout }),
	awful.key({ modkey, altkey }, "Down", function()
		awful.tag.incncol(-1, nil, true)
	end, { description = "decrease the number of columns", group = kgLayout }),

	-- On the fly padding change
	awful.key({ modkey, shiftkey }, "=", function()
		helpers.resize_padding(5)
	end, { description = "add padding", group = kgLayout }),
	awful.key({ modkey, shiftkey }, "-", function()
		helpers.resize_padding(-5)
	end, { description = "subtract padding", group = kgLayout }),

	-- On the fly useless gaps change
	awful.key({ modkey }, "=", function()
		helpers.resize_gaps(5)
	end, { description = "add gaps", group = kgLayout }),

	awful.key({ modkey }, "-", function()
		helpers.resize_gaps(-5)
	end, { description = "subtract gaps", group = kgLayout }),
})

-- Move through workspaces
----------------------------
awful.keyboard.append_global_keybindings({
	awful.key({ modkey, altkey }, "Left", awful.tag.viewprev, { description = "view previous", group = kgTag }),
	awful.key({ modkey, altkey }, "Right", awful.tag.viewnext, { description = "view next", group = kgTag }),
	awful.key({
		modifiers = { modkey },
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
		modifiers = { modkey, controlkey },
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
		modifiers = { modkey, shiftkey },
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
awful.mouse.append_global_mousebindings({

	-- Left click
	awful.button({}, 1, function()
		naughty.destroy_all_notifications()
		if mymainmenu then
			mymainmenu:hide()
		end
	end),

	-- Right click
	awful.button({}, 3, function()
		mymainmenu:toggle()
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
		awful.button({ modkey }, 1, function(c)
			c:activate({ context = "mouse_click", action = "mouse_move" })
		end),
		awful.button({ modkey }, 3, function(c)
			c:activate({ context = "mouse_click", action = "mouse_resize" })
		end),
	})
end)

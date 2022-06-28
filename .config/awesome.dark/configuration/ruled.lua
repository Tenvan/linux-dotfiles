local gears = require("gears")
local awful = require("awful")
local beautiful = require("beautiful")
local ruled = require("ruled")
local helpers = require("helpers")

-- Get screen geometry
local screen_width = awful.screen.focused().geometry.width
local screen_height = awful.screen.focused().geometry.height

ruled.client.connect_signal("request::rules", function()
	-- Global
	ruled.client.append_rule({
		id = "global",
		rule = {},
		properties = {
			raise = true,
			floating = true,
			maximized = false,
			above = false,
			below = false,
			ontop = false,
			sticky = false,
			maximized_horizontal = false,
			maximized_vertical = false,

			honor_workarea = true,
			honor_padding = true,

			screen = awful.screen.preferred,
			-- screen = awful.screen.focused,
			focus = awful.client.focus.filter,
			titlebars_enabled = beautiful.titlebar_enabled,
			placement = awful.placement.no_overlap + awful.placement.no_offscreen,
		},
	})

	-- Tasklist order
	ruled.client.append_rule({
		id = "tasklist_order",
		rule = {},
		properties = {},
		callback = awful.client.setslave,
	})

	-- Titlebar rules
	ruled.client.append_rule({
		id = "titlebars",
		rule_any = {
			class = {
				"Spotify",
				"Org.gnome.Nautilus",
			},
		},
		properties = {
			titlebars_enabled = false,
		},
	})

	-- Float
	ruled.client.append_rule({
		id = "floating",
		rule_any = {
			instance = {
				"Devtools", -- Firefox devtools
			},
			class = {
				"Lxappearance",
				"Nm-connection-editor",
			},
			name = {
				"Event Tester", -- xev
			},
			role = {
				"AlarmWindow",
				"pop-up",
				"GtkFileChooserDialog",
				"conversation",
			},
			type = {
				"dialog",
			},
		},
		properties = { floating = true, placement = helpers.centered_client_placement },
	})

	-- Centered
	ruled.client.append_rule({
		id = "centered",
		rule_any = {
			type = {
				"dialog",
			},
			class = {
				-- "discord",
			},
			role = {
				"GtkFileChooserDialog",
				"conversation",
			},
		},
		properties = { placement = helpers.centered_client_placement },
	})

	-- Terminal emulators
	ruled.client.append_rule {
		id = 'terminals',
		rule_any = {
			class = { 'kitty', 'alacritty', 'termite', 'URxvt', 'XTerm', 'UXTerm', 'K3rmit' }
		},
		properties = {
			tag = '1',
			floating = false,
			maximized = false,
			switch_to_tags = true,
			size_hints_honor = false,
			titlebars_enabled = true
		}
	}

	-- Music clients (usually a terminal running ncmpcpp)
	ruled.client.append_rule({
		rule_any = {
			class = {
				"music",
			},
			instance = {
				"music",
			},
		},
		properties = {
			floating = true,
			width = screen_width * 0.34,
			height = screen_height * 0.32,
			placement = helpers.centered_client_placement,
		},
	})

	-- Teams Hauptfenster
	ruled.client.append_rule {
		id = 'internet',
		rule = {
			class = 'teams-for-linux',
			type = 'normal'
		},
		properties = {
			maximized = false,
			floating = false,
			screen = 2,
			tag = '4',
			switch_to_tags = true
		}
	}

	-- Image viewers
	ruled.client.append_rule({
		rule_any = {
			class = {
				"feh",
				"imv",
				'Pqiv',
				'Sxiv'
			},
		},
		properties = {
			floating = true,
			width = screen_width * 0.7,
			height = screen_height * 0.75,
		},
		callback = function(c)
			awful.placement.centered(c, { honor_padding = true, honor_workarea = true })
		end,
	})

	-- Browsers and chats
	ruled.client.append_rule {
		rule_any = {
			class = { 'firefox', 'Vivaldi*', 'Tor Browser', 'discord', 'Chromium', '*chrome*', 'TelegramDesktop' }
		},
		properties = {
			tag = '2'
		}
	}

	-- Text editors and word processing
	ruled.client.append_rule {
		rule_any = {
			class = { 'Geany', 'Atom', 'Subl3', 'code-oss' },
			name = { 'LibreOffice', 'libreoffice' }
		},
		properties = {
			tag = '6'
		}
	}

	-- File managers
	ruled.client.append_rule {
		rule_any = {
			class = { 'dolphin', 'ark', 'Nemo', 'File-roller' }
		},
		properties = {
			tag = '5',
			switch_to_tags = true
		}
	}

	-- Multimedia
	ruled.client.append_rule {
		rule_any = {
			class = { 'vlc', 'Spotify', 'Shortwave' }
		},
		properties = {
			tag = '5',
			screen = 2,
			switch_to_tags = true,
			floating = false,
			placement = awful.placement.centered
		}
	}
	-- Multimedia Editing
	ruled.client.append_rule {
		rule_any = {
			class = { 'Gimp-2.10', 'Inkscape', 'Flowblade' }
		},
		properties = {
			tag = '4'
		}
	}

	-- Sandboxes and VMs
	ruled.client.append_rule {
		rule_any = {
			class = { 'Virt-manager', 'VirtualBox Manage', 'VirtualBox Machine', 'Gnome-boxes', 'Virt-manager' }
		},
		properties = {
			tag = '5',
			switch_to_tags = true
		}
	}

	-- IDEs and Tools
	ruled.client.append_rule {
		rule_any = {
			class = { 'jetbrains-.*', 'Code', 'Oomox', 'Unity', 'UnityHub', 'Ettercap', 'scrcpy' }
		},
		properties = {
			tag = '1',
			skip_decoration = true
		}
	}

	-- Alle Develop Consolen auf Screen 2 tag 2 schieben
	ruled.client.append_rule {
		rule_any = {
			name = { 'OT.:*' }
		},
		properties = {
			screen = 2,
			tag = '2',
			switch_to_tags = true,
			maximized = false,
			floating = false
		}
	}
	-- OTW Develop Consolen auf Screen 2 tag 3 schieben
	ruled.client.append_rule {
		rule_any = {
			name = { 'OTW:*' }
		},
		properties = {
			screen = 2,
			tag = '3',
		}
	}

	-- System Monitor Consolen auf Screen 2 tag 9 schieben
	ruled.client.append_rule {
		rule_any = {
			name = { 'SysMon:*', 'Sys:*', 'CF:*' },
			class = { 'Gnome-system-monitor' }
		},
		properties = {
			tag = '9',
			screen = 2,
			floating = false,
			switch_to_tags = true
		}
	}

	-- Mpv
	ruled.client.append_rule({
		rule = { class = "mpv" },
		properties = {},
		callback = function(c)
			-- make it floating, ontop and move it out of the way if the current tag is maximized
			if awful.layout.get(awful.screen.focused()) == awful.layout.suit.floating then
				c.floating = true
				c.ontop = true
				c.width = screen_width * 0.30
				c.height = screen_height * 0.35
				awful.placement.bottom_right(c, {
					honor_padding = true,
					honor_workarea = true,
					margins = { bottom = beautiful.useless_gap * 2, right = beautiful.useless_gap * 2 },
				})
				awful.titlebar.hide(c, beautiful.titlebar_pos)
			end

			-- restore `ontop` after fullscreen is disabled
			c:connect_signal("property::fullscreen", function()
				if not c.fullscreen then
					c.ontop = true
				end
			end)
		end,
	})
end)

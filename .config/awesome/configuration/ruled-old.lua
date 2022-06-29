-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = { -- All clients will match this rule.
    {
        rule = {},
        properties = {
            border_width = beautiful.border_width,
            border_color = beautiful.border_normal,
            focus = awful.client.focus.filter,
            raise = true,
            keys = clientkeys,
            buttons = clientbuttons,
            screen = awful.screen.preferred,
            placement = awful.placement.no_overlap + awful.placement.no_offscreen,
            size_hints_honor = false,
            -- float first
            maximized = false,
            floating = true
        }
    }, -- Toolbars
    {
        rule_any = {
            class = { 'Polybar', 'Plank' }
        },
        properties = {
            border_width = 0
        }
    }, -- Titlebars
    {
        rule_any = {
            type = { 'dialog', 'normal' }
        },
        properties = {
            titlebars_enabled = true
        }
    }, -- Set applications to be maximized at startup.
    -- find class or role via xprop command
    {
        rule_any = {
            class = {}
        },
        properties = {
            border_width = 0,
            titlebars_enabled = false,
            maximized = false,
            floating = false
        }
    }, {
        rule = {
            class = 'Geany'
        },
        properties = {
            maximized = false
        }
    }, {
        rule = {
            class = 'Geany',
            type = 'dialog'
        },
        properties = {
            maximized = false
        }
    }, -- Maximized clients.
    {
        rule = {
            class = 'Gimp*',
            role = 'gimp-image-window'
        },
        properties = {
            maximized = true,
            floating = false
        }
    }, {
        rule_any = {
            instance = {},
            class = { 'inkscape', 'Mailspring', 'VirtualBox Machine', 'Vlc' },
            role = {}
        },
        properties = {
            maximized = true,
            floating = false
        }
    }, -- Non-Floating clients.
    {
        rule_any = {
            instance = {},
            class = { 'Alacritty', 'kitty', 'Thunar', 'Nemo' },
            name = {},
            role = {}
        },
        properties = {
            maximized = false,
            floating = false
        }
    }, -- Floating clients but centered in screen
    {
        rule_any = {
            class = { '.*gnome-authentication-agent.*', 'Gcr-prompter', 'Rofi' }
        },
        properties = {
            floating = true,
            opacity = 0.8
        },
        callback = function(c)
            awful.placement.centered(c, nil)
        end
    }, -- Floating clients (overide non-floating rules bevor this)
    {
        rule_any = {
            instance = {},
            class = {},
            name = { 'Wetter:.*', 'XBindKey: Hit a key' },
            role = { 'pop-up', 'Preferences', 'setup' }
        },
        properties = {
            floating = true
        }
    }, -- Special applications mappings
    {
        rule_any = {
            class = { 'Pavucontrol' }
        },
        properties = {
            screen = 2,
            tag = awful.util.tagnames[3],
            ontop = true,
            switchtotag = true,
            maximized = false,
            floating = true
        }
    }, -- Audio Clients
    {
        rule_any = {
            class = { 'Spotify', 'Psst-gui', 'Shortwave' }
        },
        properties = {
            screen = 2,
            tag = awful.util.tagnames[5],
            switchtotag = true,
            maximized = false,
            floating = false
        }
    }, -- Steam
    {
        rule_any = {
            class = { 'RuneScape' }
        },
        properties = {
            border_width = 5,
            titlebars_enabled = false,
            maximized = true,
            floating = false,
            screen = 1,
            tag = awful.util.tagnames[7],
            switchtotag = true
        }
    }, {
        rule_any = {
            class = { 'Steam' }
        },
        properties = {
            border_width = 5,
            titlebars_enabled = false,
            maximized = false,
            floating = false,
            screen = 1,
            tag = awful.util.tagnames[7],
            switchtotag = true
        }
    }, {
        rule = {
            class = 'Code'
        },
        properties = {
            screen = 1,
            tag = awful.util.tagnames[9],
            switchtotag = true,
            maximized = false,
            floating = false
        }
    }, {
        rule_any = {
            name = { 'AWMTT' }
        },
        properties = {
            screen = 2,
            tag = awful.util.tagnames[6],
            switchtotag = true,
            maximized = false,
            floating = false
        }
    }, -- Set applications to always map on the tag 5 (Virtual/RDP) on screen 1.
    {
        rule_any = {
            class = { 'Virt-manager' }
        },
        properties = {
            screen = 1,
            tag = awful.util.tagnames[5],
            switchtotag = false
        }
    }, -- Set applications to always map on the tag 6 (RemoteDesktops) on screen 1.
    {
        rule_any = {
            class = { 'org.remmina.Remmina' }
        },
        properties = {
            screen = 1,
            tag = awful.util.tagnames[6],
            switchtotag = true
        }
    }, {
        rule_any = {
            class = { 'RemoteDesktopManager.*' }
        },
        properties = {
            screen = 1,
            tag = awful.util.tagnames[6],
            -- maximized = false,
            -- floating = false,
            switchtotag = true
        }
    }, -- Set applications to always map on the tag 9 (Systemtools) on screen 1.
    {
        rule_any = {
            class = { editorgui }
        },
        properties = {
            screen = 1,
            tag = awful.util.tagnames[9],
            switchtotag = true
        }
    }, -- Special handled applications
    {
        -- FreeCAD
        rule = {
            class = 'FreeCAD'
        },
        properties = {
            maximized = false,
            floating = false,
            screen = 1,
            tag = awful.util.tagnames[7],
            switchtotag = true
        }
    }, {
        -- Fusion 360
        rule = {
            class = 'fusion360.exe'
        },
        properties = {
            border_width = 0,
            titlebars_enabled = false,
            screen = 1,
            tag = awful.util.tagnames[7],
            switchtotag = true
        }
    }, {
        -- JetBrains InteliJ Hauptfenster
        rule = {
            class = 'jetbrains-.*',
            type = 'dialog'
        },
        properties = {
            maximized = false,
            float = true,
            opacity = 0.8
        }
    }, {
        -- Teams Hauptfenster
        rule = {
            class = 'teams-for-linux',
            type = 'normal'
        },
        properties = {
            maximized = false,
            floating = false,
            screen = 2,
            tag = awful.util.tagnames[4],
            switchtotag = false
        }
    }, {
        -- Teams Messagebox
        rule = {
            class = 'teams-for-linux',
            name = '.*Benachrichtigung.*'
            -- ~ type = "notification",
            -- name = "Microsoft Teams-Benachrichtigung"
        },
        properties = {
            maximized = false,
            floating = true,
            screen = 1,
            tag = awful.util.tagnames[1],
            switchtotag = true -- zur Nachricht springen
        }
    }, -- Alle Develop Consolen auf Screen 2 tag 2 schieben
    {
        rule_any = {
            name = { 'OT.:*' }
        },
        properties = {
            screen = 2,
            tag = awful.util.tagnames[2],
            switchtotag = true,
            maximized = false,
            floating = false
        }
    }, -- OTW Develop Consolen auf Screen 2 tag 3 schieben
    {
        rule_any = {
            name = { 'OTW:*' }
        },
        properties = {
            screen = 2,
            tag = awful.util.tagnames[3],
            switchtotag = true,
            maximized = false,
            floating = false
        }
    }, {
        rule_any = {
            class = { 'URxtv', 'XTerm' }
            -- instance = {
            --     "urxtv",
            --     "xterm"
            -- }
        },
        properties = {
            screen = 2,
            tag = awful.util.tagnames[2],
            switchtotag = true,
            maximized = false,
            floating = false
        }
    }, -- Chromium Debugger Instanz auf Screen 2 tag 2 schieben
    {
        rule = {
            class = 'Chromium',
            instance = 'chromium *'
        },
        properties = {
            floating = false,
            tag = awful.util.tagnames[1],
            switchtotag = false
        }
    }, -- Firefox Develop Edition auf Screen 2 tag 2 schieben
    {
        rule_any = {
            class = { 'Google-chrome', 'Vivaldi*', 'firefox', 'firefoxdeveloperedition' }
        },
        properties = {
            maximized = false,
            floating = false,
            tag = awful.util.tagnames[1],
            switchtotag = false
        }
    }, -- System Monitor Consolen auf Screen 2 tag 9 schieben
    {
        rule_any = {
            name = { 'SysMon:*', 'Sys:*', 'CF:*' },
            class = { 'Gnome-system-monitor' }
        },
        properties = {
            screen = 2,
            tag = awful.util.tagnames[9],
            floating = false,
            switchtotag = true
        }
    }, -- Finishings
    -- Dialogs
    {
        rule_any = {
            type = { 'dialog' }
        },
        properties = {
            maximized = false,
            floating = true
        }
    } }
-- }}}


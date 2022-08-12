log('Enter Module => ' .. ...)

local client = client

local awful = require('awful')
local ruled = require('ruled')
local beautiful = require('beautiful')
local client_keys = require('configuration.client.keys')
local client_buttons = require('configuration.client.buttons')
local apps = require('configuration.apps')

ruled.client.connect_signal('request::rules', function()
  -- All clients will match this rule.
  ruled.client.append_rule {
    rule = {},
    properties = {
      focus = awful.client.focus.filter,
      raise = true,
      floating = true,
      maximized = false,
      above = false,
      below = false,
      ontop = false,
      sticky = false,
      maximized_horizontal = false,
      maximized_vertical = false,
      keys = client_keys,
      buttons = client_buttons,
      screen = awful.screen.preferred,
      placement = awful.placement.no_overlap + awful.placement.no_offscreen
    }
  }

  ruled.client.append_rule {
    rule_any = {
      type = { 'normal', 'dialog' }
    },
    except_any = {
      name = { 'Discord Updater' }
    },
    properties = {
      round_corners = true,
      shape = beautiful.client_shape_rounded
    }
  }

  -- Titlebar rules
  ruled.client.append_rule {
    rule_any = {
      type = { 'normal', 'dialog', 'modal', 'utility' }
    },
    properties = {
      titlebars_enabled = true
    }
  }

  -- Dialogs
  ruled.client.append_rule {
    rule_any = {
      type = { 'dialog' },
      class = { 'Wicd-client.py' }
    },
    properties = {
      titlebars_enabled = true,
      floating = true,
      above = true,
      skip_decoration = true,
      placement = awful.placement.centered
    }
  }

  -- Modals
  ruled.client.append_rule {
    rule_any = {
      type = { 'modal' }
    },
    properties = {
      titlebars_enabled = true,
      floating = true,
      above = true,
      skip_decoration = true,
      placement = awful.placement.centered
    }
  }

  -- Utilities
  ruled.client.append_rule {
    rule_any = {
      type = { 'utility' },
      class = { '.*gnome-authentication-agent.*', 'Gcr-prompter' }
    },
    properties = {
      titlebars_enabled = false,
      floating = true,
      skip_decoration = true,
      placement = awful.placement.centered
    }
  }

  -- Splash
  ruled.client.append_rule {
    rule_any = {
      type = { 'splash' },
      name = { 'Discord Updater' }
    },
    properties = {
      titlebars_enabled = false,
      round_corners = true,
      floating = true,
      above = true,
      skip_decoration = true,
      placement = awful.placement.centered
    }
  }

  --   -- Terminal emulators
  ruled.client.append_rule {
    rule_any = {
      class = { apps.default.terminal, 'URxvt', 'XTerm', 'UXTerm', 'kitty', 'K3rmit', 'termite' }
    },
    properties = {
      size_hints_honor = false,
      titlebars_enabled = true,
      maximized = false,
      floating = false,
    }
  }

  -- Browsers and chats
  ruled.client.append_rule {
    rule_any = {
      class = { 'firefox', 'Tor Browser', 'discord', 'Chromium', 'Google-chrome', 'TelegramDesktop' }
    },
    properties = {
      screen = 2,
      tag = screen[2].tags[1],
      maximized = false,
      floating = false
    }
  }

  -- Main Browser
  ruled.client.append_rule {
    rule = {
      instance = apps.default.web_browser
    },
    properties = {
      screen = screen.primary,
      tag = screen[screen.primary].tags[8],
      maximized = false,
      floating = false
    }
  }

  ruled.client.append_rule {
    rule = {
      instance = apps.default.web_browser .. ' .*'
    },
    properties = {
      screen = 2,
    }
  }

  -- Text editors and word processing
  ruled.client.append_rule {
    rule_any = {
      class = { 'Code', 'Geany', 'Atom', 'Subl3', 'code-oss' },
      name = { 'LibreOffice', 'libreoffice' }
    },
    properties = {
      screen = screen[1],
      tag = screen[1].tags[9],
      maximized = false,
      floating = false
    }
  }

  -- File managers
  ruled.client.append_rule {
    rule_any = {
      class = { 'dolphin', 'ark', 'Nemo', 'File-roller' }
    },
    properties = {
      screen = screen.primary,
      tag = screen[screen.primary].tags[7],
      switch_to_tags = true
    }
  }

  -- Multimedia
  ruled.client.append_rule {
    rule_any = {
      class = { 'vlc', 'Spotify', 'shortwave' }
    },
    properties = {
      screen = 2,
      tag = screen[2].tags[5],
      switch_to_tags = true,
      floating = false
    }
  }

  -- Gaming
  ruled.client.append_rule {
    rule_any = {
      class = { 'Wine', 'dolphin-emu', 'Steam', 'Citra', 'supertuxkart' },
      name = { 'Steam' }
    },
    properties = {
      screen = screen.primary,
      tag = screen[screen.primary].tags[7],
      switch_to_tags = true,
      skip_decoration = true,
      placement = awful.placement.centered
    }
  }

  -- Multimedia Editing
  ruled.client.append_rule {
    rule_any = {
      class = { 'Gimp-2.10', 'Inkscape', 'Flowblade' }
    },
    properties = {
      screen = screen.primary,
      tag = screen[screen.primary].tags[4],
    }
  }

  -- Sandboxes and VMs
  ruled.client.append_rule {
    rule_any = {
      class = { 'Virt-manager', 'VirtualBox Manage', 'VirtualBox Machine', 'Gnome-boxes' }
    },
    properties = {
      screen = screen.primary,
      tag = screen[screen.primary].tags[5],
      switch_to_tags = true
    }
  }

  -- Remmina and other Remotes
  ruled.client.append_rule {
    rule_any = {
      class = { 'org.remmina.Remmina' }
    },
    properties = {
      screen = screen.primary,
      tag = screen[screen.primary].tags[5],
      floating = false,
      switch_to_tags = true
    }
  }

  -- IDEs
  ruled.client.append_rule {
    rule = {
      class = 'jetbrains-.*'
    },
    except_any = {
      name = { 'splash', 'Welcome *' }
    },
    properties = {
      screen = screen.primary,
      tag = screen[screen.primary].tags[1],
      skip_decoration = true,
      maximized = false,
      floating = false
    }
  }

  ruled.client.append_rule {
    rule = {
      class = 'jetbrains-.*',
      type  = 'dialog',
    },
    properties = {
      skip_decoration = false,
      focus = true,
      floating = true
    }
  }

  -- System Tools
  ruled.client.append_rule {
    rule_any = {
      class = { 'Oomox', 'Unity', 'UnityHub', 'Ettercap', 'scrcpy' }
    },
    properties = {
      screen = screen.primary,
      tag = screen[screen.primary].tags[9],
      skip_decoration = true
    }
  }

  -- Alle Develop Apps auf Screen 2 tag 2 schieben
  ruled.client.append_rule {
    rule_any = {
      name = { 'OT.:*' },
    },
    properties = {
      screen = 2,
      tag = screen[2].tags[2],
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
      tag = screen[2].tags[3],
    }
  }

  -- -- System Monitor Consolen auf Screen 2 tag 9 schieben
  ruled.client.append_rule {
    rule_any = {
      name = { 'SysMon:*', 'Sys:*', 'CF:*' },
      class = { 'Gnome-system-monitor' }
    },
    properties = {
      screen = 2,
      tag = screen[2].tags[9],
      switch_to_tags = true,
      maximized = false,
      floating = false
    }
  }

  -- Image viewers
  ruled.client.append_rule {
    rule_any = {
      class = { 'feh', 'Pqiv', 'Sxiv' }
    },
    properties = {
      titlebars_enabled = true,
      skip_decoration = true,
      floating = true,
      ontop = true,
      placement = awful.placement.centered
    }
  }

  -- Teams Hauptfenster
  ruled.client.append_rule {
    rule_any = {
      class = { 'teams-for-linux', 'Microsoft Teams*' },
    },
    properties = {
      screen = 2,
      tag = screen[2].tags[4],
      switch_to_tags = true,
      maximized = false,
      floating = false,
    }
  }


  -- Floating
  ruled.client.append_rule {
    rule_any = {
      instance = { 'file_progress', 'Popup', 'nm-connection-editor' },
      class = { 'scrcpy', 'Mugshot', 'Pulseeffects', 'Xfce4-appearance-settings' },
      role = { 'AlarmWindow', 'ConfigManager', 'pop-up' }
    },
    properties = {
      titlebars_enabled = true,
      skip_decoration = true,
      ontop = true,
      floating = true,
      focus = awful.client.focus.filter,
      raise = true,
      keys = client_keys,
      buttons = client_buttons,
      placement = awful.placement.centered
    }
  }
end)

-- Normally we'd do this with a rule, but some program like spotify doesn't set its class or name
-- until after it starts up, so we need to catch that signal.
---comment
---@param c any
client.connect_signal('property::class', function(c)
  log('spawn::property::class')
  dump({
    name = c.name,
    class = c.class,
    instance = c.instance,
    type = c.type,
    role = c.role,
  }, 'client:properties')

  if c.class == 'Spotify' then
    log('Spotify detected')

    local window_mode = false

    -- Check if fullscreen or window mode
    if c.fullscreen then
      window_mode = false
      c.fullscreen = false
    else
      window_mode = true
    end

    -- Check if Spotify is already open
    local app = function(c)
      return ruled.client.match(c, {
        class = 'Spotify'
      })
    end

    local app_count = 0
    for c in awful.client.iterate(app) do
      app_count = app_count + 1
    end

    -- If Spotify is already open, don't open a new instance
    if app_count > 1 then
      c:kill()
      -- Switch to previous instance
      for c in awful.client.iterate(app) do
        c:jump_to(false)
      end
    else
      -- Move the instance to specified tag on this screen
      local t = awful.tag.find_by_name(screen[2], '5')
      c:move_to_tag(t)
      c:move_to_screen(2)

      t:view_only()

      -- Fullscreen mode if not window mode
      if not window_mode then
        c.fullscreen = true
      else
        c.floating = false
        awful.placement.centered(c, {
          honor_workarea = true
        })
      end
    end
  end
end)

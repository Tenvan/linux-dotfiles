---
--- Generated by EmmyLua(https://github.com/EmmyLua)
--- Created by stira.
--- DateTime: 13.05.20 11:14
---

-- Standard Definitions
require("definitions")

-- awesome-wm-widgets
local battery_widget   = require("awesome-wm-widgets.battery-widget.battery")
local cpu_widget       = require("awesome-wm-widgets.cpu-widget.cpu-widget")

-- Standard awesome library
local gears            = require("gears")
local awful            = require("awful")

-- Widget and layout library
local wibox            = require("wibox")

-- Theme handling library
local beautiful        = require("beautiful")

-- Keyboard map indicator and switcher
myKeyboardLayout       = awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a textclock widget
mytextclock            = wibox.widget.textclock()

-- Create a wibox for each screen and add it
local taglist_buttons  = gears.table.join(
  awful.button({}, 1, function(t)
    t:view_only()
  end),
  awful.button({ modkey }, 1, function(t)
    if client.focus then
      client.focus:move_to_tag(t)
    end
  end), awful.button({}, 3, awful.tag.viewtoggle),
  awful.button({ modkey }, 3, function(t)
    if client.focus then
      client.focus:toggle_tag(t)
    end
  end), awful.button({}, 4, function(t)
    awful.tag.viewnext(t.screen)
  end),
  awful.button({}, 5, function(t)
    awful.tag.viewprev(t.screen)
  end))

local tasklist_buttons = gears.table.join(
  awful.button({}, 1, function(c)
    if c == client.focus then
      c.minimized = true
    else
      c:emit_signal("request::activate", "tasklist", { raise = true })
    end
  end), awful.button({}, 3, function()
    awful.menu.client_list({ theme = { width = 250 } })
  end), awful.button({}, 4, function()
    awful.client.focus.byidx(1)
  end),
  awful.button({}, 5, function()
    awful.client.focus.byidx(-1)
  end))

local function set_wallpaper(s)
  -- Wallpaper
  if beautiful.wallpaper then
    local wallpaper = beautiful.wallpaper
    -- If wallpaper is a function, call it with the screen
    if type(wallpaper) == "function" then
      wallpaper = wallpaper(s)
    end
    gears.wallpaper.maximized(wallpaper, s, true)
  end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
  -- Wallpaper
  set_wallpaper(s)

  tag_Develop    = "1:  Develop"
  tag_DevConsole = "2:  Develop Consolen"
  tag_Divers     = "3:  Sonstiges"
  tag_Teams      = "4:  Teams"
  tag_VM         = "5:  VirtualBox"
  tag_Web        = "6:  Web"
  tag_Media      = "7:  Audio /  Video"
  tag_Admin      = "8:  Admin"
  tag_Status     = "9:  Status"

  -- Each screen has its own tag table.
  awful.tag({
              tag_Develop, tag_DevConsole, tag_Divers, tag_Teams, tag_VM, tag_Web,
              tag_Media, tag_Admin, tag_Status
            }, s, awful.layout.layouts[2])

  -- Create a promptbox for each screen
  s.mypromptbox = awful.widget.prompt()
  -- Create an imagebox widget which will contain an icon indicating which layout we're using.
  -- We need one layoutbox per screen.
  s.mylayoutbox = awful.widget.layoutbox(s)
  s.mylayoutbox:buttons(gears.table.join(
    awful.button({}, 1,
                 function()
                   awful.layout.inc(1)
                 end),
    awful.button({}, 3,
                 function()
                   awful.layout.inc(-1)
                 end), awful.button({}, 4, function()
      awful.layout.inc(1)
    end),
    awful.button({}, 5,
                 function()
                   awful.layout.inc(-1)
                 end)))
  -- Create a taglist widget
  s.mytaglist  = awful.widget.taglist {
    screen  = s,
    filter  = awful.widget.taglist.filter.all,
    buttons = taglist_buttons
  }

  -- Create a tasklist widget
  s.mytasklist = awful.widget.tasklist {
    screen  = s,
    filter  = awful.widget.tasklist.filter.currenttags,
    buttons = tasklist_buttons
  }

  -- Create the wibox
  s.mywibox    = awful.wibar({ position = "top", screen = s })

  -- Add widgets to the wibox
  s.mywibox:setup {
    layout = wibox.layout.align.horizontal,
    { -- Left widgets
      layout = wibox.layout.fixed.horizontal,
      mylauncher,
      s.mytaglist,
      s.mypromptbox
    },
    s.mytasklist, -- Middle widget
    { -- Right widgets
      layout = wibox.layout.fixed.horizontal,
      cpu_widget({
                   width        = 120,
                   step_width   = 2,
                   step_spacing = 0,
                   color        = '#434c5e'
                 }),
      myKeyboardLayout,
      wibox.widget.systray(),
      mytextclock,
      --battery_widget(),
      s.mylayoutbox
    }
  }
end)
-- }}}

---
--- Generated by EmmyLua(https://github.com/EmmyLua)
--- Created by stira.
--- DateTime: 13.05.20 09:46
---

-- Standard awesome library
local gears      = require("gears")
local awful      = require("awful")

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.

themename        = "default" -- theme name
themetype        = "dark"
terminal         = "alacritty" -- default terminal
editor           = "kate" -- default editor
editor_cmd       = editor

-- extern apps definition
browser          = "firefox"
clipboard        = "copyq"
composite        = "picom"
filemanager      = "krusader"
filemanager_root = "kdesu krusader"
guard            = "gnome-system-monitor"
mail             = "thunderbird"
screenlook       = "blurlock"
tasks            = "xfce4-taskmanager"

themepath        = gears.filesystem.get_xdg_config_home() .. "awesome/themes/"
themefile        = themepath .. themename .. "/theme.lua"

-- {{{ tag definitions
tag_Develop    = " Develop"
tag_DevConsole = " Develop Consolen"
tag_Divers     = " Sonstiges"
tag_Teams      = " Teams"
tag_VM         = " VirtualBox"
tag_Web        = " Web"
tag_Media      = " Audio /  Video"
tag_Admin      = " Admin"
tag_Status     = " Status"
-- }}}

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey               = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
  awful.layout.suit.floating,
  awful.layout.suit.tile,
  awful.layout.suit.tile.left,
  awful.layout.suit.tile.bottom,
  awful.layout.suit.tile.top,
  awful.layout.suit.fair,
  awful.layout.suit.fair.horizontal,
  awful.layout.suit.spiral,
  awful.layout.suit.spiral.dwindle,
  awful.layout.suit.max,
  awful.layout.suit.max.fullscreen,
  awful.layout.suit.magnifier,
  awful.layout.suit.corner.nw,
  awful.layout.suit.corner.ne,
  awful.layout.suit.corner.sw,
  awful.layout.suit.corner.se,
}
-- }}}

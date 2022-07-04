log('Enter Module => theme/theme.lua')

local beautiful = require('beautiful')
local gears = require('gears')

local gtk_variable = beautiful.gtk.get_theme_variables
local dpi = require('beautiful.xresources').apply_dpi
local makeColorTransparent = require('utilities.utils').makeColorTransparent

local theme_dir = require('theme.default-theme').theme_dir

local cobalt_bg = '#072539';  -- Background
local cobalt_fg = '#e1efff';  -- Foreground
local cobalt_highlight = '#19acc6';  -- Highlight
local cobalt_accent_blue = '#0050a4';  -- Accent 1 (Blue)
local cobalt_accent_yellow = '#FFC600';  -- Accent 2 (Yellow)
local cobalt_window_bg = '#2e3a50';  -- Window Background

local base00 = '#232423' -- ---- red
local base01 = '#BA2922' -- ---  orange
local base02 = '#7E807E' -- --   yellow
local base03 = '#4C4F4D' -- -    green
local base04 = '#16A085' -- +    aqua/cyan
local base05 = '#43746A' -- ++   blue
local base06 = '#00CCCC' -- +++  purple
local base07 = '#E0E0E0' -- ++++ brown
local base08 = '#282928' -- red
local base09 = '#CC372C' -- orange
local base0A = '#8D8F8D' -- yellow
local base0B = '#4E524F' -- green
local base0C = '#13BF9D' -- aqua/cyan
local base0D = '#487D72' -- blue
local base0E = '#00D1D1' -- purple
local base0F = '#E8E8E8' -- brown

-- Create theme table
local theme = {}

-- Font
theme.font_family = 'Noto Sans '
theme.font_size = dpi(10)
theme.font_hint_size = dpi(5)

theme.font = theme.font_family .. ' Regular ' .. theme.font_size
theme.font_bold = theme.font_family .. ' Bold ' .. theme.font_size
theme.hint_font = theme.font_family .. ' ' .. theme.font_hint_size
theme.symbol_font = 'Noto Sans Symbol Bold ' .. dpi(15)

theme.icon_theme = 'oomox-cobalt'

---- Color Definitions

-- Accent color
theme.accent = cobalt_accent_yellow

-- General colors
theme.success_fg = base0C
theme.loaded_fg  = base0D
theme.error_fg   = cobalt_bg
theme.error_bg   = base08

-- Warning colors
theme.warning_fg = cobalt_bg
theme.warning_bg = base0E

-- Notification colors
theme.notif_fg = cobalt_bg
theme.notif_bg = cobalt_fg

-- Background color
theme.background = makeColorTransparent(gtk_variable().bg_color, '80')

-- Transparent
theme.transparent = makeColorTransparent(gtk_variable().bg_color, '10')

-- Awesome icon
theme.awesome_icon = theme_dir .. '/icons/awesome.png'
-- -----------------------------------------------

local awesome_overrides = function(theme)
  -- Default wallpaper path
  theme.wallpaper = theme_dir .. '/wallpaper.jpg'

  -- Foreground
  theme.fg_normal = cobalt_fg
  theme.fg_focus  = cobalt_fg
  theme.fg_urgent = theme.warning_fg

  theme.bg_normal = cobalt_bg
  theme.bg_focus  = cobalt_accent_blue
  theme.bg_urgent = makeColorTransparent(theme.warning_bg, '80')

  -- System tray
  theme.bg_systray = theme.bg_normal
  theme.systray_icon_spacing = dpi(16)

  -- Client Decorations
  -- Borders
  theme.border_focus = makeColorTransparent(cobalt_accent_yellow, '40')
  theme.border_normal = makeColorTransparent(cobalt_accent_blue, '40')
  theme.border_marked = cobalt_highlight

  theme.border_width = dpi(2)
  theme.border_radius = dpi(5)

  -- Decorations
  theme.useless_gap = dpi(3)

  -- Menu
  theme.menu_font = theme.font

  theme.menu_height = dpi(30)
  theme.menu_width = dpi(300)
  theme.menu_border_width = dpi(5)
  theme.menu_bg_focus = theme.accent .. 'CC'

  theme.menu_bg_normal = makeColorTransparent(theme.background, '33')
  theme.menu_fg_normal = theme.fg_normal
  theme.menu_fg_focus = theme.fg_focus
  theme.menu_border_color = makeColorTransparent(theme.background, '5C')

  theme.menu_title_bg = cobalt_bg
  theme.menu_primary_title_fg = base05
  theme.menu_secondary_title_fg = base04

  -- Tooltips
  theme.tooltip_border_width = dpi(2)
  theme.tooltip_gaps = dpi(5)
  theme.tooltip_shape = function(cr, w, h)
    gears.shape.rounded_rect(cr, w, h, dpi(6))
  end

  -- Separators
  theme.separator_color = theme.accent

  -- Taglist
  theme.taglist_bg_urgent = makeColorTransparent(theme.bg_urgent, '99')
  theme.taglist_spacing = dpi(2)

  -- Tasklist
  theme.tasklist_font = theme.font

  theme.tasklist_bg_urgent = makeColorTransparent(theme.bg_urgent, '99')

  -- Notification
  theme.notification_position = 'bottom_right'

  -- Hotkey popup
  
  --- Hotkeys widget background color.
  theme.hotkeys_bg = theme.cobalt_bg
  --- Hotkeys widget foreground color.
  theme.hotkeys_fg = cobalt_accent_yellow
  --- Hotkeys widget border width.
  -- theme.hotkeys_border_width
  --- Hotkeys widget border color.
  theme.hotkeys_border_color = theme.accent
  --- Hotkeys widget shape.
  -- theme.hotkeys_shape
  --- Foreground color used for hotkey modifiers (Ctrl, Alt, Super, etc).
  theme.hotkeys_modifiers_fg = cobalt_highlight
  --- Background color used for miscellaneous labels of hotkeys widget.
  -- theme.hotkeys_label_bg
  --- Foreground color used for hotkey groups and other labels.
  -- theme.hotkeys_label_fg = cobalt_accent_yellow
  --- Main hotkeys widget font.
  theme.hotkeys_font             = theme.font_family .. ' Bold ' .. dpi(9)
  --- Font used for hotkeys' descriptions.
  theme.hotkeys_description_font = theme.font_family .. ' Regular ' .. dpi(7)
  --- Margin between hotkeys groups.
  -- theme.hotkeys_group_margin

  theme.hotkeys_group_margin = dpi(10)

  theme.awesome_icon = theme_dir .. '/icons/awesome.png'
  theme.taglist_squares_sel = theme_dir .. '/icons/square_sel.png'
  theme.taglist_squares_unsel = theme_dir .. '/icons/square_unsel.png'
  theme.widget_ac = theme_dir .. '/icons/ac.png'
  theme.widget_battery = theme_dir .. '/icons/battery.png'
  theme.widget_battery_low = theme_dir .. '/icons/battery_low.png'
  theme.widget_battery_empty = theme_dir .. '/icons/battery_empty.png'
  theme.widget_mem = theme_dir .. '/icons/mem.png'
  theme.widget_cpu = theme_dir .. '/icons/cpu.png'
  theme.widget_temp = theme_dir .. '/icons/temp.png'
  theme.widget_net = theme_dir .. '/icons/net.png'
  theme.widget_hdd = theme_dir .. '/icons/hdd.png'
  theme.widget_music = theme_dir .. '/icons/note.png'
  theme.widget_music_on = theme_dir .. '/icons/note.png'
  theme.widget_music_pause = theme_dir .. '/icons/pause.png'
  theme.widget_music_stop = theme_dir .. '/icons/stop.png'
  theme.widget_vol = theme_dir .. '/icons/vol.png'
  theme.widget_vol_low = theme_dir .. '/icons/vol_low.png'
  theme.widget_vol_no = theme_dir .. '/icons/vol_no.png'
  theme.widget_vol_mute = theme_dir .. '/icons/vol_mute.png'
  theme.widget_mail = theme_dir .. '/icons/mail.png'
  theme.widget_mail_on = theme_dir .. '/icons/mail_on.png'
  theme.widget_task = theme_dir .. '/icons/task.png'
  theme.widget_scissors = theme_dir .. '/icons/scissors.png'
  theme.widget_weather = theme_dir .. '/icons/dish.png'

  theme.system_black_dark = base00
  theme.system_black_light = base08

  theme.system_red_dark = base01
  theme.system_red_light = base09

  theme.system_green_dark = base02
  theme.system_green_light = base0A

  theme.system_yellow_dark = base03
  theme.system_yellow_light = base0B

  theme.system_blue_dark = base04
  theme.system_blue_light = base0C

  theme.system_magenta_dark = base05
  theme.system_magenta_light = base0D

  theme.system_cyan_dark = base06
  theme.system_cyan_light = base0E

  theme.system_white_dark = base07
  theme.system_white_light = base0F
end

awesome_overrides(theme)

return {
  theme = theme,
  awesome_overrides = awesome_overrides
}

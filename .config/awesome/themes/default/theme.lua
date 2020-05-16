---------------------------
-- Default awesome theme --
---------------------------

local theme_assets            = require("beautiful.theme_assets")
local xresources              = require("beautiful.xresources")
local dpi                     = xresources.apply_dpi

local theme_assets            = require("beautiful.theme_assets")
local gears_shape             = require("gears.shape")

local wibox                   = require("wibox")
local awful_widget_clienticon = require("awful.widget.clienticon")

local gtk                     = require("beautiful.gtk")
local gears                   = require("gears")

require("../../definitions")

local themes_path                               = themepath

-- Helper functions for modifying hex colors:
--
local hex_color_match = "[a-fA-F0-9][a-fA-F0-9]"
local function darker(color_value, darker_n)
  local result          = "#"
  local channel_counter = 1
  for s in color_value:gmatch(hex_color_match) do
    local bg_numeric_value = tonumber("0x" .. s)
    if channel_counter <= 3 then
      bg_numeric_value = bg_numeric_value - darker_n
    end
    if bg_numeric_value < 0 then
      bg_numeric_value = 0
    end
    if bg_numeric_value > 255 then
      bg_numeric_value = 255
    end
    result          = result .. string.format("%02x", bg_numeric_value)
    channel_counter = channel_counter + 1
  end
  return result
end
local function is_dark(color_value)
  local bg_numeric_value = 0;
  local channel_counter  = 1
  for s in color_value:gmatch(hex_color_match) do
    bg_numeric_value = bg_numeric_value + tonumber("0x" .. s);
    if channel_counter == 3 then
      break
    end
    channel_counter = channel_counter + 1
  end
  local is_dark_bg = (bg_numeric_value < 383)
  return is_dark_bg
end
local function mix(color1, color2, ratio)
  ratio           = ratio or 0.5
  local result    = "#"
  local channels1 = color1:gmatch(hex_color_match)
  local channels2 = color2:gmatch(hex_color_match)
  for _ = 1, 3 do
    local bg_numeric_value = math.ceil(
      tonumber("0x" .. channels1()) * ratio +
        tonumber("0x" .. channels2()) * (1 - ratio)
    )
    if bg_numeric_value < 0 then
      bg_numeric_value = 0
    end
    if bg_numeric_value > 255 then
      bg_numeric_value = 255
    end
    result = result .. string.format("%02x", bg_numeric_value)
  end
  return result
end
local function reduce_contrast(color, ratio)
  ratio = ratio or 50
  return darker(color, is_dark(color) and -ratio or ratio)
end

local function choose_contrast_color(reference, candidate1, candidate2)
  -- luacheck: no unused
  if is_dark(reference) then
    if not is_dark(candidate1) then
      return candidate1
    else
      return candidate2
    end
  else
    if is_dark(candidate1) then
      return candidate1
    else
      return candidate2
    end
  end
end


-- inherit xresources theme:
local theme = dofile(themes_path .. "xresources/theme.lua")
-- load and prepare for use gtk theme:
theme.gtk   = gtk.get_theme_variables()
if not theme.gtk then
  local gears_debug = require("gears.debug")
  gears_debug.print_warning("Can't load GTK+3 theme. Using 'xresources' theme as a fallback.")
  return theme
end
theme.gtk.button_border_radius                  = dpi(theme.gtk.button_border_radius or 0)
theme.gtk.button_border_width                   = dpi(theme.gtk.button_border_width or 1)
theme.gtk.bold_font                             = theme.gtk.font_family .. ' Bold ' .. theme.gtk.font_size
theme.gtk.menubar_border_color                  = mix(
  theme.gtk.menubar_bg_color,
  theme.gtk.menubar_fg_color,
  0.7
)

--theme.font                                      = "sans 8"
--
--theme.bg_normal                                 = my_bg_normal
--theme.bg_focus                                  = my_bg_focus
--theme.bg_urgent                                 = my_bg_urgent
--theme.bg_minimize                               = my_bg_minimize
--theme.bg_systray                                = my_bg_systray
--
--theme.fg_normal                                 = my_fg_normal
--theme.fg_focus                                  = my_fg_focus
--theme.fg_urgent                                 = my_fg_urgent
--theme.fg_minimize                               = my_fg_minimize
--
--theme.useless_gap                               = my_useless_gap
--theme.border_width                              = my_border_width
--
--theme.border_normal                             = my_border_normal
--theme.border_focus                              = my_border_focus
--theme.border_marked                             = my_border_marked

theme.font                                      = theme.gtk.font_family .. ' ' .. theme.gtk.font_size

theme.bg_normal                                 = theme.gtk.bg_color
theme.fg_normal                                 = theme.gtk.fg_color

theme.wibar_bg                                  = theme.gtk.menubar_bg_color
theme.wibar_fg                                  = theme.gtk.menubar_fg_color

theme.bg_focus                                  = theme.gtk.selected_bg_color
theme.fg_focus                                  = theme.gtk.selected_fg_color

theme.bg_urgent                                 = theme.gtk.error_bg_color
theme.fg_urgent                                 = theme.gtk.error_fg_color

theme.bg_minimize                               = mix(theme.wibar_fg, theme.wibar_bg, 0.3)
theme.fg_minimize                               = mix(theme.wibar_fg, theme.wibar_bg, 0.9)

theme.bg_systray                                = theme.wibar_bg

theme.border_normal                             = theme.gtk.wm_border_unfocused_color
theme.border_focus                              = theme.gtk.wm_border_focused_color
theme.border_marked                             = theme.gtk.success_color

theme.border_width                              = dpi(theme.gtk.button_border_width or 1)
theme.border_radius                             = theme.gtk.button_border_radius

theme.useless_gap                               = dpi(3)

local rounded_rect_shape                        = function(cr, w, h)
  gears_shape.rounded_rect(
    cr, w, h, theme.border_radius
  )
end

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty|volatile]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- prompt_[fg|bg|fg_cursor|bg_cursor|font]
-- hotkeys_[bg|fg|border_width|border_color|shape|opacity|modifiers_fg|label_bg|label_fg|group_margin|font|description_font]
-- Example:
--theme.taglist_bg_focus = "#ff0000"

theme.tasklist_fg_normal                        = theme.wibar_fg
theme.tasklist_bg_normal                        = theme.wibar_bg
theme.tasklist_fg_focus                         = theme.tasklist_fg_normal
theme.tasklist_bg_focus                         = theme.tasklist_bg_normal

theme.tasklist_font_focus                       = theme.gtk.bold_font

theme.tasklist_shape_minimized                  = rounded_rect_shape
theme.tasklist_shape_border_color_minimized     = mix(
  theme.bg_minimize,
  theme.fg_minimize,
  0.85
)
theme.tasklist_shape_border_width_minimized     = theme.gtk.button_border_width

theme.tasklist_spacing                          = theme.gtk.button_border_width

--[[ Advanced taglist and tasklist styling: {{{
--]]

theme.tasklist_widget_template                  = {
  {
    {
      {
        {
          id     = 'clienticon',
          widget = awful_widget_clienticon,
        },
        margins = dpi(4),
        widget  = wibox.container.margin,
      },
      {
        id     = 'text_role',
        widget = wibox.widget.textbox,
      },
      layout = wibox.layout.fixed.horizontal,
    },
    left   = dpi(2),
    right  = dpi(4),
    widget = wibox.container.margin
  },
  id              = 'background_role',
  widget          = wibox.container.background,
  create_callback = function(self, c)
    self:get_children_by_id('clienticon')[1].client = c
  end,
}

theme.taglist_shape_container                   = rounded_rect_shape
theme.taglist_shape_clip_container              = true
theme.taglist_shape_border_width_container      = theme.gtk.button_border_width * 2
theme.taglist_shape_border_color_container      = theme.gtk.header_button_border_color
-- }}}

theme.taglist_bg_occupied                       = theme.gtk.header_button_bg_color
theme.taglist_fg_occupied                       = theme.gtk.header_button_fg_color

theme.taglist_bg_empty                          = mix(
  theme.gtk.menubar_bg_color,
  theme.gtk.header_button_bg_color,
  0.3
)
theme.taglist_fg_empty                          = mix(
  theme.gtk.menubar_bg_color,
  theme.gtk.header_button_fg_color
)


-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon                         = themes_path .. "default/submenu.png"
theme.menu_height                               = dpi(24)
theme.menu_width                                = dpi(160)

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
theme.titlebar_close_button_normal              = themes_path .. "default/titlebar/close_normal.png"
theme.titlebar_close_button_focus               = themes_path .. "default/titlebar/close_focus.png"

theme.titlebar_minimize_button_normal           = themes_path .. "default/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus            = themes_path .. "default/titlebar/minimize_focus.png"

theme.titlebar_ontop_button_normal_inactive     = themes_path .. "default/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive      = themes_path .. "default/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active       = themes_path .. "default/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active        = themes_path .. "default/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive    = themes_path .. "default/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive     = themes_path .. "default/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active      = themes_path .. "default/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active       = themes_path .. "default/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive  = themes_path .. "default/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive   = themes_path .. "default/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active    = themes_path .. "default/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active     = themes_path .. "default/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = themes_path .. "default/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = themes_path .. "default/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active   = themes_path .. "default/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active    = themes_path .. "default/titlebar/maximized_focus_active.png"

theme.wallpaper                                 = function(s)
  return gears.filesystem.get_xdg_config_home() .. "awesome/background" .. s.index .. "_" .. themetype .. ".jpg"
end

-- You can use your own layout icons like this:
theme.layout_fairh                              = themes_path .. "default/layouts/fairhw.png"
theme.layout_fairv                              = themes_path .. "default/layouts/fairvw.png"
theme.layout_floating                           = themes_path .. "default/layouts/floatingw.png"
theme.layout_magnifier                          = themes_path .. "default/layouts/magnifierw.png"
theme.layout_max                                = themes_path .. "default/layouts/maxw.png"
theme.layout_fullscreen                         = themes_path .. "default/layouts/fullscreenw.png"
theme.layout_tilebottom                         = themes_path .. "default/layouts/tilebottomw.png"
theme.layout_tileleft                           = themes_path .. "default/layouts/tileleftw.png"
theme.layout_tile                               = themes_path .. "default/layouts/tilew.png"
theme.layout_tiletop                            = themes_path .. "default/layouts/tiletopw.png"
theme.layout_spiral                             = themes_path .. "default/layouts/spiralw.png"
theme.layout_dwindle                            = themes_path .. "default/layouts/dwindlew.png"
theme.layout_cornernw                           = themes_path .. "default/layouts/cornernww.png"
theme.layout_cornerne                           = themes_path .. "default/layouts/cornernew.png"
theme.layout_cornersw                           = themes_path .. "default/layouts/cornersww.png"
theme.layout_cornerse                           = themes_path .. "default/layouts/cornersew.png"

-- Generate Awesome icon:
theme.awesome_icon                              = theme_assets.awesome_icon(
  theme.menu_height, theme.bg_focus, theme.fg_focus
)

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme                                = nil

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80

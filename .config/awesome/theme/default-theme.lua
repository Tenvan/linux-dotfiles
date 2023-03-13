local log = require('utilities.debug').log
log('Enter Module => ' .. ...)

local gears              = require('gears')
local beautiful          = require('beautiful')

local filesystem         = gears.filesystem
local dpi                = beautiful.xresources.apply_dpi
local config             = require('configuration.json') or {}
local specs              = require('layout.specs')

local theme_dir          = filesystem.get_configuration_dir() .. 'theme'
local titlebar_icon_path = theme_dir .. '/icons/titlebar/'
local tip                = titlebar_icon_path
local tie                = 'png'

-- Create theme table
local theme              = {}

-- Font
theme.font_family        = 'Iosevka'
-- theme.symbol_font_family = 'Material-Design-Iconic-Font'
-- theme.symbol_font_family = 'Symbol Nerd Font, Material-Design-Iconic-Font, '

-- Symbol Font
theme.symbol_font_family = 'twemoji'
theme.font_symbol        = theme.symbol_font_family .. ' Regular ' .. dpi(14)

theme.font               = string.format('%s Regular %d', theme.font_family, dpi(12))
theme.font_bold          = string.format('%s Bold %d', theme.font_family, dpi(12))

-- Font
theme.font_small_size    = dpi(config.font_size_small or 8)
theme.font_size          = dpi(config.font_size_normal or 10)
theme.font_large_size    = dpi(config.font_size_large or 14)
theme.font_big_size      = dpi(config.font_size_big or 20)

theme.element_size       = specs.elementSize

log('Font Font Family: ' .. theme.font_small_size)
log('Font Small Size: ' .. theme.font_small_size)
log('Font Normal Size: ' .. theme.font_size)
log('Font Large Size: ' .. theme.font_large_size)
log('Font Big Size: ' .. theme.font_big_size)

theme.font_small  = theme.font_family .. ' Regular ' .. theme.font_small_size
theme.font        = theme.font_family .. ' Regular ' .. theme.font_size
theme.font_large  = theme.font_family .. ' Regular ' .. theme.font_large_size

theme.font_bold_small  = theme.font_family .. ' Bold ' .. theme.font_small_size
theme.font_bold        = theme.font_family .. ' Bold ' .. theme.font_size
theme.font_bold_large  = theme.font_family .. ' Bold ' .. theme.font_large_size

theme.font_light_small = theme.font_family .. ' Light ' .. theme.font_small_size
theme.font_light       = theme.font_family .. ' Light ' .. theme.font_size
theme.font_light_large = theme.font_family .. ' Light ' .. theme.font_large_size

theme.hint_font        = theme.font_light_small
theme.symbol_font      = theme.font_family .. ' Bold ' .. dpi(12)
theme.widgets_font     = theme.font_light

-- Menu icon theme
theme.icon_theme       = 'twemoji'

-- general Radius
theme.radius           = dpi(15)



local awesome_overrides = function(theme)
  -- General colors
  theme.success_fg                                      = theme.gtk_vars.success_fg_color
  theme.success_bg                                      = theme.gtk_vars.success_bg_color
  theme.error_fg                                        = theme.fg
  -- theme.gtk_vars.error_fg_color
  theme.error_bg                                        = theme.gtk_vars.error_bg_color

  theme.loaded_fg                                       = theme.gtk_vars.selected_bg_color

  -- Warning colors
  theme.warning_fg                                      = theme.gtk_vars.warning_fg_color
  theme.warning_bg                                      = theme.gtk_vars.warning_bg_color

  -- Background color
  theme.background                                      = theme.gtk_vars.bg_color:sub(1, 7) .. '80'

  -- Transparent
  theme.transparent                                     = theme.gtk_vars.bg_color:sub(1, 7) .. '20'

  -- Accent color
  theme.accent                                          = theme.gtk_vars.wm_border_focused_color

  theme.dir                                             = theme_dir
  theme.icons                                           = theme_dir .. '/icons/'

  -- Default wallpaper path
  theme.wallpaper                                       = theme.dir .. '/wallpapers/morning-wallpaper.jpg'

  -- Foreground
  theme.fg_normal                                       = theme.gtk_vars.fg_color:sub(1, 7)
  theme.fg_focus                                        = theme.gtk_vars.selected_fg_color:sub(1, 7)
  theme.fg_urgent                                       = theme.gtk_vars.warning_fg_color:sub(1, 7)

  theme.bg_normal                                       = theme.gtk_vars.bg_color:sub(1, 7)
  theme.bg_focus                                        = theme.gtk_vars.selected_bg_color:sub(1, 7)
  theme.bg_urgent                                       = theme.gtk_vars.warning_bg_color:sub(1, 7)

  -- System tray
  -- theme.bg_systray = theme.background
  theme.systray_icon_spacing                            = dpi(8)

  -- Titlebar
  theme.titlebar_size                                   = dpi(34)
  theme.titlebar_bg_focus                               = theme.gtk_vars.bg_color:sub(1, 7)
  theme.titlebar_bg_normal                              = theme.gtk_vars.base_color:sub(1, 7)
  theme.titlebar_fg_focus                               = theme.gtk_vars.fg_color
  theme.titlebar_fg_normal                              = theme.gtk_vars.fg_color

  -- Close Button
  theme.titlebar_close_button_normal                    = tip .. 'close_normal.' .. tie
  theme.titlebar_close_button_focus                     = tip .. 'close_focus.' .. tie

  -- Minimize Button
  theme.titlebar_minimize_button_normal                 = tip .. 'minimize_normal.' .. tie
  theme.titlebar_minimize_button_focus                  = tip .. 'minimize_focus.' .. tie

  -- Ontop Button
  theme.titlebar_ontop_button_normal_inactive           = tip .. 'ontop_normal_inactive.' .. tie
  theme.titlebar_ontop_button_focus_inactive            = tip .. 'ontop_focus_inactive.' .. tie
  theme.titlebar_ontop_button_normal_active             = tip .. 'ontop_normal_active.' .. tie
  theme.titlebar_ontop_button_focus_active              = tip .. 'ontop_focus_active.' .. tie

  -- Sticky Button
  theme.titlebar_sticky_button_normal_inactive          = tip .. 'sticky_normal_inactive.' .. tie
  theme.titlebar_sticky_button_focus_inactive           = tip .. 'sticky_focus_inactive.' .. tie
  theme.titlebar_sticky_button_normal_active            = tip .. 'sticky_normal_active.' .. tie
  theme.titlebar_sticky_button_focus_active             = tip .. 'sticky_focus_active.' .. tie

  -- Floating Button
  theme.titlebar_floating_button_normal_inactive        = tip .. 'floating_normal_inactive.' .. tie
  theme.titlebar_floating_button_focus_inactive         = tip .. 'floating_focus_inactive.' .. tie
  theme.titlebar_floating_button_normal_active          = tip .. 'floating_normal_active.' .. tie
  theme.titlebar_floating_button_focus_active           = tip .. 'floating_focus_active.' .. tie

  -- Maximized Button
  theme.titlebar_maximized_button_normal_inactive       = tip .. 'maximized_normal_inactive.' .. tie
  theme.titlebar_maximized_button_focus_inactive        = tip .. 'maximized_focus_inactive.' .. tie
  theme.titlebar_maximized_button_normal_active         = tip .. 'maximized_normal_active.' .. tie
  theme.titlebar_maximized_button_focus_active          = tip .. 'maximized_focus_active.' .. tie

  -- Hovered Close Button
  theme.titlebar_close_button_normal_hover              = tip .. 'close_normal_hover.svg'
  theme.titlebar_close_button_focus_hover               = tip .. 'close_focus_hover.svg'

  -- Hovered Minimize Buttin
  theme.titlebar_minimize_button_normal_hover           = tip .. 'minimize_normal_hover.svg'
  theme.titlebar_minimize_button_focus_hover            = tip .. 'minimize_focus_hover.svg'

  -- Hovered Ontop Button
  theme.titlebar_ontop_button_normal_inactive_hover     = tip .. 'ontop_normal_inactive_hover.svg'
  theme.titlebar_ontop_button_focus_inactive_hover      = tip .. 'ontop_focus_inactive_hover.svg'
  theme.titlebar_ontop_button_normal_active_hover       = tip .. 'ontop_normal_active_hover.svg'
  theme.titlebar_ontop_button_focus_active_hover        = tip .. 'ontop_focus_active_hover.svg'

  -- Hovered Sticky Button
  theme.titlebar_sticky_button_normal_inactive_hover    = tip .. 'sticky_normal_inactive_hover.svg'
  theme.titlebar_sticky_button_focus_inactive_hover     = tip .. 'sticky_focus_inactive_hover.svg'
  theme.titlebar_sticky_button_normal_active_hover      = tip .. 'sticky_normal_active_hover.svg'
  theme.titlebar_sticky_button_focus_active_hover       = tip .. 'sticky_focus_active_hover.svg'

  -- Hovered Floating Button
  theme.titlebar_floating_button_normal_inactive_hover  = tip .. 'floating_normal_inactive_hover.svg'
  theme.titlebar_floating_button_focus_inactive_hover   = tip .. 'floating_focus_inactive_hover.svg'
  theme.titlebar_floating_button_normal_active_hover    = tip .. 'floating_normal_active_hover.svg'
  theme.titlebar_floating_button_focus_active_hover     = tip .. 'floating_focus_active_hover.svg'

  -- Hovered Maximized Button
  theme.titlebar_maximized_button_normal_inactive_hover = tip .. 'maximized_normal_inactive_hover.svg'
  theme.titlebar_maximized_button_focus_inactive_hover  = tip .. 'maximized_focus_inactive_hover.svg'
  theme.titlebar_maximized_button_normal_active_hover   = tip .. 'maximized_normal_active_hover.svg'
  theme.titlebar_maximized_button_focus_active_hover    = tip .. 'maximized_focus_active_hover.svg'

  -- UI Groups
  theme.groups_title_bg                                 = theme.gtk_vars.osd_bg_color
  theme.groups_bg                                       = theme.background
  theme.groups_radius                                   = theme.radius

  -- Client Decorations

  -- Borders
  theme.border_normal                                   = theme.gtk_vars.wm_border_unfocused_color
  theme.border_focus                                    = theme.gtk_vars.wm_border_focused_color
  theme.border_marked                                   = theme.gtk_vars.wm_border_focused_color
  theme.border_width                                    = dpi(0)
  theme.border_radius                                   = theme.radius

  theme.border_color_marked                             = theme.titlebar_bg_normal
  theme.border_color_active                             = theme.titlebar_bg_normal
  theme.border_color_normal                             = theme.titlebar_bg_normal
  theme.border_color_new                                = theme.titlebar_bg_normal
  theme.border_color_urgent                             = theme.titlebar_bg_normal
  theme.border_color_floating                           = theme.titlebar_bg_normal
  theme.border_color_maximized                          = theme.titlebar_bg_normal
  theme.border_color_fullscreen                         = theme.titlebar_bg_normal

  -- Decorations
  theme.useless_gap                                     = dpi(4)
  theme.client_shape_rectangle                          = gears.shape.rectangle
  theme.client_shape_rounded                            = function(cr, width, height)
    gears.shape.rounded_rect(cr, width, height, dpi(6))
  end

  -- Menu
  theme.menu_font                                       = theme.font
  theme.menu_submenu                                    = '' -- ➤

  theme.menu_height                                     = dpi(34)
  theme.menu_width                                      = dpi(300)
  theme.menu_border_width                               = dpi(2)
  theme.menu_title_bg                                   = theme.bg_urgent
  theme.menu_fg_normal                                  = theme.gtk_vars.menubar_fg_color:sub(1, 7)
  theme.menu_fg_focus                                   = theme.fg_focus
  theme.menu_bg_normal                                  = theme.gtk_vars.menubar_bg_color:sub(1, 7)
  theme.menu_bg_focus                                   = theme.accent:sub(1, 7) .. 'CC'
  theme.menu_border_color                               = theme.border_normal:sub(1, 7)
  theme.menu_submenu_icon                               = theme.icons .. 'submenu.png'

  -- Tooltips

  theme.tooltip_bg                                      = theme.background
  theme.tooltip_border_color                            = theme.transparent
  theme.tooltip_border_width                            = 0
  theme.tooltip_gaps                                    = dpi(5)
  theme.tooltip_shape                                   = function(cr, w, h)
    gears.shape.rounded_rect(cr, w, h, dpi(6))
  end

  -- Separators
  theme.separator_color                                 = '#f2f2f244'

  -- Layoutbox icons
  theme.layout_dwindle                                  = theme.icons .. 'layouts/dwindle.' .. tie
  theme.layout_fairh                                    = theme.icons .. 'layouts/fairh.' .. tie
  theme.layout_fairv                                    = theme.icons .. 'layouts/fairv.' .. tie
  theme.layout_floating                                 = theme.icons .. 'layouts/floating.' .. tie
  theme.layout_fullscreen                               = theme.icons .. 'layouts/fullscreen.' .. tie
  theme.layout_magnifier                                = theme.icons .. 'layouts/magnifier.' .. tie
  theme.layout_max                                      = theme.icons .. 'layouts/max.' .. tie
  theme.layout_spiral                                   = theme.icons .. 'layouts/spiral.' .. tie
  theme.layout_tile                                     = theme.icons .. 'layouts/tile.' .. tie
  theme.layout_tilebottom                               = theme.icons .. 'layouts/tilebottom.' .. tie
  theme.layout_tileleft                                 = theme.icons .. 'layouts/tileleft.' .. tie
  theme.layout_tiletop                                  = theme.icons .. 'layouts/tiletop.' .. tie

  -- Taglist
  theme.taglist_bg_empty                                = theme.background .. '99'
  theme.taglist_bg_occupied                             = '#ffffff' .. '1A'
  theme.taglist_bg_urgent                               = '#E91E63' .. '99'
  theme.taglist_bg_focus                                = theme.background
  theme.taglist_spacing                                 = dpi(0)

  -- Tasklist
  theme.tasklist_font                                   = theme.font_small
  theme.tasklist_bg_normal                              = theme.background .. '99'
  theme.tasklist_bg_focus                               = theme.background
  theme.tasklist_bg_urgent                              = '#E91E63' .. '99'
  theme.tasklist_fg_focus                               = '#DDDDDD'
  theme.tasklist_fg_urgent                              = '#ffffff'
  theme.tasklist_fg_normal                              = '#AAAAAA'

  -- Notification
  theme.notification_position                           = 'top_right'
  theme.notification_bg                                 = theme.transparent
  theme.notification_margin                             = dpi(5)
  theme.notification_border_width                       = dpi(0)
  theme.notification_border_color                       = theme.transparent
  theme.notification_spacing                            = dpi(5)
  theme.notification_icon_resize_strategy               = 'center'
  theme.notification_icon_size                          = dpi(32)

  -- Client Snap Theme
  theme.snap_bg                                         = theme.background
  theme.snap_shape                                      = gears.shape.rectangle
  theme.snap_border_width                               = dpi(15)

  -- Hotkey popup
  theme.hotkeys_font                                    = theme.font_bold
  theme.hotkeys_description_font                        = theme.font
  theme.hotkeys_bg                                      = theme.background
  theme.hotkeys_group_margin                            = dpi(20)
end

return {
  theme = theme,
  theme_dir = theme_dir,
  awesome_overrides = awesome_overrides
}

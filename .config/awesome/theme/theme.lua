log('Enter Module => ' .. ...)

local gears = require('gears')

local dpi = require('beautiful').xresources.apply_dpi

local theme_dir = require('theme.default-theme').theme_dir
local modConfig = require('configuration.config').module

local makeColorTransparent = require('utilities.utils').makeColorTransparent

-- Create theme table
local theme = {}

theme.icon_theme = 'Papirus-Dark'
theme.icon_theme_ext = { 'Papirus', 'ePapirus-Dark', 'ePapirus', 'Paper', 'hicolor' }

local awesome_overrides = function(theme)
  -- Awesome icon
  theme.awesome_icon = theme_dir .. '/icons/awesome.png'

  -- Default wallpaper path
  theme.wallpaper = modConfig.dynamic_wallpaper.wall_dir .. '/locksreen-bg.jpg'
  theme.wallpaper_path = modConfig.dynamic_wallpaper.wall_dir

  -- Foreground
  -- theme.fg_normal = theme.theme_fg_color
  -- theme.fg_focus  = theme.theme_selected_fg_color
  -- theme.fg_urgent = theme.warning_fg

  -- theme.bg_normal = theme.theme_bg_color
  -- theme.bg_focus  = theme.theme_selected_bg_color
  -- theme.bg_urgent = makeColorTransparent(theme.warning_bg, '80')

  -- System tray
  theme.bg_systray = theme.background
  theme.systray_icon_spacing = dpi(2)

  -- ░█▀█░█▀█░█▀█░█░█░█▀█░█▀▀
  -- ░█▀▀░█░█░█▀▀░█░█░█▀▀░▀▀█
  -- ░▀░░░▀▀▀░▀░░░▀▀▀░▀░░░▀▀▀
  theme.popup_bg           = theme.bg_normal
  theme.popup_fg           = theme.fg_normal
  theme.popup_border       = theme.bg_focus
  theme.popup_border_width = dpi(3)

  -- Client Decorations
  -- Borders
  theme.border_normal = makeColorTransparent(theme.border_normal, '40')
  theme.border_focus = makeColorTransparent(theme.border_focus, '40')
  theme.border_marked = makeColorTransparent(theme.border_focus, '40')

  theme.border_width = dpi(2)
  theme.border_radius = theme.radius

  -- Decorations
  theme.useless_gap = dpi(3)

  -- Tooltips
  theme.tooltip_border_width = dpi(2)
  theme.tooltip_gaps = dpi(5)
  theme.tooltip_shape = function(cr, w, h)
    gears.shape.rounded_rect(cr, w, h, dpi(6))
  end

  -- Separators
  theme.separator_color = theme.accent

  -- ░▀█▀░█▀█░█▀▀░█░░░▀█▀░█▀▀░▀█▀
  -- ░░█░░█▀█░█░█░█░░░░█░░▀▀█░░█░
  -- ░░▀░░▀░▀░▀▀▀░▀▀▀░▀▀▀░▀▀▀░░▀░
  --- The tag list main foreground (text) color.
  -- @beautiful theme.taglist_fg_focus
  -- @param[opt=fg_focus] color
  -- @see gears.color
  -- theme.taglist_fg_focus = theme.gtk_vars.header_button_fg_color

  --- The tag list main background color.
  -- @beautiful theme.taglist_bg_focus
  -- @param[opt=bg_focus] color
  -- @see gears.color
  theme.taglist_bg_focus = theme.gtk_vars.header_button_bg_color

  --- The tag list urgent elements foreground (text) color.
  -- @beautiful theme.taglist_fg_urgent
  -- @param[opt=fg_urgent] color
  -- @see gears.color
  theme.taglist_fg_urgent = makeColorTransparent(theme.bg_urgent, '99')

  --- The tag list urgent elements background color.
  -- @beautiful theme.taglist_bg_urgent
  -- @param[opt=bg_urgent] color
  -- @see gears.color
  theme.taglist_bg_urgent = makeColorTransparent(theme.bg_urgent, '99')

  --- The tag list occupied elements background color.
  -- @beautiful theme.taglist_bg_occupied
  -- @param color
  -- @see gears.color
  theme.taglist_bg_occupied = makeColorTransparent(theme.gtk_vars.success_color, '80')

  --- The tag list occupied elements foreground (text) color.
  -- @beautiful theme.taglist_fg_occupied
  -- @param color
  -- @see gears.color
  theme.taglist_fg_occupied = theme.xres_vars.color1

  --- The tag list empty elements background color.
  -- @beautiful theme.taglist_bg_empty
  -- @param color
  -- @see gears.color
  theme.taglist_bg_empty = theme.background
  -- theme.taglist_bg_empty = theme.transparent

  --- The tag list empty elements foreground (text) color.
  -- @beautiful theme.taglist_fg_empty
  -- @param color
  -- @see gears.color
  theme.taglist_fg_empty = theme.fg_normal

  --- The tag list volatile elements background color.
  -- @beautiful theme.taglist_bg_volatile
  -- @param color
  -- @see gears.color
  theme.taglist_bg_volatile = '#0f0'

  --- The tag list volatile elements foreground (text) color.
  -- @beautiful theme.taglist_fg_volatile
  -- @param color
  -- @see gears.color
  theme.taglist_fg_volatile = '#fff'

  --- The selected elements background image.
  -- @beautiful theme.taglist_squares_sel
  -- @param surface
  -- @see gears.surface

  --- The unselected elements background image.
  -- @beautiful theme.taglist_squares_unsel
  -- @param surface
  -- @see gears.surface

  --- The selected empty elements background image.
  -- @beautiful theme.taglist_squares_sel_empty
  -- @param surface
  -- @see gears.surface

  --- The unselected empty elements background image.
  -- @beautiful theme.taglist_squares_unsel_empty
  -- @param surface
  -- @see gears.surface

  --- If the background images can be resized.
  -- @beautiful theme.taglist_squares_resize
  -- @param boolean

  --- Do not display the tag icons, even if they are set.
  -- @beautiful theme.taglist_disable_icon
  -- @param boolean
  theme.taglist_disable_icon = true

  --- The taglist font.
  -- @beautiful theme.taglist_font
  -- @param string
  theme.taglist_font = theme.symbol_font

  --- The space between the taglist elements.
  -- @beautiful theme.taglist_spacing
  -- @tparam[opt=0] number spacing The spacing between tags.
  theme.taglist_spacing = dpi(2)

  --- The main shape used for the elements.
  -- This will be the fallback for state specific shapes.
  -- To get a shape for the whole taglist, use `wibox.container.background`.
  -- @beautiful theme.taglist_shape
  -- @tparam[opt=gears.shape.rectangle] gears.shape shape
  -- @see gears.shape
  -- @see theme.taglist_shape_empty
  -- @see theme.taglist_shape_focus
  -- @see theme.taglist_shape_urgent
  -- @see theme.taglist_shape_volatile
  theme.taglist_shape = gears.shape.rect

  --- The shape elements border width.
  -- @beautiful theme.taglist_shape_border_width
  -- @param[opt=0] number
  -- @see wibox.container.background
  theme.taglist_shape_border_width = 0

  --- The elements shape border color.
  -- @beautiful theme.taglist_shape_border_color
  -- @param color
  -- @see gears.color
  theme.taglist_shape_border_color = theme.accent

  --- The shape used for the empty elements.
  -- @beautiful theme.taglist_shape_empty
  -- @tparam[opt=gears.shape.rectangle] gears.shape shape
  -- @see gears.shape

  --- The shape used for the empty elements border width.
  -- @beautiful theme.taglist_shape_border_width_empty
  -- @param[opt=0] number
  -- @see wibox.container.background

  --- The empty elements shape border color.
  -- @beautiful theme.taglist_shape_border_color_empty
  -- @param color
  -- @see gears.color

  --- The shape used for the selected elements.
  -- @beautiful theme.taglist_shape_focus
  -- @tparam[opt=gears.shape.rectangle] gears.shape shape
  -- @see gears.shape

  --- The shape used for the selected elements border width.
  -- @beautiful theme.taglist_shape_border_width_focus
  -- @param[opt=0] number
  -- @see wibox.container.background

  --- The selected elements shape border color.
  -- @beautiful theme.taglist_shape_border_color_focus
  -- @param color
  -- @see gears.color

  --- The shape used for the urgent elements.
  -- @beautiful theme.taglist_shape_urgent
  -- @tparam[opt=gears.shape.rectangle] gears.shape shape
  -- @see gears.shape

  --- The shape used for the urgent elements border width.
  -- @beautiful theme.taglist_shape_border_width_urgent
  -- @param[opt=0] number
  -- @see wibox.container.background

  --- The urgents elements shape border color.
  -- @beautiful theme.taglist_shape_border_color_urgent
  -- @param color
  -- @see gears.color

  --- The shape used for the volatile elements.
  -- @beautiful theme.taglist_shape_volatile
  -- @tparam[opt=gears.shape.rectangle] gears.shape shape
  -- @see gears.shape

  --- The shape used for the volatile elements border width.
  -- @beautiful theme.taglist_shape_border_width_volatile
  -- @param[opt=0] number
  -- @see wibox.container.background

  --- The volatile elements shape border color.
  -- @beautiful theme.taglist_shape_border_color_volatile
  -- @param color
  -- @see gears.color

  -- ░▀█▀░█▀█░█▀▀░█░█░█░░░▀█▀░█▀▀░▀█▀
  -- ░░█░░█▀█░▀▀█░█▀▄░█░░░░█░░▀▀█░░█░
  -- ░░▀░░▀░▀░▀▀▀░▀░▀░▀▀▀░▀▀▀░▀▀▀░░▀░
  theme.tasklist_font = theme.font

  theme.tasklist_fg_normal         = theme.fg_normal --	The default foreground (text) color.
  theme.tasklist_bg_normal         = theme.background -- The default background color.
  theme.tasklist_fg_focus          = theme.fg_focus -- The focused client foreground (text) color.
  theme.tasklist_bg_focus          = theme.bg_focus -- The focused client background color.
  theme.tasklist_fg_urgent         = theme.fg_urgent --	The urgent clients foreground (text) color.
  theme.tasklist_bg_urgent         = theme.bg_urgent -- The urgent clients background color.
  -- theme.tasklist_fg_minimize	The minimized clients foreground (text) color.
  -- theme.tasklist_bg_minimize	The minimized clients background color.
  -- theme.tasklist_bg_image_normal	The elements default background image.
  -- theme.tasklist_bg_image_focus	The focused client background image.
  -- theme.tasklist_bg_image_urgent	The urgent clients background image.
  -- theme.tasklist_bg_image_minimize	The minimized clients background image.
  theme.tasklist_disable_icon      = true -- Disable the tasklist client icons.
  theme.tasklist_disable_task_name = false --	Disable the tasklist client titles.
  theme.tasklist_plain_task_name   = true --	Disable the extra tasklist client property notification icons.
  theme.tasklist_font              = theme.font_small -- The tasklist font.
  -- theme.tasklist_align	The focused client alignment.
  theme.tasklist_font_focus        = theme.font -- 	The focused client title alignment.
  theme.tasklist_font_minimized    = theme.font_small -- The minimized clients font.
  theme.tasklist_font_urgent       = theme.font_small -- 	The urgent clients font.
  -- theme.tasklist_spacing	The space between the tasklist elements.
  -- theme.tasklist_shape	The default tasklist elements shape.
  -- theme.tasklist_shape_border_width	The default tasklist elements border width.
  -- theme.tasklist_shape_border_color	The default tasklist elements border color.
  -- theme.tasklist_shape_focus	The focused client shape.
  -- theme.tasklist_shape_border_width_focus	The focused client border width.
  -- theme.tasklist_shape_border_color_focus	The focused client border color.
  -- theme.tasklist_shape_minimized	The minimized clients shape.
  -- theme.tasklist_shape_border_width_minimized	The minimized clients border width.
  -- theme.tasklist_shape_border_color_minimized	The minimized clients border color.
  -- theme.tasklist_shape_urgent	The urgent clients shape.
  -- theme.tasklist_shape_border_width_urgent	The urgent clients border width.
  -- theme.tasklist_shape_border_color_urgent	The urgent clients border color.

  -- Notification
  theme.notification_position = 'bottom_right'
  theme.notification_icon_size = dpi(256)

  -- Notification colors
  theme.notif_fg = theme.gtk_vars.osd_fg_color
  theme.notif_bg = theme.gtk_vars.osd_bg_color

  -- Hotkey popup

  --- Hotkeys widget background color.
  theme.hotkeys_bg               = theme.cobalt_bg
  --- Hotkeys widget foreground color.
  theme.hotkeys_fg               = theme.xres_vars.color2
  --- Hotkeys widget border width.
  -- theme.hotkeys_border_width
  --- Hotkeys widget border color.
  theme.hotkeys_border_color     = theme.accent
  --- Hotkeys widget shape.
  -- theme.hotkeys_shape
  --- Foreground color used for hotkey modifiers (Ctrl, Alt, Super, etc).
  theme.hotkeys_modifiers_fg     = theme.xres_vars.color8
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

  theme.system_black_dark = theme.xres_vars.color0
  theme.system_black_light = theme.xres_vars.color8

  theme.system_red_dark = theme.xres_vars.color1
  theme.system_red_light = theme.xres_vars.color9

  theme.system_green_dark = theme.xres_vars.color2
  theme.system_green_light = theme.xres_vars.color10

  theme.system_yellow_dark = theme.xres_vars.color3
  theme.system_yellow_light = theme.xres_vars.color11

  theme.system_blue_dark = theme.xres_vars.color4
  theme.system_blue_light = theme.xres_vars.color12

  theme.system_magenta_dark = theme.xres_vars.color5
  theme.system_magenta_light = theme.xres_vars.color13

  theme.system_cyan_dark = theme.xres_vars.color6
  theme.system_cyan_light = theme.xres_vars.color14

  theme.system_white_dark = theme.xres_vars.color7
  theme.system_white_light = theme.xres_vars.color15

  -- ░█▀█░█▀█░█▀█░█░░░▀█▀░█▀▀░█▀█░▀█▀░▀█▀░█▀█░█▀█░█▀▀
  -- ░█▀█░█▀▀░█▀▀░█░░░░█░░█░░░█▀█░░█░░░█░░█░█░█░█░▀▀█
  -- ░▀░▀░▀░░░▀░░░▀▀▀░▀▀▀░▀▀▀░▀░▀░░▀░░▀▀▀░▀▀▀░▀░▀░▀▀▀
  theme.teams_bg = '#444791'
  theme.spotify_bg = theme.success_bg

  -- ░█░█░▀█▀░█▀█░█▀▄░█▀█░█░█░░░█▀▀░█░█░▀█▀░▀█▀░█▀▀░█░█░█▀▀░█▀▄
  -- ░█▄█░░█░░█░█░█░█░█░█░█▄█░░░▀▀█░█▄█░░█░░░█░░█░░░█▀█░█▀▀░█▀▄
  -- ░▀░▀░▀▀▀░▀░▀░▀▀░░▀▀▀░▀░▀░░░▀▀▀░▀░▀░▀▀▀░░▀░░▀▀▀░▀░▀░▀▀▀░▀░▀
  theme.window_switcher_widget_bg                      = theme.popup_bg -- The bg color of the widget
  theme.window_switcher_widget_border_width            = theme.popup_border_width -- The border width of the widget
  theme.window_switcher_widget_border_radius           = theme.radius -- The border radius of the widget
  theme.window_switcher_widget_border_color            = theme.popup_border -- The border color of the widget
  theme.window_switcher_clients_spacing                = dpi(20) -- The space between each client item
  theme.window_switcher_client_icon_horizontal_spacing = dpi(5) -- The space between client icon and text
  theme.window_switcher_client_width                   = dpi(150) -- The width of one client widget
  theme.window_switcher_client_height                  = dpi(250) -- The height of one client widget
  theme.window_switcher_client_margins                 = dpi(10) -- The margin between the content and the border of the widget
  theme.window_switcher_thumbnail_margins              = dpi(10) -- The margin between one client thumbnail and the rest of the widget
  theme.thumbnail_scale                                = false -- If set to true, the thumbnails fit policy will be set to "fit" instead of "auto"
  theme.window_switcher_name_margins                   = dpi(10) -- The margin of one clients title to the rest of the widget
  theme.window_switcher_name_valign                    = 'center' -- How to vertically align one clients title
  theme.window_switcher_name_forced_width              = dpi(200) -- The width of one title
  theme.window_switcher_name_font                      = theme.font -- The font of all titles
  theme.window_switcher_name_normal_color              = theme.fg_normal -- The color of one title if the client is unfocused
  theme.window_switcher_name_focus_color               = theme.fg_focus -- The color of one title if the client is focused
  theme.window_switcher_icon_valign                    = 'center' -- How to vertically align the one icon
  theme.window_switcher_icon_width                     = dpi(40) -- The width of one icon

  -- ░▀█▀░█▀█░█▀▀░█░█░░░█▀█░█▀▄░█▀▀░█░█░▀█▀░█▀▀░█░█
  -- ░░█░░█▀█░▀▀█░█▀▄░░░█▀▀░█▀▄░█▀▀░▀▄▀░░█░░█▀▀░█▄█
  -- ░░▀░░▀░▀░▀▀▀░▀░▀░░░▀░░░▀░▀░▀▀▀░░▀░░▀▀▀░▀▀▀░▀░▀
  theme.task_preview_widget_border_radius = theme.radius -- Border radius of the widget (With AA)
  theme.task_preview_widget_bg            = theme.popup_bg -- The bg color of the widget
  theme.task_preview_widget_border_color  = theme.popup_border -- The border color of the widget
  theme.task_preview_widget_border_width  = theme.popup_border_width -- The border width of the widget
  theme.task_preview_widget_margin        = dpi(10) -- The margin of the widget

  -- ░▀█▀░█▀█░█▀▀░░░█▀█░█▀▄░█▀▀░█░█░▀█▀░█▀▀░█░█
  -- ░░█░░█▀█░█░█░░░█▀▀░█▀▄░█▀▀░▀▄▀░░█░░█▀▀░█▄█
  -- ░░▀░░▀░▀░▀▀▀░░░▀░░░▀░▀░▀▀▀░░▀░░▀▀▀░▀▀▀░▀░▀
  theme.tag_preview_widget_border_radius = theme.radius -- Border radius of the widget (With AA)
  theme.tag_preview_client_border_radius = theme.radius -- Border radius of each client in the widget (With AA)
  theme.tag_preview_client_opacity       = 0.8 -- Opacity of each client
  theme.tag_preview_client_bg            = theme.bg_normal -- The bg color of each client
  theme.tag_preview_client_border_color  = theme.border_normal -- The border color of each client
  theme.tag_preview_client_border_width  = theme.border_width -- The border width of each client
  theme.tag_preview_widget_bg            = theme.popup_bg -- The bg color of the widget
  theme.tag_preview_widget_border_color  = theme.popup_border -- The border color of the widget
  theme.tag_preview_widget_border_width  = theme.popup_border_width -- The border width of the widget
  theme.tag_preview_widget_margin        = dpi(5) -- The margin of the widget

  theme.graph_bg = theme.transparent -- The graph background color.
  theme.graph_fg = theme.fg_normal -- The graph foreground color.
  theme.graph_border_color = theme.border_normal -- The graph border color.
end

return {
  theme = theme,
  awesome_overrides = awesome_overrides
}

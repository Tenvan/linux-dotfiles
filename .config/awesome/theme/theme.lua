--[[

     Powerarrow Awesome WM theme
     github.com/lcpz

--]]

local log = require('utilities.debug').log
log("Enter Module => theme/theme.lua")

local gears = require("gears")

local awful = require("awful")
local wibox = require("wibox")
local dpi = require("beautiful.xresources").apply_dpi

local makeColorTransparent = require("utilities.utils").makeColorTransparent
local theme_dir = require("theme.default-theme").theme_dir

local cobalt_bg = "#072539";  -- Background
local cobalt_fg = "#e1efff";  -- Foreground
local cobalt_highlight = "#19acc6";  -- Highlight
local cobalt_accent_blue = "#0050a4";  -- Accent 1 (Blue)
local cobalt_accent_yellow = "#FFC600";  -- Accent 2 (Yellow)
local cobalt_window_bg = "#2e3a50";  -- Window Background

local base00 = "#232423" -- ---- red
local base01 = "#BA2922" -- ---  orange
local base02 = "#7E807E" -- --   yellow
local base03 = "#4C4F4D" -- -    green
local base04 = "#16A085" -- +    aqua/cyan
local base05 = "#43746A" -- ++   blue
local base06 = "#00CCCC" -- +++  purple
local base07 = "#E0E0E0" -- ++++ brown
local base08 = "#282928" -- red
local base09 = "#CC372C" -- orange
local base0A = "#8D8F8D" -- yellow
local base0B = "#4E524F" -- green
local base0C = "#13BF9D" -- aqua/cyan
local base0D = "#487D72" -- blue
local base0E = "#00D1D1" -- purple
local base0F = "#E8E8E8" -- brown

local theme = {}

theme.fg_normal = cobalt_fg
theme.bg_normal = cobalt_bg

theme.fg_focus = cobalt_fg
theme.bg_focus = cobalt_accent_blue

theme.bg_minimize = "#101010"
-- theme.bg_systray = cobalt9_3

theme.fg_urgent = "#ff0000"
theme.fg_minimize = "#ffffff"

theme.border_width = dpi(5)
theme.margins_width = dpi(10)

theme.border_normal = makeColorTransparent(cobalt_accent_blue, "80")
theme.border_focus = makeColorTransparent(cobalt_accent_yellow, "80")
theme.border_marked = cobalt_highlight

-- Default settings
theme.fg = theme.fg_normal
theme.bg = theme.bg_normal

-- General colors
theme.success_fg = base0C
theme.loaded_fg = base0D
theme.error_fg = cobalt_bg
theme.error_bg = base08

-- Warning colors
theme.warning_fg = cobalt_bg
theme.warning_bg = base0E

-- Notification colors
theme.notif_fg = cobalt_bg
theme.notif_bg = cobalt_fg

-- Menu colours
theme.menu_fg = cobalt_fg
theme.menu_bg = cobalt_bg
theme.menu_selected_fg = cobalt_bg
theme.menu_selected_bg = cobalt_accent_blue

theme.menu_title_bg = cobalt_bg
theme.menu_primary_title_fg = base05
theme.menu_secondary_title_fg = base04

theme.menu_disabled_fg = base08
theme.menu_disabled_bg = base00
theme.menu_enabled_fg = theme.menu_fg
theme.menu_enabled_bg = theme.menu_bg
theme.menu_active_fg = theme.menu_fg
theme.menu_active_bg = theme.menu_bg

-- Proxy manager
theme.proxy_active_menu_fg = base05
theme.proxy_active_menu_bg = cobalt_bg
theme.proxy_inactive_menu_fg = base03
theme.proxy_inactive_menu_bg = cobalt_bg

-- Statusbar specific
theme.sbar_fg = "red"
theme.sbar_bg = cobalt_bg

-- Downloadbar specific
theme.dbar_fg = "green"
theme.dbar_bg = base0D
theme.dbar_error_fg = base08

-- Input bar specific
theme.ibar_fg = "blue"
theme.ibar_bg = cobalt_bg

-- Tab label
theme.tab_fg = cobalt_fg
theme.tab_bg = cobalt_bg
theme.tab_hover_bg = cobalt_highlight
theme.selected_fg = cobalt_fg
theme.selected_bg = cobalt_highlight
theme.loading_fg = base0D
theme.loading_bg = "green"
theme.tab_ntheme = "blue"
theme.selected_ntheme = "red"

theme.selected_private_tab_bg = "red"
theme.private_tab_bg = "red"

-- Trusted/untrusted ssl colours
theme.trust_fg = base0B
theme.notrust_fg = base0D

-- Follow mode hints
theme.hint_fg = cobalt_fg
theme.hint_bg = cobalt_bg

-- theme.hint_border = string.format("1px dashed %s", base0A)
theme.hint_border = 10

-- General colour pairings
theme.ok = {
    fg = "white",
    bg = "red"
}
theme.warn = {
    fg = "white",
    bg = "green"
}
theme.error = {
    fg = "white",
    bg = "blue"
}

-- Font
theme.font_family = "Noto Sans Regular"
theme.hint_font = theme.font_base

-- End of oomox

theme.wallpaper = theme_dir .. "/wallpaper.jpg"

theme.bg_urgent = makeColorTransparent(theme.warning_bg, "80")
theme.fg_urgent = theme.warning_fg

theme.font_size = dpi(10)

theme.font = theme.font_family .. " " .. theme.font_size

theme.taglist_font = "Noto Sans Symbol Bold " .. (theme.font_size * 2)

-- theme.taglist_fg_focus = theme.selected_fg
-- theme.taglist_bg_focus = theme.selected_bg

-- theme.tasklist_fg_normal = theme.menu_fg
-- theme.tasklist_bg_normal = theme.menu_bg
-- theme.tasklist_fg_focus = theme.selected_fg
-- theme.tasklist_bg_focus = theme.selected_bg

-- theme.titlebar_fg_normal = theme.menu_fg
-- theme.titlebar_bg_normal = theme.menu_bg
-- theme.titlebar_fg_focus = theme.selected_fg
-- theme.titlebar_bg_focus = theme.selected_bg

-- theme.menu_bar = dpi(theme.font_size + theme.margins_width)
theme.menu_height = dpi(30)
theme.menu_width = dpi(300)

theme.notification_opacity = 1
-- theme.notification_bg = makeColorTransparent(theme.hint_bg, "50")
theme.notification_bg = theme.hint_bg
theme.notification_fg = theme.hint_fg
theme.notification_border_color = theme.border_normal
theme.notification_spacing = dpi(32)
theme.notification_opacity = 0.67
theme.notification_margin = dpi(32)

theme.tasklist_plain_task_name = false
theme.tasklist_disable_icon = false
theme.useless_gap = dpi(2)

theme.menu_submenu_icon = theme_dir .. "/icons/submenu.png"
theme.awesome_icon = theme_dir .. "/icons/awesome.png"
theme.taglist_squares_sel = theme_dir .. "/icons/square_sel.png"
theme.taglist_squares_unsel = theme_dir .. "/icons/square_unsel.png"
theme.widget_ac = theme_dir .. "/icons/ac.png"
theme.widget_battery = theme_dir .. "/icons/battery.png"
theme.widget_battery_low = theme_dir .. "/icons/battery_low.png"
theme.widget_battery_empty = theme_dir .. "/icons/battery_empty.png"
theme.widget_mem = theme_dir .. "/icons/mem.png"
theme.widget_cpu = theme_dir .. "/icons/cpu.png"
theme.widget_temp = theme_dir .. "/icons/temp.png"
theme.widget_net = theme_dir .. "/icons/net.png"
theme.widget_hdd = theme_dir .. "/icons/hdd.png"
theme.widget_music = theme_dir .. "/icons/note.png"
theme.widget_music_on = theme_dir .. "/icons/note.png"
theme.widget_music_pause = theme_dir .. "/icons/pause.png"
theme.widget_music_stop = theme_dir .. "/icons/stop.png"
theme.widget_vol = theme_dir .. "/icons/vol.png"
theme.widget_vol_low = theme_dir .. "/icons/vol_low.png"
theme.widget_vol_no = theme_dir .. "/icons/vol_no.png"
theme.widget_vol_mute = theme_dir .. "/icons/vol_mute.png"
theme.widget_mail = theme_dir .. "/icons/mail.png"
theme.widget_mail_on = theme_dir .. "/icons/mail_on.png"
theme.widget_task = theme_dir .. "/icons/task.png"
theme.widget_scissors = theme_dir .. "/icons/scissors.png"
theme.widget_weather = theme_dir .. "/icons/dish.png"

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

-- Accent color
theme.accent = theme.system_blue_dark

-- Background color
theme.background = makeColorTransparent(cobalt_window_bg, "80")

-- Transparent
theme.transparent = makeColorTransparent(cobalt_window_bg, "20")

-- Awesome icon
theme.awesome_icon = theme_dir .. "/icons/awesome.png"

-- Separators
-- local widget_seperator = wibox.container.margin(nil, theme.margins_width, theme.margins_width) -- wibox.container.textbox()
local widget_seperator = wibox.container.background(wibox.container.margin(nil, dpi(1)), theme.border_normal)

theme.tasklist_widget_template = {
    {
        {
            {
                {
                    id = "clienticon",
                    widget = awful.widget.clienticon
                },
                margins = theme.margins_width,
                widget = wibox.container.margin
            },
            {
                id = "text_role",
                widget = wibox.widget.textbox
            },
            layout = wibox.layout.fixed.horizontal
        },
        left = dpi(5),
        right = dpi(5),
        widget = wibox.container.margin
    },
    id = "background_role",
    widget = wibox.container.background,
    create_callback = function(self, c)
        self:get_children_by_id("clienticon")[1].client = c
    end
}

-- CPU Histogramm
-- vicious.cache(vicious.widgets.cpu)

-- local cpuHistogrammWidget = wibox.widget.graph()
-- cpuHistogrammWidget:set_width(100)
-- cpuHistogrammWidget:set_background_color(widget_bg_color)
-- cpuHistogrammWidget:set_color{
--     type = "linear",
--     from = {0, 0},
--     to = {50, 0},
--     stops = {{0, "#FF5656"}, {0.5, "#88A175"}, {1, "#AECF96"}}
-- }
-- vicious.register(cpuHistogrammWidget, vicious.widgets.cpu, "$1", 1)

-- Get CPU stats
-- local f = io.open("/proc/stat")
-- local cpu_kernels = 0
-- for line in f:lines() do
--     if string.sub(line, 1, 3) ~= "cpu" then
--         break
--     end
--     cpu_kernels = cpu_kernels + 1
-- end
-- f:close()

-- CPU Kernels Bar
-- local cpuKernelProgress = {}

-- local cpuKernelGridWidget = wibox.widget {
--     forced_num_cols = cpu_kernels - 1,
--     forced_num_rows = 1,
--     homogeneous = true,
--     expand = true,
--     layout = wibox.layout.grid
-- }

-- for i = 1, cpu_kernels - 1 do
--     cpuKernelProgress[i] = wibox.widget.progressbar()

--     local newKernelProgress = wibox.container.margin(wibox.widget {
--         {
--             max_value = 100,
--             value = 0,
--             forced_height = 12,
--             background_color = "alpha",
--             widget = cpuKernelProgress[i]
--         },
--         direction = "east",
--         layout = wibox.container.rotate
--     }, dpi(1))

--     cpuKernelGridWidget:add(newKernelProgress)
-- end

-- vicious.register(cpuKernelGridWidget, vicious.widgets.cpu, function(widget, args)
--     for i = 2, cpu_kernels do
--         cpuKernelProgress[i - 1]:set_value(args[i])
--     end
-- end, 3)

-- ...

local screen1LeftWidges = {
    layout = wibox.layout.fixed.horizontal,
    widget_seperator,
}

local screen2LeftWidges = {
    layout = wibox.layout.fixed.horizontal
}

theme.systray_icon_spacing = dpi(2)

local systray = wibox.widget.systray()

-- systray:set_base_size(dpi(18))
local systrayWidget = wibox.container.margin(systray, dpi(5), dpi(5), dpi(8), dpi(2))

local screen2RightWidgets = {
    -- Right widgets
    layout = wibox.layout.fixed.horizontal,
    systrayWidget,
    widget_seperator

}

local screen1RightWidgets = {
    -- Right widgets
    layout = wibox.layout.fixed.horizontal,
    widget_seperator,
    systrayWidget
}

function theme.at_screen_connect(s)
    -- Create a taglist widget
    local taglist = awful.widget.taglist {
        screen = s,
        filter = awful.widget.taglist.filter.all,
        buttons = awful.util.taglist_buttons
    }

    -- Create a tasklist widget
    local tasklist = awful.widget.tasklist {
        screen = s,
        filter = awful.widget.tasklist.filter.currenttags,
        buttons = awful.util.tasklist_buttons,
        style = {
            shape_border_width = dpi(3),
            shape_border_color = theme.tasklist_fg_focus,
            shape = gears.shape.rectangle
        }
    }

    -- Create the wibox
    local mywibox = awful.wibar({
        position = "top",
        screen = s,
        height = theme.menu_bar,
        bg = theme.bg_normal,
        fg = theme.fg_normal
    })

    -- Add widgets to the wibox

    local rightWidgets, leftWidgets

    if s.index == 1 then
        rightWidgets = screen1RightWidgets
        leftWidgets = screen1LeftWidges
    else
        rightWidgets = screen2RightWidgets
        leftWidgets = screen2LeftWidges
    end

    mywibox:setup {
        layout = wibox.layout.align.horizontal,
        {
            -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            awful.widget.layoutbox(s),
            taglist,
            leftWidgets
        },
        -- Middle widgets
        tasklist,
        -- Right widgets
        rightWidgets
    }

    s.mywibox = mywibox
end

local awesome_overrides = function(theme)
end

return {
    theme = theme,
    awesome_overrides = awesome_overrides
}

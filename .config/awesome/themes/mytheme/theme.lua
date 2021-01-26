--[[

     Powerarrow Awesome WM theme
     github.com/lcpz

--]]
local gears = require("gears")
local gdebug = require("gears.debug")

local lain = require("lain")
local awful = require("awful")
local wibox = require("wibox")
local dpi = require("beautiful.xresources").apply_dpi
local vicious = require("vicious")

local math, string, os = math, string, os
local ram_widget = require("awesome-wm-widgets.ram-widget.ram-widget")

local xres = require("beautiful.gtk").get_theme_variables()
gdebug.dump(xres)

-- current Values
-- base_color : #404552ff (string)
-- bg_color : #383c4aff (string)
-- button_bg_color : #444a58ff (string)
-- button_border_color : #2b2e39ff (string)
-- button_border_radius : 3.0 (number)
-- button_border_width : 1.0 (number)
-- button_fg_color : #d3dae3ff (string)
-- error_bg_color : #f04a50ff (string)
-- error_color : #fc4138ff (string)
-- error_fg_color : #ffffffff (string)
-- fg_color : #d3dae3ff (string)
-- font_family : Sans (string)
-- font_size : 15.36 (number)
-- header_button_bg_color : #2f343f00 (string)
-- header_button_border_color : #2f343f00 (string)
-- header_button_fg_color : #cfdae7cc (string)
-- menubar_bg_color : #2f343fff (string)
-- menubar_fg_color : #cfdae7cc (string)
-- osd_bg_color : #383c4aff (string)
-- osd_border_color : #d3dae3ff (string)
-- osd_fg_color : #d3dae3ff (string)
-- selected_bg_color : #5294e2ff (string)
-- selected_fg_color : #ffffffff (string)
-- success_bg_color : #73d216ff (string)
-- success_color : #73d216ff (string)
-- success_fg_color : #ffffffff (string)
-- text_color : #d3dae3ff (string)
-- tooltip_bg_color : #383c4aff (string)
-- tooltip_fg_color : #d3dae3ff (string)
-- warning_bg_color : #f27835ff (string)
-- warning_color : #f27835ff (string)
-- warning_fg_color : #ffffffff (string)
-- wm_bg_color : #2f343fff (string)
-- wm_border_focused_color : #5294e2ff (string)
-- wm_border_unfocused_color : #2f343fff (string)
-- wm_icons_focused_color : #ffffffff (string)
-- wm_icons_unfocused_color : #cfdae7cc (string)
-- wm_title_focused_color : #ffffffff (string)
-- wm_title_unfocused_color : #cfdae7cc (string)

local theme = {}

theme.xres = xres

theme.dir = os.getenv("HOME") .. "/.config/awesome/themes/mytheme"
theme.wallpaper = theme.dir .. "/wallpaper.jpg"

local function makeColorTransparent(colorkey)
    local alpha = 0xFFFFFF50
    local newcolorkey = colorkey and 0xFFFFFF00
    return colorkey
end

theme.border_normal = makeColorTransparent(xres.wm_border_unfocused_color)
theme.border_focus = makeColorTransparent(xres.wm_border_focused_color)
theme.border_marked = makeColorTransparent(xres.selected_bg_color)

theme.bg_normal = xres.bg_color
theme.fg_normal = xres.fg_color
theme.bg_focus = xres.wm_border_focused_color
theme.fg_focus = xres.fg_color
theme.bg_urgent = xres.warning_bg_color
theme.fg_urgent = xres.warning_color

theme.border_width = dpi(5)
theme.margins_width = dpi(10)

theme.font_size = xres.font_size
theme.font = xres.font_family .. theme.font_size

theme.taglist_font = "Twemoji " .. theme.font_size
theme.taglist_bg_focus = xres.selected_bg_color
theme.taglist_fg_focus = xres.selected_fg_color

theme.tasklist_bg_focus = xres.menubar_bg_color
theme.tasklist_fg_focus = xres.menubar_fg_color

theme.titlebar_bg_focus = xres.menubar_bg_color
theme.titlebar_fg_focus = xres.wm_title_focused_color
theme.titlebar_bg_normal = xres.menubar_bg_color
theme.titlebar_fg_normal = xres.wm_title_unfocused_color

-- theme.menu_bar = dpi(theme.font_size + theme.margins_width)
theme.menu_height = dpi(30)
theme.menu_width = dpi(300)

theme.notification_opacity = 0.90
theme.notification_bg = xres.tooltip_bg_color
theme.notification_fg = xres.tooltip_fg_color
theme.notification_border_color = xres.osd_border_color

theme.tasklist_plain_task_name = false
theme.tasklist_disable_icon = false
theme.useless_gap = dpi(2)

theme.menu_submenu_icon = theme.dir .. "/icons/submenu.png"
theme.awesome_icon = theme.dir .. "/icons/awesome.png"
theme.taglist_squares_sel = theme.dir .. "/icons/square_sel.png"
theme.taglist_squares_unsel = theme.dir .. "/icons/square_unsel.png"
theme.layout_tile = theme.dir .. "/icons/tile.png"
theme.layout_tileleft = theme.dir .. "/icons/tileleft.png"
theme.layout_tilebottom = theme.dir .. "/icons/tilebottom.png"
theme.layout_tiletop = theme.dir .. "/icons/tiletop.png"
theme.layout_fairv = theme.dir .. "/icons/fairv.png"
theme.layout_fairh = theme.dir .. "/icons/fairh.png"
theme.layout_spiral = theme.dir .. "/icons/spiral.png"
theme.layout_dwindle = theme.dir .. "/icons/dwindle.png"
theme.layout_max = theme.dir .. "/icons/max.png"
theme.layout_fullscreen = theme.dir .. "/icons/fullscreen.png"
theme.layout_magnifier = theme.dir .. "/icons/magnifier.png"
theme.layout_floating = theme.dir .. "/icons/floating.png"
theme.widget_ac = theme.dir .. "/icons/ac.png"
theme.widget_battery = theme.dir .. "/icons/battery.png"
theme.widget_battery_low = theme.dir .. "/icons/battery_low.png"
theme.widget_battery_empty = theme.dir .. "/icons/battery_empty.png"
theme.widget_mem = theme.dir .. "/icons/mem.png"
theme.widget_cpu = theme.dir .. "/icons/cpu.png"
theme.widget_temp = theme.dir .. "/icons/temp.png"
theme.widget_net = theme.dir .. "/icons/net.png"
theme.widget_hdd = theme.dir .. "/icons/hdd.png"
theme.widget_music = theme.dir .. "/icons/note.png"
theme.widget_music_on = theme.dir .. "/icons/note.png"
theme.widget_music_pause = theme.dir .. "/icons/pause.png"
theme.widget_music_stop = theme.dir .. "/icons/stop.png"
theme.widget_vol = theme.dir .. "/icons/vol.png"
theme.widget_vol_low = theme.dir .. "/icons/vol_low.png"
theme.widget_vol_no = theme.dir .. "/icons/vol_no.png"
theme.widget_vol_mute = theme.dir .. "/icons/vol_mute.png"
theme.widget_mail = theme.dir .. "/icons/mail.png"
theme.widget_mail_on = theme.dir .. "/icons/mail_on.png"
theme.widget_task = theme.dir .. "/icons/task.png"
theme.widget_scissors = theme.dir .. "/icons/scissors.png"
theme.widget_weather = theme.dir .. "/icons/dish.png"

-- Separators
-- local widget_seperator = wibox.container.margin(nil, theme.margins_width, theme.margins_width) -- wibox.container.textbox()
local widget_seperator = wibox.container.background(wibox.container.margin(nil, dpi(1)), theme.border_normal)

local widget_bg_color = theme.warning_bg_color

theme.titlebar_close_button_focus = theme.dir .. "/icons/titlebar/close_focus.png"
theme.titlebar_close_button_normal = theme.dir .. "/icons/titlebar/close_normal.png"
theme.titlebar_ontop_button_focus_active = theme.dir .. "/icons/titlebar/ontop_focus_active.png"
theme.titlebar_ontop_button_normal_active = theme.dir .. "/icons/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_inactive = theme.dir .. "/icons/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_inactive = theme.dir .. "/icons/titlebar/ontop_normal_inactive.png"
theme.titlebar_sticky_button_focus_active = theme.dir .. "/icons/titlebar/sticky_focus_active.png"
theme.titlebar_sticky_button_normal_active = theme.dir .. "/icons/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_inactive = theme.dir .. "/icons/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_inactive = theme.dir .. "/icons/titlebar/sticky_normal_inactive.png"
theme.titlebar_floating_button_focus_active = theme.dir .. "/icons/titlebar/floating_focus_active.png"
theme.titlebar_floating_button_normal_active = theme.dir .. "/icons/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_inactive = theme.dir .. "/icons/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_inactive = theme.dir .. "/icons/titlebar/floating_normal_inactive.png"
theme.titlebar_maximized_button_focus_active = theme.dir .. "/icons/titlebar/maximized_focus_active.png"
theme.titlebar_maximized_button_normal_active = theme.dir .. "/icons/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_inactive = theme.dir .. "/icons/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_inactive = theme.dir .. "/icons/titlebar/maximized_normal_inactive.png"

local markup = lain.util.markup

-- Textclock
local clockicon = wibox.widget.imagebox(theme.widget_clock)
local clock =
    awful.widget.watch(
    "date +'%a %d %b %R'",
    60,
    function(widget, stdout)
        widget:set_markup(" " .. markup.font(theme.font, stdout))
    end
)

local datewidget = wibox.widget.textbox()
vicious.register(datewidget, vicious.widgets.date, "%b %d, %R")

-- Calendar
theme.cal =
    lain.widget.cal(
    {
        attach_to = {clock},
        notification_preset = {
            font = theme.font,
            fg = theme.fg_normal,
            bg = theme.bg_normal
        }
    }
)

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

-- Battery
local baticon = wibox.widget.imagebox(theme.widget_battery)
local bat =
    lain.widget.bat(
    {
        settings = function()
            if bat_now.status and bat_now.status ~= "N/A" then
                if bat_now.ac_status == 1 then
                    widget:set_markup(markup.font(theme.font, " AC "))
                    baticon:set_image(theme.widget_ac)
                    return
                elseif not bat_now.perc and tonumber(bat_now.perc) <= 5 then
                    baticon:set_image(theme.widget_battery_empty)
                elseif not bat_now.perc and tonumber(bat_now.perc) <= 15 then
                    baticon:set_image(theme.widget_battery_low)
                else
                    baticon:set_image(theme.widget_battery)
                end
                widget:set_markup(markup.font(theme.font, " " .. bat_now.perc .. "% "))
            else
                widget:set_markup()
                baticon:set_image(theme.widget_ac)
            end
        end
    }
)

-- ALSA volume
local volicon = wibox.widget.imagebox(theme.widget_vol)
theme.volume =
    lain.widget.alsa(
    {
        settings = function()
            if volume_now.status == "off" then
                volicon:set_image(theme.widget_vol_mute)
            elseif tonumber(volume_now.level) == 0 then
                volicon:set_image(theme.widget_vol_no)
            elseif tonumber(volume_now.level) <= 50 then
                volicon:set_image(theme.widget_vol_low)
            else
                volicon:set_image(theme.widget_vol)
            end

            widget:set_markup(markup.font(theme.font, " " .. volume_now.level .. "% "))
        end
    }
)

-- MEM
local memwidget = wibox.widget.textbox()
vicious.cache(vicious.widgets.mem)
vicious.register(memwidget, vicious.widgets.mem, "$1% ($2MiB/$3MiB)", 7)

-- Graph
local graph_mem = wibox.widget.graph()
graph_mem:set_width(100)
graph_mem:set_background_color(widget_bg_color)
graph_mem:set_color(
    {
        type = "linear",
        from = {0, 0},
        to = {50, 0},
        stops = {{0, "#FF5656"}, {0.5, "#88A175"}, {1, "#AECF96"}}
    }
)

-- Register widget
vicious.cache(vicious.widgets.mem)
vicious.register(graph_mem, vicious.widgets.mem, "$1", 7)

-- CPU Histogramm
vicious.cache(vicious.widgets.cpu)

local cpuHistogrammWidget = wibox.widget.graph()
cpuHistogrammWidget:set_width(100)
cpuHistogrammWidget:set_background_color(widget_bg_color)
cpuHistogrammWidget:set_color {
    type = "linear",
    from = {0, 0},
    to = {50, 0},
    stops = {{0, "#FF5656"}, {0.5, "#88A175"}, {1, "#AECF96"}}
}
vicious.register(cpuHistogrammWidget, vicious.widgets.cpu, "$1", 1)

-- Get CPU stats
local f = io.open("/proc/stat")
local cpu_kernels = 0
for line in f:lines() do
    if string.sub(line, 1, 3) ~= "cpu" then
        break
    end
    cpu_kernels = cpu_kernels + 1
end
f:close()

-- CPU Kernels Bar
local cpuKernelProgress = {}

local cpuKernelGridWidget =
    wibox.widget {
    forced_num_cols = cpu_kernels - 1,
    forced_num_rows = 1,
    homogeneous = true,
    expand = true,
    layout = wibox.layout.grid
}

for i = 1, cpu_kernels - 1 do
    cpuKernelProgress[i] = wibox.widget.progressbar()

    local newKernelProgress =
        wibox.container.margin(
        wibox.widget {
            {
                max_value = 100,
                value = 0,
                forced_height = 12,
                -- border_width = 1,
                -- border_color =  theme.border_normal,
                background_color = "alpha",
                color = xres.warning_bg_color,
                widget = cpuKernelProgress[i]
            },
            direction = "east",
            layout = wibox.container.rotate
        },
        dpi(1)
    )

    cpuKernelGridWidget:add(newKernelProgress)
end

vicious.register(
    cpuKernelGridWidget,
    vicious.widgets.cpu,
    function(widget, args)
        for i = 2, cpu_kernels do
            cpuKernelProgress[i - 1]:set_value(args[i])
        end
    end,
    3
)

-- Coretemp (lain, average)
local temp =
    lain.widget.temp(
    {
        settings = function()
            widget:set_markup(markup.font(theme.font, " " .. coretemp_now .. "°C "))
        end
    }
)
--]]
local tempicon = wibox.widget.imagebox(theme.widget_temp)

-- Net
local neticon = wibox.widget.imagebox(theme.widget_net)
local net =
    lain.widget.net(
    {
        settings = function()
            widget:set_markup(
                markup.fontfg(theme.font, "#FEFEFE", " " .. net_now.received .. " ↓↑ " .. net_now.sent .. " ")
            )
        end
    }
)
local calendar_widget = require("awesome-wm-widgets.calendar-widget.calendar")

-- ...
-- Create a textclock widget
local mytextclock = wibox.widget.textclock()

local cw =
    calendar_widget(
    {
        theme = "outrun",
        placement = "bottom_right",
        radius = 8
    }
)

mytextclock:connect_signal(
    "button::press",
    function(_, _, _, button)
        if button == 1 then
            cw.toggle()
        end
    end
)

-- Storage
vicious.cache(vicious.widgets.fs)

local fsRootWidget = wibox.widget.textbox()
local fsWorkspaceWidget = wibox.widget.textbox()
local fsVmWidget = wibox.widget.textbox()
local fsBidataWidget = wibox.widget.textbox()
vicious.register(fsRootWidget, vicious.widgets.fs, "/:${/ avail_gb}GB", 61)
vicious.register(fsWorkspaceWidget, vicious.widgets.fs, "WS:${/media/WORKSPACE avail_gb}GB", 61)
vicious.register(fsVmWidget, vicious.widgets.fs, "VM:${/media/VM avail_gb}GB", 67)
vicious.register(fsBidataWidget, vicious.widgets.fs, "BD:${/media/BIGDATA avail_gb}GB", 63)

-- System Os
vicious.cache(vicious.widgets.os)

local sysOs = wibox.widget.textbox()
vicious.register(sysOs, vicious.widgets.os, "$1 $2", 86400)

-- System Host
local sysHost = wibox.widget.textbox()
vicious.register(sysHost, vicious.widgets.os, "$3@$4", 86400)

-- Packages
vicious.cache(vicious.widgets.pkg)

-- Up Time
vicious.cache(vicious.widgets.uptime)

local upTime = wibox.widget.textbox()
vicious.register(upTime, vicious.widgets.uptime, "Up: $1d $2h $3m", 60)

local screen1LeftWidges = {
    layout = wibox.layout.fixed.horizontal,
    wibox.container.background(wibox.container.margin(upTime, theme.margins_width, theme.margins_width)),
    widget_seperator,
    wibox.container.background(wibox.container.margin(sysOs, theme.margins_width, theme.margins_width)),
    widget_seperator,
    wibox.container.background(wibox.container.margin(sysHost, theme.margins_width, theme.margins_width)),
    widget_seperator
}

local screen2LeftWidges = {
    layout = wibox.layout.fixed.horizontal
}

-- theme.bg_systray = xres.warning_bg_color
theme.systray_icon_spacing = dpi(2)

local systray = wibox.widget.systray()

-- systray:set_base_size(dpi(18))
local systrayWidget = wibox.container.margin(systray, dpi(5), dpi(5), dpi(2), dpi(2))

local screen2RightWidgets = {
    -- Right widgets
    layout = wibox.layout.fixed.horizontal,
    widget_seperator,
    wibox.container.background(wibox.container.margin(fsRootWidget, theme.margins_width, theme.margins_width)),
    widget_seperator,
    wibox.container.background(wibox.container.margin(fsWorkspaceWidget, theme.margins_width, theme.margins_width)),
    widget_seperator,
    wibox.container.background(wibox.container.margin(fsVmWidget, theme.margins_width, theme.margins_width)),
    widget_seperator,
    wibox.container.background(wibox.container.margin(fsBidataWidget, theme.margins_width, theme.margins_width)),
    widget_seperator,
    wibox.container.background(wibox.container.margin(ram_widget(), theme.margins_width, theme.margins_width)),
    widget_seperator,
    wibox.container.background(wibox.container.margin(memwidget, theme.margins_width, theme.margins_width)),
    widget_seperator,
    wibox.container.background(wibox.container.margin(graph_mem, theme.margins_width, theme.margins_width)),
    widget_seperator,
    wibox.container.background(
        wibox.container.margin(
            wibox.widget {tempicon, temp.widget, layout = wibox.layout.align.horizontal},
            theme.margins_width,
            theme.margins_width
        )
    ),
    widget_seperator,
    wibox.container.background(
        wibox.container.margin(
            wibox.widget {volicon, theme.volume.widget, layout = wibox.layout.align.horizontal},
            theme.margins_width,
            theme.margins_width
        )
    ),
    widget_seperator,
    wibox.container.background(wibox.container.margin(datewidget, theme.margins_width, theme.margins_width * 2))
}

local screen1RightWidgets = {
    -- Right widgets
    layout = wibox.layout.fixed.horizontal,
    widget_seperator,
    wibox.container.background(
        wibox.container.margin(
            wibox.widget {nil, neticon, net.widget, layout = wibox.layout.align.horizontal},
            theme.margins_width,
            theme.margins_width
        )
    ),
    widget_seperator,
    wibox.container.background(wibox.container.margin(cpuKernelGridWidget, theme.margins_width, theme.margins_width)),
    widget_seperator,
    ram_widget(),
    widget_seperator,
    wibox.container.background(wibox.container.margin(datewidget, theme.margins_width, theme.margins_width)),
    widget_seperator,
    systrayWidget
}

function theme.at_screen_connect(s)
    -- Quake application
    -- s.quake = lain.util.quake({app = awful.util.terminal})
    s.quake = lain.util.quake({app = "kitty", height = 0.50, argname = "--title %s"})

    -- All tags open with layout 1
    awful.tag(awful.util.tagnames, s, awful.layout.layouts[1])

    -- Create a taglist widget
    local taglist =
        awful.widget.taglist {
        screen = s,
        filter = awful.widget.taglist.filter.all,
        buttons = awful.util.taglist_buttons
    }

    -- Create a tasklist widget
    local tasklist =
        awful.widget.tasklist {
        screen = s,
        filter = awful.widget.tasklist.filter.currenttags,
        buttons = awful.util.tasklist_buttons,
        style = {
            shape_border_width = 2,
            shape_border_color = theme.tasklist_fg_focus,
            shape = gears.shape.rectangle
        }
        -- widget_template = theme.tasklist_widget_template
    }

    -- Create the wibox
    local mywibox =
        awful.wibar(
        {
            position = "top",
            screen = s,
            height = theme.menu_bar,
            bg = theme.bg_normal,
            fg = theme.fg_normal
        }
    )

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

return theme

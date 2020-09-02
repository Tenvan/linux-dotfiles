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
local my_table = awful.util.table or gears.table -- 4.{0,1} compatibility

local theme = {}

local accent_color1 = "#497B96"
local accent_color2 = "#889FA7"

theme.dir = os.getenv("HOME") .. "/.config/awesome/themes/mytheme"
theme.wallpaper = theme.dir .. "/wallpaper.jpg"
theme.font = "Mononoki 12"
theme.taglist_font = "NotoEmoji Nerd Font Light 12"
theme.fg_normal = "#FEFEFE"
theme.fg_focus = accent_color2
theme.fg_urgent = "#b74822"
theme.bg_normal = "#000000"
theme.bg_focus = "#1E2320"
theme.bg_urgent = "#3F3F3F"
theme.taglist_fg_focus = accent_color2
theme.tasklist_bg_focus = "#000000"
theme.tasklist_fg_focus = accent_color2
theme.border_width = dpi(2)
theme.border_normal = "#3F3F3F"
theme.border_focus = "#6F6F6F"
theme.border_marked = "#CC9393"
theme.titlebar_bg_focus = "#3F3F3F"
theme.titlebar_bg_normal = "#3F3F3F"
theme.titlebar_bg_focus = theme.bg_focus
theme.titlebar_bg_normal = theme.bg_normal
theme.titlebar_fg_focus = theme.fg_focus
theme.menu_height = dpi(25)
theme.menu_width = dpi(260)
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
theme.tasklist_plain_task_name = true
theme.tasklist_disable_icon = true
theme.useless_gap = 0
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
local separators = lain.util.separators

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
            font = "Noto Sans Mono Medium 10",
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
                margins = dpi(4),
                widget = wibox.container.margin
            },
            {
                id = "text_role",
                widget = wibox.widget.textbox
            },
            layout = wibox.layout.fixed.horizontal
        },
        left = dpi(2),
        right = dpi(4),
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
graph_mem:set_width(50)
graph_mem:set_background_color(accent_color1)
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
cpuHistogrammWidget:set_width(50)
cpuHistogrammWidget:set_background_color(accent_color1)
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

gdebug.dump("Kernels:" .. cpu_kernels)

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
                forced_height = 16,
                border_width = dpi(1),
                border_color = accent_color2,
                color = {
                    type = "linear",
                    from = {0, 0},
                    to = {0, 50},
                    stops = {
                        {0, "#FF5656"},
                        {1, "#AECF96"}
                    }
                },
                widget = cpuKernelProgress[i]
            },
            direction = "east",
            layout = wibox.container.rotate
        },
        dpi(1)
        -- dpi(5)
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

-- Storage
vicious.cache(vicious.widgets.fs)

local fsRootWidget = wibox.widget.textbox()
vicious.register(fsRootWidget, vicious.widgets.fs, "ROOT:${/ avail_gb}GB", 61)
local fsBidataWidget = wibox.widget.textbox()
vicious.register(fsBidataWidget, vicious.widgets.fs, "BIGDATA:${/media/BIGDATA avail_gb}GB", 63)
local fsVmWidget = wibox.widget.textbox()
vicious.register(fsVmWidget, vicious.widgets.fs, "VM:${/media/VM avail_gb}GB", 67)

-- System Os
vicious.cache(vicious.widgets.os)

local sysOs = wibox.widget.textbox()
vicious.register(sysOs, vicious.widgets.os, "$1 $2", 86400)

-- System Host
local sysHost = wibox.widget.textbox()
vicious.register(sysHost, vicious.widgets.os, "$3@$4", 86400)

-- Packages
vicious.cache(vicious.widgets.pkg)

local pkgAll = wibox.widget.textbox()
vicious.register(pkgAll, vicious.widgets.pkg, "Updates: $1", 311, "Arch C")

-- Up Time
vicious.cache(vicious.widgets.uptime)

local upTime = wibox.widget.textbox()
vicious.register(upTime, vicious.widgets.uptime, "Up: $1d $2h $3m", 60)

-- Separators
local arrowl = separators.arrow_left
local arrowr = separators.arrow_right

function theme.at_screen_connect(s)
    local screen1LeftWidges = {
        layout = wibox.layout.fixed.horizontal,
        wibox.container.background(wibox.container.margin(upTime, dpi(3), dpi(3)), accent_color2),
        arrowr(accent_color2, accent_color1),
        wibox.container.background(wibox.container.margin(pkgAll, dpi(3), dpi(3)), accent_color1),
        arrowr(accent_color1, accent_color2),
        wibox.container.background(wibox.container.margin(sysOs, dpi(3), dpi(3)), accent_color2),
        arrowr(accent_color2, accent_color1),
        wibox.container.background(wibox.container.margin(sysHost, dpi(3), dpi(3)), accent_color1),
        arrowr(accent_color1, "alpha")
    }

    local screen2LeftWidges = {
        layout = wibox.layout.fixed.horizontal,
    }

    local screen1widgets = {
        -- Right widgets
        layout = wibox.layout.fixed.horizontal,
        -- using separators
        -- arrow("alpha", accent_color2),
        -- arrow(accent_color1, accent_color2),
        -- arrow(accent_color2, accent_color1),
        arrowl("alpha", accent_color1),
        wibox.container.background(
            wibox.container.margin(
                wibox.widget {nil, neticon, net.widget, layout = wibox.layout.align.horizontal},
                dpi(3),
                dpi(3)
            ),
            accent_color1
        ),
        arrowl(accent_color1, accent_color2),
        wibox.container.background(wibox.container.margin(cpuKernelGridWidget, dpi(3), dpi(3)), accent_color2),
        arrowl(accent_color2, accent_color1),
        wibox.container.background(wibox.container.margin(cpuHistogrammWidget), accent_color1),
        arrowl(accent_color1, accent_color2),
        wibox.container.background(
            wibox.container.margin(
                wibox.widget {volicon, theme.volume.widget, layout = wibox.layout.align.horizontal},
                dpi(3),
                dpi(3)
            ),
            accent_color2
        ),
        arrowl(accent_color2, accent_color1),
        wibox.container.background(wibox.container.margin(datewidget, dpi(4), dpi(8)), accent_color1),
        arrowl(accent_color1, "alpha"),
        wibox.widget.systray()
    }

    local screen2widgets = {
        -- Right widgets
        layout = wibox.layout.fixed.horizontal,
        -- using separators
        -- arrow("alpha", accent_color2),
        -- arrow(accent_color1, accent_color2),
        -- arrow(accent_color2, accent_color1),
        arrowl("alpha", accent_color1),
        wibox.container.background(wibox.container.margin(fsRootWidget, dpi(2), dpi(3)), accent_color1),
        arrowl(accent_color1, accent_color2),
        wibox.container.background(wibox.container.margin(fsBidataWidget, dpi(2), dpi(3)), accent_color2),
        arrowl(accent_color2, accent_color1),
        wibox.container.background(wibox.container.margin(fsVmWidget, dpi(2), dpi(3)), accent_color1),
        arrowl(accent_color1, accent_color2),
        wibox.container.background(wibox.container.margin(memwidget, dpi(2), dpi(3)), accent_color2),
        arrowl(accent_color2, accent_color1),
        wibox.container.background(wibox.container.margin(graph_mem, dpi(2), dpi(3)), accent_color1),
        arrowl(accent_color1, accent_color2),
        wibox.container.background(
            wibox.container.margin(
                wibox.widget {tempicon, temp.widget, layout = wibox.layout.align.horizontal},
                dpi(4),
                dpi(4)
            ),
            accent_color2
        ),
        arrowl(accent_color2, accent_color1),
        wibox.container.background(
            wibox.container.margin(
                wibox.widget {volicon, theme.volume.widget, layout = wibox.layout.align.horizontal},
                dpi(3),
                dpi(3)
            ),
            accent_color1
        ),
        arrowl(accent_color1, accent_color2),
        wibox.container.background(wibox.container.margin(datewidget, dpi(4), dpi(8)), accent_color2)
        -- arrow(accent_color1, "alpha"),
    }

    -- Quake application
    -- s.quake = lain.util.quake({app = awful.util.terminal})
    s.quake = lain.util.quake({app = "termite", height = 0.50, argname = "--title %s"})

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
        style    = {
            shape_border_width = 2,
            shape_border_color = accent_color1,
            shape  = gears.shape.rounded_bar,
        },
        widget_template = theme.tasklist_widget_template
    }

    -- Create the wibox
    local wibox_bar =
        awful.wibar(
        {
            position = "top",
            screen = s,
            height = dpi(22),
            bg = theme.bg_normal,
            fg = theme.fg_normal
        }
    )

    -- Add widgets to the wibox
    local widgets = screen2widgets
    local leftWidgets = screen2LeftWidges
    if s.index == 1 then
        widgets = screen1widgets
        leftWidgets = screen1LeftWidges
    end

    wibox_bar:setup {
        layout = wibox.layout.align.horizontal,
        {
            -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            awful.widget.layoutbox(s),
            taglist,
            leftWidgets,
        },
        tasklist, -- Middle widget
        widgets
    }
end

return theme

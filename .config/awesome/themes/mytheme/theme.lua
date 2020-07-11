--[[

     Powerarrow Awesome WM theme
     github.com/lcpz

--]]
local gears = require("gears")
local lain = require("lain")
local awful = require("awful")
local wibox = require("wibox")
local dpi = require("beautiful.xresources").apply_dpi

local math, string, os = math, string, os
local my_table = awful.util.table or gears.table -- 4.{0,1} compatibility

local theme = {}
theme.dir = os.getenv("HOME") .. "/.config/awesome/themes/mytheme"
theme.wallpaper = theme.dir .. "/wallpaper.jpg"
theme.font = "Mononoki 12"
theme.taglist_font = "NotoEmoji Nerd Font Light 12"
theme.fg_normal = "#FEFEFE"
theme.fg_focus = "#889FA7"
theme.fg_urgent = "#b74822" 
theme.bg_normal = "#000000"
theme.bg_focus = "#1E2320"
theme.bg_urgent = "#3F3F3F"
theme.taglist_fg_focus = "#889FA7"
theme.tasklist_bg_focus = "#000000"
theme.tasklist_fg_focus = "#889FA7"
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

function theme.at_screen_connect(s)
    -- Quake application
    -- s.quake = lain.util.quake({app = awful.util.terminal})
    s.quake = lain.util.quake({app = "termite", height = 0.50, argname = "--title %s"})

    -- If wallpaper is a function, call it with the screen
    -- local wallpaper = theme.wallpaper
    -- if type(wallpaper) == "function" then
    --     wallpaper = wallpaper(s)
    -- end
    -- gears.wallpaper.maximized(wallpaper, s, true)

    -- All tags open with layout 1
    awful.tag(awful.util.tagnames, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(
        my_table.join(
            awful.button(
                {},
                1,
                function()
                    awful.layout.inc(1)
                end
            ),
            awful.button(
                {},
                3,
                function()
                    awful.layout.inc(-1)
                end
            ),
            awful.button(
                {},
                4,
                function()
                    awful.layout.inc(1)
                end
            ),
            awful.button(
                {},
                5,
                function()
                    awful.layout.inc(-1)
                end
            )
        )
    )
    -- Create a taglist widget
    s.mytaglist =
        awful.widget.taglist {
        screen = s,
        filter = awful.widget.taglist.filter.all,
        buttons = awful.util.taglist_buttons
    }

    -- Create a tasklist widget
    --s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, awful.util.tasklist_buttons)

    -- Create the wibox
    s.mywibox =
        awful.wibar({position = "top", screen = s, height = dpi(30), bg = theme.bg_normal, fg = theme.fg_normal})

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        {
            -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            s.mylayoutbox,
            s.mytaglist
            --spr,
        },
        s.mytasklist, -- Middle widget
        {
            -- Right widgets
            layout = wibox.layout.fixed.horizontal
            --[[ using shapes
            pl(wibox.widget { mpdicon, theme.mpd.widget, layout = wibox.layout.align.horizontal }, "#343434"),
            pl(task, "#343434"),
            --pl(wibox.widget { mailicon, mail and theme.mail.widget, layout = wibox.layout.align.horizontal }, "#343434"),
            pl(wibox.widget { memicon, mem.widget, layout = wibox.layout.align.horizontal }, "#777E76"),
            pl(wibox.widget { cpuicon, cpu.widget, layout = wibox.layout.align.horizontal }, "#4B696D"),
            pl(wibox.widget { tempicon, temp.widget, layout = wibox.layout.align.horizontal }, "#4B3B51"),
            --pl(wibox.widget { fsicon, theme.fs and theme.fs.widget, layout = wibox.layout.align.horizontal }, "#CB755B"),
            pl(wibox.widget { baticon, bat.widget, layout = wibox.layout.align.horizontal }, "#8DAA9A"),
            pl(wibox.widget { neticon, net.widget, layout = wibox.layout.align.horizontal }, "#C0C0A2"),
            pl(binclock.widget, "#777E76"),
            --]]
            -- using separators
            --arrow(theme.bg_normal, "#343434"),
            -- wibox.container.background(wibox.container.margin(wibox.widget { mailicon, mail and mail.widget, layout = wibox.layout.align.horizontal }, dpi(4), dpi(7)), "#343434"),
            -- wibox.widget.systray(),
        }
    }
end

return theme

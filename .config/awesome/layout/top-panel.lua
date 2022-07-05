log('Enter Module => layout/top-panel.lua')

local beautiful = require('beautiful')
local dpi = beautiful.xresources.apply_dpi
local wibox = require('wibox')
local clickable_container = require('widget.clickable-container')
local task_list = require('widget.task-list')
local specs = require('layout.specs')

local offsetx = specs.leftPanel.actionBarWidth

local top_panel = function(s, offset)
  local panel = wibox {
    ontop = true,
    screen = s,
    type = 'dock',
    height = specs.topPanel.height,
    width = s.geometry.width - offsetx,
    x = s.geometry.x + offsetx,
    y = s.geometry.y,
    stretch = false,
    bg = beautiful.background,
    fg = beautiful.fg_normal
  }

  panel:struts {
    top = specs.topPanel.height
  }

  panel:connect_signal('mouse::enter', function()
    local w = mouse.current_wibox
    if w then
      w.cursor = 'left_ptr'
    end
  end)


  local clock              = require('widget.clock')(s)
  local layout_box         = require('widget.layoutbox')(s)
  local cpu_meter          = require('widget.cpu-meter-top-panel').widget
  local hard_drives        = require('widget.harddrive-meter').widget
  local weather            = require('widget.weather.weather-aw').widget
  local systray            = require('widget.systray').widget
  local net_speed_widget   = require('awesome-wm-widgets.net-speed-widget.net-speed')
  local ram_widget         = require('awesome-wm-widgets.ram-widget.ram-widget')
  local volume_widget      = require('awesome-wm-widgets.volume-widget.volume')
  local updater            = require('widget.package-updater')()
  local screen_rec         = require('widget.screen-recorder')()
  local info_center_toggle = require('widget.info-center-toggle')()

  panel:setup {
    layout = wibox.layout.align.horizontal,
    expand = 'none',
    {
      layout = wibox.layout.fixed.horizontal,
      layout_box,
      cpu_meter,
      task_list(s),
    },
    nil,
    {
      layout = wibox.layout.fixed.horizontal,
      spacing = dpi(5),
      net_speed_widget({
        width = 75
      }),
      weather,
      updater,
      hard_drives,
      ram_widget({
        widget_height = specs.topPanel.height,
        widget_width = specs.topPanel.height
      }),
      volume_widget({
        widget_type = 'arc',
        size = 32
      }),
      -- s.bluetooth,
      -- s.battery,
      screen_rec,
      systray,
      clock,
      info_center_toggle
    }
  }

  return panel
end

return top_panel

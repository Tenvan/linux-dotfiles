log('Enter Module => ' .. ...)

local beautiful = require('beautiful')
local dpi = beautiful.xresources.apply_dpi
local wibox = require('wibox')
local clickable_container = require('widget.clickable-container')
local task_list = require('widget.task-list')
local specs = require('layout.specs')
local config = require('configuration.config')

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
  local volume_widget      = require('awesome-wm-widgets.volume-widget.volume')
  local updater            = require('widget.package-updater')()
  local screen_rec         = require('widget.screen-recorder')()
  local info_center_toggle = require('widget.info-center-toggle')()

  local ram_widget = nil
  local ram_widget = require('widget.ram-widget.ram-widget')({
    -- width = dpi(200)
  })

  local color_palette = nil
  if config.debug_mode then
    color_palette = require('widget.color-palette.popup')
  end

  panel:setup {
    {
      -- left widgets
      layout_box,
      color_palette,
      cpu_meter,
      ram_widget,
      task_list(s),
      spacing = dpi(2),
      layout = wibox.layout.fixed.horizontal,
    },
    -- middle widgets
    nil,
    {
      -- right widgets
      layout = wibox.layout.fixed.horizontal,
      spacing = dpi(5),
      net_speed_widget({
        widget_width = dpi(66)
      }),
      weather,
      updater,
      hard_drives,
      volume_widget({
        widget_type = 'arc',
        size = dpi(32)
      }),
      -- s.bluetooth,
      -- s.battery,
      screen_rec,
      systray,
      clock,
      info_center_toggle
    },
    expand = 'none',
    spacing = 4,
    layout = wibox.layout.align.horizontal,
  }

  return panel
end

return top_panel

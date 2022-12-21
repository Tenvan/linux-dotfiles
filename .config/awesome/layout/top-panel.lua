log('Enter Module => ' .. ...)

local beautiful = require('beautiful')
local dpi = beautiful.xresources.apply_dpi
local wibox = require('wibox')
local specs = require('layout.specs')
local config = require('configuration.config')

local top_panel = function(s, offset)
  local panel = wibox {
    ontop = true,
    screen = s,
    type = 'dock',
    height = specs.topPanel.height,
    width = s.geometry.width,
    x = s.geometry.x,
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

  local clock                = require('widget.clock')(s)
  local layout_box           = require('widget.layoutbox')(s)
  local hard_drives          = require('widget.harddrive-meter').widget
  local weather              = require('widget.weather.weather-aw').widget
  local systray              = require('widget.systray').widget
  local net_speed_widget_old = require('awesome-wm-widgets.net-speed-widget.net-speed')
  local volume_widget        = require('awesome-wm-widgets.volume-widget.volume')
  local spotify_widget       = require('widget.spotify')

  local updater            = require('widget.package-updater')()
  local screen_rec         = require('widget.screen-recorder')()
  local info_center_toggle = require('widget.info-center-toggle')()
  local net_speed_widget   = require('widget.network-speed')
  local toolbar_ram_widget = require('widget.ram-widget.ram-widget')({
    -- width = dpi(200)
  })

  local seperator = {
    orientation = 'vertical',
    forced_width = dpi(5),
    color = beautiful.bg_focus,
    widget = wibox.widget.separator
  }

  local ram_widget = {
    {
      {
        text         = '',
        align        = 'center',
        valign       = 'center',
        font         = beautiful.font_large,
        forced_width = dpi(32),
        widget       = wibox.widget.textbox
      },
      margins = dpi(2),
      widget  = wibox.container.margin
    },
    nil,
    toolbar_ram_widget,
    layout = wibox.layout.align.horizontal
  }

  local cpu_meter = {
    {
      {
        text         = 'ﴟ',
        align        = 'center',
        valign       = 'center',
        font         = beautiful.font_large,
        forced_width = dpi(32),
        widget       = wibox.widget.textbox
      },
      margins = dpi(2),
      widget  = wibox.container.margin
    },
    require('widget.cpu-meter-top-panel.histo').widget,
    seperator,
    require('widget.cpu-meter-top-panel.kern').widget,
    seperator,
    spacing = dpi(2),
    layout = wibox.layout.fixed.horizontal
  }

  local color_palette = nil
  if config.debug_mode then
    color_palette = require('widget.color-palette.popup')
  end

  panel:setup {
    {
      -- left widgets
      layout_box,
      seperator,
      -- color_palette,
      -- seperator,
      cpu_meter,
      toolbar_ram_widget,
      seperator,
      net_speed_widget,
      seperator,
      spotify_widget,
      spacing = dpi(2),
      layout = wibox.layout.fixed.horizontal,
    },
    -- middle widgets
    nil,
    {
      -- right widgets
      layout = wibox.layout.fixed.horizontal,
      spacing = dpi(5),
      net_speed_widget_old({
        widget_width = dpi(66),
        width = dpi(100)
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

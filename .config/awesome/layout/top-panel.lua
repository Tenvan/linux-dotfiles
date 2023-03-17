log('Enter Module => ' .. ...)

local beautiful = require('beautiful')
local dpi       = beautiful.xresources.apply_dpi
local wibox     = require('wibox')
local specs     = require('layout.specs')
local config    = require('configuration.config')
local icon      = require('widget.icon')
local text      = require('widget.text')

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

  local clock              = require('widget.clock')(s)
  local layout_box         = require('widget.layoutbox')(s)
  local hard_drives        = require('widget.harddrive-meter').widget
  local weather            = require('widget.weather.weather-aw').widget
  local systray            = require('widget.systray').widget
  local spotify_widget     = require('widget.spotify')
  local updater            = require('widget.package-updater')()
  local screen_rec         = require('widget.screen-recorder')()
  local info_center_toggle = require('widget.info-center-toggle')()
  local net_speed_widget   = require('widget.network-speed')
  local toolbar_ram_widget = require('widget.ram-widget.ram-widget')({
    -- width = dpi(200)
  })

  local seperator          = {
    orientation = 'vertical',
    forced_width = dpi(5),
    color = beautiful.bg_focus,
    widget = wibox.widget.separator
  }

  local cpu_meter          = {
    icon({ icon = '🔥', label = 'CPU:' }),
    require('widget.cpu-meter-top-panel.histo').widget,
    seperator,
    require('widget.cpu-meter-top-panel.kern').widget,
    seperator,
    spacing = dpi(2),
    layout = wibox.layout.fixed.horizontal
  }

  local color_palette      = nil
  if config.debug_mode then
    color_palette = require('widget.color-palette.popup')
  end

  local leftWidgets = {
    spacing = dpi(2),
    layout = wibox.layout.fixed.horizontal,
    layout_box,
    seperator,
    -- color_palette,
    -- seperator,
    cpu_meter,
    icon({ icon = '🔥', label = 'RAM:' }),
    toolbar_ram_widget,
    seperator,
    icon({ icon = '🌐' }),
    net_speed_widget,
    seperator,
    icon({ icon = '🎹' }),
    spotify_widget
  }

  -- TODO config options optinal machen
  -- if config.widget.spotify.enabled then
  -- leftWidgets[#leftWidgets + 1] = spotify_widget
  -- end

  local middleWidgets = nil

  local rightWidgets = {
    weather,
    updater,
    hard_drives,
    screen_rec,
    systray,
    clock,
    info_center_toggle,
    spacing = dpi(5),
    layout = wibox.layout.fixed.horizontal,
  }
  panel:setup {
    leftWidgets,
    middleWidgets,
    rightWidgets,
    expand = 'none',
    spacing = 4,
    layout = wibox.layout.align.horizontal,
  }

  return panel
end

return top_panel

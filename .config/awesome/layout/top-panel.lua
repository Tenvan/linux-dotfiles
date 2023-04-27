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
  }

  -- add layout box widget
  if config.widget.layoutbox.enabled then
    leftWidgets[#leftWidgets + 1] = layout_box
    if config.widget.seperator.enabled then
      leftWidgets[#leftWidgets + 1] = seperator
    end
  end

  leftWidgets[#leftWidgets + 1] = color_palette
  if config.widget.seperator.enabled then
    leftWidgets[#leftWidgets + 1] = seperator
  end

  leftWidgets[#leftWidgets + 1] = cpu_meter
  if config.widget.seperator.enabled then
    leftWidgets[#leftWidgets + 1] = seperator
  end

  leftWidgets[#leftWidgets + 1] = icon({ icon = '🔥', label = 'RAM:' })
  leftWidgets[#leftWidgets + 1] = toolbar_ram_widget
  if config.widget.seperator.enabled then
    leftWidgets[#leftWidgets + 1] = seperator
  end

  leftWidgets[#leftWidgets + 1] = icon({ icon = '🌐' })
  leftWidgets[#leftWidgets + 1] = net_speed_widget
  if config.widget.seperator.enabled then
    leftWidgets[#leftWidgets + 1] = seperator
  end

  if config.widget.spotify.enabled then
    leftWidgets[#leftWidgets + 1] = icon({ icon = '🎹' })
    leftWidgets[#leftWidgets + 1] = spotify_widget
    if config.widget.seperator.enabled then
      leftWidgets[#leftWidgets + 1] = seperator
    end
  end

  local middleWidgets = nil

  local rightWidgets = {
    spacing = dpi(5),
    layout = wibox.layout.fixed.horizontal,
  }

  rightWidgets[#rightWidgets + 1] = seperator
  rightWidgets[#rightWidgets + 1] = weather
  rightWidgets[#rightWidgets + 1] = seperator
  rightWidgets[#rightWidgets + 1] = updater
  rightWidgets[#rightWidgets + 1] = seperator
  rightWidgets[#rightWidgets + 1] = hard_drives
  rightWidgets[#rightWidgets + 1] = seperator
  rightWidgets[#rightWidgets + 1] = screen_rec
  rightWidgets[#rightWidgets + 1] = seperator
  rightWidgets[#rightWidgets + 1] = systray
  rightWidgets[#rightWidgets + 1] = seperator
  rightWidgets[#rightWidgets + 1] = clock
  rightWidgets[#rightWidgets + 1] = seperator
  rightWidgets[#rightWidgets + 1] = info_center_toggle

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

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

  local seperator                 = {
    orientation = 'vertical',
    forced_width = dpi(5),
    color = beautiful.bg_focus,
    widget = wibox.widget.separator
  }

  local layout_box_widget         = require('widget.layoutbox')(s)
  local clock_widget              = require('widget.clock')(s)
  local hard_drives_widget        = require('widget.harddrive-meter').widget
  local weather_widget            = require('widget.weather.weather-aw').widget
  local systray_widget            = require('widget.systray').widget
  local spotify_widget            = require('widget.spotify')
  local updater_widget            = require('widget.package-updater')()
  local screen_rec_widget         = require('widget.screen-recorder')()
  local info_center_toggle_widget = require('widget.info-center-toggle')()
  local net_speed_widget          = require('widget.network-speed')
  local ram_widget                = require('widget.ram-widget.ram-widget')({})
  local cpu_widget                = {
    require('widget.cpu-meter-top-panel.histo').widget,
    require('widget.cpu-meter-top-panel.kern').widget,
    spacing = dpi(2),
    layout = wibox.layout.fixed.horizontal
  }
  local color_palette_widget      = require('widget.color-palette.popup')

  local function setWidgetsFromConfig(widgets, config)
    dump(config)
    for i = 1, #config do
      local widget_name = config[i]
      log('add widget ' .. tostring(widget_name))

      if widget_name == 'seperator' then
        widgets[#widgets + 1] = seperator
      end
      if widget_name == 'fire_icon' then
        widgets[#widgets + 1] = icon({ icon = '🔥' })
      end
      if widget_name == 'net_icon' then
        widgets[#widgets + 1] = icon({ icon = '🌐' })
      end
      if widget_name == 'media_icon' then
        widgets[#widgets + 1] = icon({ icon = '🎹' })
      end
      if widget_name == 'clock_icon' then
        widgets[#widgets + 1] = icon({ icon = '🕰️' })
      end
      if widget_name == 'color_palette' then
        widgets[#widgets + 1] = color_palette_widget
      end
      if widget_name == 'layout_box' then
        widgets[#widgets + 1] = layout_box_widget
      end
      if widget_name == 'cpu' then
        widgets[#widgets + 1] = cpu_widget
      end
      if widget_name == 'ram' then
        widgets[#widgets + 1] = ram_widget
      end
      if widget_name == 'net_speed' then
        widgets[#widgets + 1] = net_speed_widget
      end
      if widget_name == 'spotify' then
        widgets[#widgets + 1] = spotify_widget
      end
      if widget_name == 'weather' then
        widgets[#widgets + 1] = weather_widget
      end
      if widget_name == 'updater' then
        widgets[#widgets + 1] = updater_widget
      end
      if widget_name == 'hard_drives' then
        widgets[#widgets + 1] = hard_drives_widget
      end
      if widget_name == 'screen_rec' then
        widgets[#widgets + 1] = screen_rec_widget
      end
      if widget_name == 'systray' then
        widgets[#widgets + 1] = systray_widget
      end
      if widget_name == 'clock' then
        widgets[#widgets + 1] = clock_widget
      end
      if widget_name == 'info_center_toggle' then
        widgets[#widgets + 1] = info_center_toggle_widget
      end
    end
  end


  local leftWidgets = {
    spacing = dpi(2),
    layout = wibox.layout.fixed.horizontal,
  }
  setWidgetsFromConfig(leftWidgets, config.widget.left_widgets[1])
  setWidgetsFromConfig(leftWidgets, config.widget.left_widgets[s.index + 1])

  local middleWidgets = {
    spacing = dpi(2),
    layout = wibox.layout.fixed.horizontal,
  }
  setWidgetsFromConfig(middleWidgets, config.widget.center_widgets[1])
  setWidgetsFromConfig(middleWidgets, config.widget.center_widgets[s.index + 1])

  local rightWidgets = {
    spacing = dpi(5),
    layout = wibox.layout.fixed.horizontal,
  }
  setWidgetsFromConfig(rightWidgets, config.widget.right_widgets[1])
  setWidgetsFromConfig(rightWidgets, config.widget.right_widgets[s.index + 1])


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

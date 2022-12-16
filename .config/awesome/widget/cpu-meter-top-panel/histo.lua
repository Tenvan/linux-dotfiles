local cpu_widget = require('awesome-wm-widgets.cpu-widget.cpu-widget')
local beautiful = require('beautiful')

local dpi = require('beautiful').xresources.apply_dpi

return {
  widget = cpu_widget({
    width = dpi(150),
    step_width = dpi(1),
    step_spacing = dpi(0),
    color = beautiful.accent,
    background_color = beautiful.background,
    enable_kill_button = true
  })
}

local cpu_widget = require('awesome-wm-widgets.cpu-widget.cpu-widget')
local beautiful = require('beautiful')

local dpi = require('beautiful').xresources.apply_dpi

return {
  widget = cpu_widget({
    width = dpi(120),
    step_width = dpi(5),
    step_spacing = dpi(1),
    color = beautiful.accent,
    background_color = beautiful.popup_bg,
    enable_kill_button = true
  })
}

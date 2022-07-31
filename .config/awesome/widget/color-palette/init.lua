log('Enter Module => ' .. ...)

local wibox = require('wibox')
local awful = require('awful')
local gears = require('gears')
local beautiful = require('beautiful')

local gtk_variable = beautiful.gtk.get_theme_variables
local dpi = beautiful.xresources.apply_dpi

local gtkVars = {
  'bg_color',
  'fg_color',
  'base_color',
  'text_color',
  'selected_bg_color',
  'selected_fg_color',
  'tooltip_fg_color',
  'osd_bg_color',
  'osd_fg_color',
  'osd_border_color',
  'menubar_bg_color',
  'menubar_fg_color',
  'button_fg_color',
  'header_button_bg_color',
  'header_button_fg_color',
  'wm_border_focused_color',
  'wm_border_unfocused_color',
  'wm_title_focused_color',
  'wm_title_unfocused_color',
  'wm_icons_focused_color',
  'wm_icons_unfocused_color',
  'error_bg_color',
  'error_fg_color',
  'error_color',
  'warning_color',
  'warning_bg_color',
  'warning_fg_color',
  'warning_color',
  'success_color',
  'success_bg_color',
  'success_fg_color',
  'success_color',
}

local colorArray = {
  layout = wibox.layout.fixed.vertical,
  spacing = dpi(1),
}

for i = 1, #gtkVars, 1 do
  local name = gtkVars[i]
  local sampleText = 'Hello World !!'
  local color = gtk_variable()[name]

  local widget = {
    {
      {
        {
          forced_width = dpi(340),
          text        = i .. ': ' .. name,
          widget       = wibox.widget.textbox
        },
        {
          {
            forced_width = dpi(200),
            markup       = '<span foreground="' .. beautiful.fg_normal:sub(1, 7) .. 'A0' .. '">' .. sampleText .. '</span>',
            widget       = wibox.widget.textbox
          },
          bg     = gtk_variable()[gtkVars[i]],
          widget = wibox.container.background
        },
        {
          forced_width = dpi(200),
          markup       = '<span foreground="' .. color .. '">' .. sampleText .. '</span>',
          widget       = wibox.widget.textbox
        },
        spacing = 10,
        widget = wibox.layout.fixed.horizontal,
      },
      margins = dpi(2),
      widget = wibox.container.margin
    },
    margins = dpi(1),
    widget = wibox.container.margin
  }
  table.insert(colorArray, widget)
end

return wibox.widget {
  colorArray,
  widget = wibox.container.background
}

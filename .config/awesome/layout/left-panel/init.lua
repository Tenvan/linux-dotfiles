local log = require('utilities.debug').log
local dump = require('utilities.debug').dump
log('Enter Module => layout/left-panel/init.lua')

local awful = require('awful')
local wibox = require('wibox')
local beautiful = require('beautiful')
local gears = require('gears')
local dpi = beautiful.xresources.apply_dpi

local left_panel = function(s)
  -- Set left panel geometry
  local panel_content_width = dpi(350)
  local action_bar_width = dpi(45)
  local offsety = dpi(28)

  local panel = wibox {
    type = 'dock',
    ontop = true,
    screen = s,
    visible = false,
    width = action_bar_width,
    height = s.geometry.height - offsety,
    x = s.geometry.x,
    y = s.geometry.y + offsety,
    bg = beautiful.background,
    fg = beautiful.fg_normal,
    shape = gears.shape.rectangle,
  }

  panel.opened = false

  s.backdrop_ldb = wibox {
    ontop = true,
    screen = s,
    bg = beautiful.transparent,
    type = 'utility',
    x = s.geometry.x,
    y = s.geometry.y,
    width = s.geometry.width,
    height = s.geometry.height
  }

  panel:struts {
    left = action_bar_width
  }

  -- "Punch a hole" on backdrop to show the left dashboard
  local update_backdrop = function(wibox_backdrop, wibox_panel)
    local cairo = require('lgi').cairo
    local geo = wibox_panel.screen.geometry

    wibox_backdrop.x = geo.x
    wibox_backdrop.y = geo.y
    wibox_backdrop.width = geo.width
    wibox_backdrop.height = geo.height

    -- Create an image surface that is as large as the wibox_panel screen
    local shape = cairo.ImageSurface.create(cairo.Format.A1, geo.width, geo.height)
    local cr = cairo.Context(shape)

    -- Fill with "completely opaque"
    cr.operator = 'SOURCE'
    cr:set_source_rgba(1, 1, 1, 1)
    cr:paint()

    -- Remove the shape of the client
    local c_geo = wibox_panel:geometry()
    local c_shape = gears.surface(wibox_panel.shape_bounding)
    cr:set_source_rgba(0, 0, 0, 0)
    cr:mask_surface(c_shape, c_geo.x + wibox_panel.border_width - geo.x, c_geo.y + wibox_panel.border_width - geo.y)
    c_shape:finish()

    wibox_backdrop.shape_bounding = shape._native
    shape:finish()
    wibox_backdrop:draw()
  end

  local open_panel = function()
    panel.width = action_bar_width + panel_content_width
    s.backdrop_ldb.visible = true

    local focused = awful.screen.focused()
    focused.backdrop_ldb.visible = true

    -- update_backdrop(screen.backdrop_ldb, panel)

    panel:get_children_by_id('panel_content')[1].visible = true

    panel:emit_signal('opened')
  end

  local close_panel = function()
    panel.width = action_bar_width
    panel:get_children_by_id('panel_content')[1].visible = false

    local focused = awful.screen.focused()
    focused.backdrop_ldb.visible = false

    -- update_backdrop(screen.backdrop_ldb, panel)

    panel:emit_signal('closed')
  end

  -- Hide this panel when app dashboard is called.
  function panel:hide_dashboard()
    close_panel()
  end

  function panel:toggle()
    self.opened = not self.opened
    if self.opened then
      open_panel()
    else
      close_panel()
    end
  end

  s.backdrop_ldb:buttons(awful.util.table.join(awful.button({}, 1, function()
    panel:toggle()
  end)))

  panel:setup {
    layout = wibox.layout.align.horizontal,
    nil,
    {
      id = 'panel_content',
      bg = beautiful.transparent,
      widget = wibox.container.background,
      visible = true,
      forced_width = panel_content_width,
      {
        require('layout.left-panel.dashboard')(s, panel),
        layout = wibox.layout.stack
      }
    },
    require('layout.left-panel.action-bar')(s, panel, action_bar_width)
  }
  return panel
end

return left_panel

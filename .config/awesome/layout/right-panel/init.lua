local log = require('utilities.debug').log
local dump = require('utilities.debug').dump
log('Enter Module => layout/right-panel/init.lua')

local awful = require('awful')
local wibox = require('wibox')
local beautiful = require('beautiful')
local dpi = beautiful.xresources.apply_dpi

--Erstellt Right Panel
---@param pScreen screen
---@return dock
local right_panel = function(pScreen)
  -- Set right panel geometry
  local panel_width = dpi(350)
  local panel_x = pScreen.geometry.x + pScreen.geometry.width - panel_width

  local panel = wibox {
    type = 'dock',
    ontop = true,
    screen = pScreen,
    width = panel_width,
    height = pScreen.geometry.height,
    x = panel_x,
    y = pScreen.geometry.y,
    bg = beautiful.background,
    fg = beautiful.fg_normal,
    visible = false,
  }

  panel.opened = false

  pScreen.backdrop_rdb = wibox {
    ontop = true,
    screen = pScreen,
    bg = beautiful.transparent,
    fg = beautiful.fg_normal,
    type = 'splash',
    x = pScreen.geometry.x,
    y = pScreen.geometry.y,
    width = pScreen.geometry.width,
    height = pScreen.geometry.height
  }

  panel:struts {
    right = 0
  }

  local open_panel = function()
    local focused = awful.screen.focused()

    focused.backdrop_rdb.visible = true
    focused.right_panel.visible = true

    panel:emit_signal('opened')
  end

  local close_panel = function()
    local focused = awful.screen.focused()

    focused.right_panel.visible = false
    focused.backdrop_rdb.visible = false

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

  function panel:switch_pane(mode)
    if mode == 'notif_mode' then
      -- Update Content
      panel:get_children_by_id('notif_id')[1].visible = true
      panel:get_children_by_id('pane_id')[1].visible = false
    elseif mode == 'today_mode' then
      -- Update Content
      panel:get_children_by_id('notif_id')[1].visible = false
      panel:get_children_by_id('pane_id')[1].visible = true
    end
  end

  pScreen.backdrop_rdb:buttons(awful.util.table.join(awful.button({}, 1, function()
    panel:toggle()
  end)))

  local separator = wibox.widget {
    orientation = 'horizontal',
    opacity = 0.0,
    forced_height = 15,
    widget = wibox.widget.separator
  }

  local line_separator = wibox.widget {
    orientation = 'horizontal',
    forced_height = dpi(1),
    span_ratio = 1.0,
    color = beautiful.groups_title_bg,
    widget = wibox.widget.separator
  }

  panel:setup {
    {
      expand = 'none',
      layout = wibox.layout.fixed.vertical,
      {
        layout = wibox.layout.align.horizontal,
        expand = 'none',
        nil,
        require('widget.info-center-switch'),
        nil
      },
      separator,
      line_separator,
      separator,
      {
        layout = wibox.layout.stack,
        -- Today Pane
        {
          id = 'pane_id',
          visible = true,
          layout = wibox.layout.fixed.vertical,
          {
            require('widget.user-profile'),
            require('widget.weather'),
            require('widget.email'),
            -- require('widget.social-media'),
            require('widget.calculator'),
            spacing = dpi(7),
            layout = wibox.layout.fixed.vertical,
          }

        },
        -- Notification Center
        {
          id = 'notif_id',
          visible = false,
          require('widget.notif-center')(pScreen),
          layout = wibox.layout.fixed.vertical
        }
      }
    },
    margins = dpi(16),
    widget = wibox.container.margin
  }

  return panel
end

return right_panel

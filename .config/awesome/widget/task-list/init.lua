local tasklist_buttons = awful.util.table.join(
  awful.button(
    {},
    1,
    function(c)
      if c == client.focus then
        c.minimized = true
      else
        -- Without this, the following
        -- :isvisible() makes no sense
        c.minimized = false
        if not c:isvisible() and c.first_tag then
          c.first_tag:view_only()
        end
        -- This will also un-minimize
        -- the client, if needed
        c:emit_signal('request::activate')
        c:raise()
      end
    end
  ),
  awful.button(
    {},
    2,
    function(c)
      c:kill()
    end
  ),
  awful.button(
    {},
    3,
    function(c)
      log('Right Mouse Button pressed')
      emit('bling::task_preview::visibility', nil, false, c, c)

      -- TODO Menü fertig stellen
      -- @todo Menü fertig stellen
      local menu = {
        { 'Schließen',
          function()
            c:kill()
          end },
        { 'Minimieren',
          function()
            c:minimize()
          end },
        { 'Maximieren',
          function()
            c:maximize()
          end },
        { 'Abbrechen',
          function()
            c.popup_menu:hide()
          end }
      }
      c.popup_menu = awful.menu(menu)
      c.popup_menu:show()

      c.popup_menu:connect_signal('mouse::enter', function()
        log('Enter Menu')
      end)

      c.popup_menu:connect_signal('mouse::leave', function()
        log('Enter Menu')
      end)
    end
  ),
  awful.button(
    {},
    4,
    function()
      awful.client.focus.byidx(1)
    end
  ),
  awful.button(
    {},
    5,
    function()
      awful.client.focus.byidx(-1)
    end
  )
)

local task_list = function(pScreen)
  return awful.widget.tasklist {
    screen          = pScreen,
    filter          = awful.widget.tasklist.filter.currenttags,
    buttons         = tasklist_buttons,
    layout          = {
      spacing_widget = {
        {
          forced_width  = dpi(5),
          forced_height = dpi(24),
          thickness     = 1,
          color         = beautiful.separator_color,
          widget        = wibox.widget.separator
        },
        valign = 'center',
        halign = 'center',
        widget = wibox.container.place,
      },
      spacing        = dpi(1),
      layout         = wibox.layout.fixed.horizontal
    },
    -- Notice that there is *NO* wibox.wibox prefix, it is a template,
    -- not a widget instance.
    widget_template = {
      {
        {
          {
            {
              id     = 'icon_role',
              -- widget = awful.widget.clienticon,
              widget = awful.widget.clienticon,
            },
            margins = 2,
            widget  = wibox.container.margin,
          },
          {
            id     = 'my_text_role',
            widget = wibox.widget.textbox,
          },
          layout = wibox.layout.fixed.horizontal,
        },
        left   = 10,
        right  = 10,
        widget = wibox.container.margin
      },

      id = 'my_background_role',
      widget = wibox.container.background,

      create_callback = function(self, c, index, objects)
        local iconbox = self:get_children_by_id('icon_role')[1]
        iconbox.client = c

        -- BLING: Toggle the popup on hover and disable it off hover
        self:connect_signal('mouse::enter', function()
          emit('bling::task_preview::visibility', pScreen, true, c, self)
        end)

        self:connect_signal('mouse::leave', function()
          emit('bling::task_preview::visibility', pScreen, false, c, self)
        end)
      end,
      update_callback = function(self, c, index, objects)
        local background = self:get_children_by_id('my_background_role')[1]
        background.bg = beautiful.background

        if c == client.focus then
          log('Focused Client: ' .. c.name)
          background.bg = beautiful.bg_focus
        end
        if c.minimized then
          log('Minimized Client: ' .. c.name)
          background.bg = beautiful.bg_urgent
        end

        local textbox = self:get_children_by_id('my_text_role')[1]
        local textIcon = 'O'
        if c.minimized then
          textbox:set_text('(' .. c.name .. ')')
        else
          textbox:set_text(c.name)
        end
      end,
      -- layout = wibox.layout.align.vertical,
    },
  }
end

return task_list

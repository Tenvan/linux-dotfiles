local awful = require('awful')
local wibox = require('wibox')
local beautiful = require('beautiful')

local dpi = require('beautiful').xresources.apply_dpi

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
              widget = awful.widget.clienticon,
            },
            margins = 2,
            widget  = wibox.container.margin,
          },
          {
            id     = 'text_role',
            widget = wibox.widget.textbox,
          },
          layout = wibox.layout.fixed.horizontal,
        },
        left   = 10,
        right  = 10,
        widget = wibox.container.margin
      },

      id = 'background_role',
      widget = wibox.container.background,

      create_callback = function(self, c, index, objects)
        self:get_children_by_id('icon_role')[1].client = c

        -- BLING: Toggle the popup on hover and disable it off hover
        self:connect_signal('mouse::enter', function()
          awesome.emit_signal('bling::task_preview::visibility', pScreen, true, c, self)
        end)

        self:connect_signal('mouse::leave', function()
          awesome.emit_signal('bling::task_preview::visibility', pScreen, false, c, self)
        end)
      end,
      -- layout = wibox.layout.align.vertical,
    },
  }
end

return task_list

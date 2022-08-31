log('Enter Module => ' .. ...)

local awful = require('awful')
local wibox = require('wibox')
local beautiful = require('beautiful')

local tag_list = function(pScreen)
  return awful.widget.taglist {
    screen = pScreen,
    filter = awful.widget.taglist.filter.all,

    buttons = awful.util.table.join(
      awful.button({}, 1,
        function(t)
          t:view_only()
        end)
    ),

    widget_template = {
      {
        {
          {
            {
              {
                id     = 'index_role',
                widget = wibox.widget.textbox,
              },
              halign = 'center',
              valign = 'center',
              layout = wibox.container.place,
            },
            forced_height = beautiful.element_size,
            forced_width = beautiful.element_size,
            widget = wibox.container.background,
          },
          {
            id     = 'icon_role',
            -- margins = dpi(2),
            widget = wibox.container.margin,
          },
          layout = wibox.layout.fixed.horizontal,
        },
        widget = wibox.container.margin
      },
      id = 'background_role',
      -- widget = clickable_container, -- wibox.container.background,
      widget = wibox.container.background, -- wibox.container.background,

      -- Add support for hover colors and an index label
      create_callback = function(self, c3, index, objects)
        local index_role = self:get_children_by_id('index_role')[1]
        index_role.markup = "<span font_desc='" ..
          beautiful.taglist_font .. "'>" .. index .. '</span>'

        local tag = objects[index]
        local current_tag = tag.screen.selected_tag

        self:connect_signal('mouse::enter', function()
          -- BLING: Only show widget when there are clients in the tag
          if #c3:clients() > 0 then
            -- BLING: Update the widget with the new tag
            awesome.emit_signal('bling::tag_preview::update', c3)
            -- BLING: Show the widget
            awesome.emit_signal('bling::tag_preview::visibility', pScreen, true, self)
          end

          if self.bg ~= beautiful.accent then
            self.backup_bg  = self.bg
            self.backup_fg  = self.fg
            self.has_backup = true
          end
          self.bg = beautiful.accent:sub(1, 7) .. '80'
          self.fg = beautiful.bg_normal
        end)

        self:connect_signal('mouse::leave', function()
          -- BLING: Turn the widget off
          awesome.emit_signal('bling::tag_preview::visibility', pScreen, false, self)

          local t = awful.screen.focused().selected_tag
          local ct = objects[index]

          if self.has_backup then
            self.bg = self.backup_bg
            self.fg = self.backup_fg
          end

          if t == ct then
            self.bg = beautiful.taglist_bg_focus
          end
        end)
      end,
    },
    layout = wibox.layout.fixed.vertical()
  }
end

return tag_list

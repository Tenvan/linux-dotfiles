log('Enter Module => ' .. ...)

local client = client

local awful = require('awful')
local wibox = require('wibox')
local beautiful = require('beautiful')
local helpers = require('helpers')

local dpi = require('beautiful').xresources.apply_dpi

local keys = require('configuration.keys.mod')
local modkey = keys.mod_key

local tag_list = function(pScreen)
  dump(pScreen.index, 'tag list screen index')

  return awful.widget.taglist {
    screen = pScreen,
    filter = awful.widget.taglist.filter.all,

    buttons = awful.util.table.join(
      awful.button({}, 1,
        function(t)
          t:view_only()
        end),
      awful.button({}, 3, awful.tag.viewtoggle),
      awful.button(
        { modkey }, 3, function(t)
        if _G.client.focus then
          _G.client.focus:toggle_tag(t)
        end
      end
      ),
      awful.button(
        {}, 4, function(t)
        awful.tag.viewprev(t.screen)
      end
      ),
      awful.button(
        {}, 5, function(t)
        awful.tag.viewnext(t.screen)
      end
      )
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
      widget = wibox.container.background,

      -- Add support for hover colors and an index label
      create_callback = function(self, c3, index, objects)
        self:get_children_by_id('index_role')[1].markup = "<span font_desc='" ..
          beautiful.taglist_font .. "'>" .. index .. '</span>'

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
          self.bg = beautiful.accent .. '99'
          self.fg = beautiful.bg_normal
        end)

        self:connect_signal('mouse::leave', function()
          -- BLING: Turn the widget off
          awesome.emit_signal('bling::tag_preview::visibility', pScreen, false, self)

          if self.has_backup then
            self.bg = self.backup_bg
            self.fg = self.backup_fg
          end
        end)
      end,
    },
    layout = wibox.layout.fixed.vertical()
  }
end

return tag_list

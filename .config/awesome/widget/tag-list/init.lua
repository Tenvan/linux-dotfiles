log("Enter Module => " .. ... )

local client = client

local awful = require('awful')
local wibox = require('wibox')
local beautiful = require('beautiful')
local gears = require('gears')
local helpers = require('helpers')

local dpi = require('beautiful').xresources.apply_dpi
local clickable_container = require('widget.clickable-container')

local keys = require('configuration.keys.mod')
local modkey = keys.mod_key
local altkey = keys.alt_key

--- Common method to create buttons.
-- @tab buttons
-- @param object
-- @return table
local function create_buttons(buttons, object)
  if buttons then
    local btns = {}
    for _, b in ipairs(buttons) do
      -- Create a proxy button object: it will receive the real
      -- press and release events, and will propagate them to the
      -- button object the user provided, but with the object as
      -- argument.
      local btn = awful.button {
        modifiers = b.modifiers,
        button = b.button,
        on_press = function()
          b:emit_signal('press', object)
        end,
        on_release = function()
          b:emit_signal('release', object)
        end,
      }
      btns[#btns + 1] = btn
    end
    return btns
  end
end

---@param w any The widget.
---@param buttons table
---@param label func Function to generate label parameters from an object. The function gets passed an object from objects, and has to return text, bg, bg_image, icon.
---@param data table Current data/cache, indexed by objects.
---@param objects table Objects to be displayed / updated.
local function list_update(w, buttons, label, data, objects)
  -- update the widgets, creating them if needed
  dump(buttons, 'tag_list.buttons(data)', 1)
  dump(data, 'tag_list.list_update(data)', 1)
  dump(objects, 'tag_list.list_update(objects)', 1)

  w:reset()

  for i, o in ipairs(objects) do

    local cache = data[o]
    local ib, tb, bgb, tbm, l, bg_clickable

    if cache then
      ib = cache.ib
      tb = cache.tb
      bgb = cache.bgb
      tbm = cache.tbm
    else
      ib = wibox.widget.imagebox()
      tb = wibox.widget.textbox()
      bgb = wibox.container.background()
      tbm = wibox.widget {
        tb,
        margins = dpi(1),
        widget = wibox.container.margin,
      }
      l = wibox.layout.fixed.horizontal()
      bg_clickable = clickable_container()

      -- All of this is added in a fixed widget
      l:fill_space(true)

      l:add(tbm)
      bg_clickable:set_widget(l)

      -- And all of this gets a background
      bgb:set_widget(bg_clickable)

      bgb:buttons(create_buttons(buttons, o))

      data[o] = { ib = ib, tb = tb, bgb = bgb, tbm = tbm }
    end

    local text, bg, bg_image, icon, args = label(o, tb)

    args = args or {}

    -- The text might be invalid, so use pcall.
    if text == nil or text == '' then
      tbm:set_margins(0)
    else
      if not tb:set_markup_silently("<span font_desc='" .. beautiful.taglist_font .. "'>" .. o.text .. '</span>') then
        tb:set_markup('<i>&lt;Invalid text&gt;</i>')
      end
    end

    bgb:set_bg(bg)
    if type(bg_image) == 'function' then
      -- TODO: Why does this pass nil as an argument?
      bg_image = bg_image(tb, o, nil, objects, i)
    end

    bgb:set_bgimage(bg_image)
    if icon then
      ib.image = icon
    end

    bgb.shape = args.shape
    bgb.shape_border_width = args.shape_border_width
    bgb.shape_border_color = args.shape_border_color

    w:add(bgb)
  end
end

local tag_box = helpers.vertical_pad(dpi(40))

local tag_list = function(args)
  return awful.widget.taglist {
    screen = args,
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

    update_function_old = list_update,

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
      create_callback = function(self, c3, index, objects) --luacheck: no unused args
        self:get_children_by_id('index_role')[1].markup = "<span font_desc='" ..
          beautiful.taglist_font .. "'>" .. index .. '</span>'

        self:connect_signal('mouse::enter', function()
          if self.bg ~= beautiful.accent then
            self.backup_bg  = self.bg
            self.backup_fg  = self.fg
            self.has_backup = true
          end
          self.bg = beautiful.accent .. '99'
          self.fg = beautiful.bg_normal
        end)

        self:connect_signal('mouse::leave', function()
          if self.has_backup then
            self.bg = self.backup_bg
            self.fg = self.backup_fg
          end
        end)
      end,

      update_callback = function(self, c3, index, objects) --luacheck: no unused args
        self:get_children_by_id('index_role')[1].markup = "<span font_desc='" ..
          beautiful.taglist_font .. "'>" .. index .. '</span>'
      end,
    },
    layout = wibox.layout.fixed.vertical()
  }
end

return tag_list

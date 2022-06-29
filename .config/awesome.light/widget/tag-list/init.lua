local awful = require('awful')
local wibox = require('wibox')
local dpi = require('beautiful').xresources.apply_dpi
local clickable_container = require('widget.clickable-container')
local icons = require('theme.icons')

local gdebug = require('gears.debug')

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

local function list_update(w, buttons, label, data, objects)
  -- gdebug.print_warning(gdebug.dump_return(objects, 'objects', 4))
  -- update the widgets, creating them if needed
  w:reset()
  for i, o in ipairs(objects) do
    -- gdebug.print_warning(gdebug.dump_return(o, 'object', 4))
    local cache = data[o]
    local ib, tb, bgb, tbm, ibm, l, bg_clickable
    if cache then
      ib = cache.ib
      tb = cache.tb
      bgb = cache.bgb
      tbm = cache.tbm
      ibm = cache.ibm
    else
      ib = wibox.widget.imagebox()
      tb = wibox.widget.textbox()
      bgb = wibox.container.background()
      tbm = wibox.widget {
        tb,
        -- margins = dpi(1),
        left = dpi(5),
        -- right = dpi(16),
        -- top = dpi(12),
        -- bottom = dpi(12),
        widget = wibox.container.margin,
      }
      ibm = wibox.widget {
        ib,
        margins = dpi(10),
        widget = wibox.container.margin,
      }
      l = wibox.layout.fixed.horizontal()
      bg_clickable = clickable_container()

      -- All of this is added in a fixed widget
      l:fill_space(true)

      -- l:add(ibm)
      l:add(tbm)
      bg_clickable:set_widget(l)

      -- And all of this gets a background
      bgb:set_widget(bg_clickable)

      bgb:buttons(create_buttons(buttons, o))

      data[o] = { ib = ib, tb = tb, bgb = bgb, tbm = tbm, ibm = ibm }
    end

    local text, bg, bg_image, icon, args = label(o, tb)

    args = args or {}

    -- The text might be invalid, so use pcall.
    if text == nil or text == '' then
      tbm:set_margins(0)
    else
      if not tb:set_markup_silently("<span font_desc='Noto Sans Symbol Bold 24'>" .. o.text .. '</span>') then
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
    else
      ibm:set_margins(0)
    end

    bgb.shape = args.shape
    bgb.shape_border_width = args.shape_border_width
    bgb.shape_border_color = args.shape_border_color

    w:add(bgb)
  end
end

local tag_list = function(args)
  -- gdebug.print_warning(gdebug.dump_return(args, 'args', 3))
  return awful.widget.taglist(
    args,
    awful.widget.taglist.filter.all,
    awful.util.table.join(
      awful.button({}, 1,
        function(t)
          t:view_only()
        end),
      awful.button(
        { modkey }, 1,
        function(t)
          if _G.client.focus then
            _G.client.focus:move_to_tag(t)
            t:view_only()
          end
        end
      ),
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
    {},
    list_update,
    wibox.layout.fixed.vertical()
  )
end
return tag_list
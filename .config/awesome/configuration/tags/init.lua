log('Enter Module => ' .. ...)

local screen, tag = screen, tag

local awful = require('awful')
local beautiful = require('beautiful')
local icons = require('theme.icons')
local apps = require('configuration.apps')
local lain = require('lain')

-- local tagnames = {"󾠮", "󾠯", "󾠰", "󾠱", "󾠲", "󾠳", "󾠴", "󾠵", "󾠶"}
-- local tagnames = {"1", "2", "3", "4", "5", "6", "7", "8", "9"}
-- local tagnames = { "", "", "", "", "", "", "", "", "" }
-- local tagnames = {  "", "", "", "", "", "", "", "", "", "" }
-- local tagnames = { "⠐", "⠡", "⠲", "⠵", "⠻", "⠿" }
-- local tagnames = { 'www', 'edit', 'gimp', 'inkscape', 'music', '⑥', '⑦', '⑧', '⑨' }
-- local tagnames = { "➊", "➋", "➌", "➍", "➎", "➏", "➐", "➑", "➒" }
local tagnames = { '①', '②', '③', '④', '⑤', '⑥', '⑦', '⑧', '⑨' }

local defaultLayouts = {
  awful.layout.suit.tile,
  awful.layout.suit.floating,
  awful.layout.suit.tile.left,
  awful.layout.suit.tile.bottom,
  awful.layout.suit.tile.top,
  awful.layout.suit.fair,
  awful.layout.suit.fair.horizontal,
  -- awful.layout.suit.spiral,
  -- awful.layout.suit.spiral.dwindle,
  awful.layout.suit.max,
  awful.layout.suit.max.fullscreen,
  awful.layout.suit.magnifier,
  -- awful.layout.suit.corner.nw,
  -- awful.layout.suit.corner.ne,
  -- awful.layout.suit.corner.sw,
  -- awful.layout.suit.corner.se,
  -- lain.layout.cascade,
  -- lain.layout.cascade.tile,
  lain.layout.centerwork,
  lain.layout.centerwork.horizontal,
  -- lain.layout.termfair,
  -- lain.layout.termfair.center,
}

local tags = {
  {
    gap = beautiful.useless_gap,
    layouts = {
      awful.layout.suit.tile,
      awful.layout.suit.max,
      awful.layout.suit.max.fullscreen,
      awful.layout.suit.magnifier,
      lain.layout.centerwork,
      lain.layout.centerwork.horizontal,
    },
  },
  {
    layout = lain.layout.centerwork,
  },
  {
  },
  {
    layout = lain.layout.centerwork,
  },
  {
  },
  {
  },
  {
  },
  {
  },
  {
  },
}

-- Set tags layout
tag.connect_signal('request::default_layouts', function()
  -- awful.layout.append_default_layouts({awful.layout.suit.spiral.dwindle, awful.layout.suit.tile,
  --                                      awful.layout.suit.floating, awful.layout.suit.max})
  awful.layout.append_default_layouts(defaultLayouts)
end)

-- Create tags for each screen
screen.connect_signal('request::desktop_decoration', function(s)
  for i, tag in pairs(tags) do
    awful.tag.add(i, {
      text = tagnames[i],
      icon = tag.icon,
      icon_only = false,
      layout = tag.layout or awful.layout.suit.tile,
      layouts = tag.layouts or defaultLayouts,
      gap_single_client = true,
      gap = tag.gap or beautiful.useless_gap,
      screen = s,
      selected = i == 1
    })
  end
end)

local update_gap_and_shape = function(t)
  -- Get current tag layout
  local current_layout = awful.tag.getproperty(t, 'layout')
  -- If the current layout is awful.layout.suit.max
  if (current_layout == awful.layout.suit.max) then
    -- Set clients gap to 0 and shape to rectangle if maximized
    t.gap = 0
    for _, c in ipairs(t:clients()) do
      if not c.floating or not c.round_corners or c.maximized or c.fullscreen then
        c.shape = beautiful.client_shape_rectangle
      else
        c.shape = beautiful.client_shape_rounded
      end
    end
  else
    t.gap = beautiful.useless_gap
    for _, c in ipairs(t:clients()) do
      if not c.round_corners or c.maximized or c.fullscreen then
        c.shape = beautiful.client_shape_rectangle
      else
        c.shape = beautiful.client_shape_rounded
      end
    end
  end
end

-- Change tag's client's shape and gap on change
tag.connect_signal('property::layout', function(t)
  update_gap_and_shape(t)
end)

-- Change tag's client's shape and gap on move to tag
tag.connect_signal('tagged', function(t)
  update_gap_and_shape(t)
end)

-- Focus on urgent clients
awful.tag.attached_connect_signal(s, 'property::selected', function()
  local urgent_clients = function(c)
    return awful.rules.match(c, {
      urgent = true
    })
  end
  for c in awful.client.iterate(urgent_clients) do
    if c.first_tag == mouse.screen.selected_tag then
      c:emit_signal('request::activate')
      c:raise()
    end
  end
end)

return {
  tags = tags,
  tagnames = tagnames
}

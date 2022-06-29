local log = require('utilities.debug').log
log("Enter Module => configuration/tags/init.lua" )

local screen, tag, client = screen, tag, client

local awful = require('awful')
local gears = require('gears')
local dpi = require('beautiful.xresources').apply_dpi

local beautiful = require('beautiful')
local apps = require('configuration.apps')

local mod_key = require("configuration.keys.mod").mod_key

local my_table = awful.util.table or gears.table

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
  awful.layout.suit.fair.horizontal, -- awful.layout.suit.spiral,
  -- awful.layout.suit.spiral.dwindle,
  awful.layout.suit.max,
  awful.layout.suit.max.fullscreen,
  awful.layout.suit.magnifier -- awful.layout.suit.corner.nw,
  -- awful.layout.suit.corner.ne,
  -- awful.layout.suit.corner.sw,
  -- awful.layout.suit.corner.se
  -- ~ lain.layout.cascade,
  -- ~ lain.layout.cascade.tile,
  -- ~ lain.layout.centerwork,
  -- ~ lain.layout.centerwork.horizontal,
  -- ~ lain.layout.termfair,
  -- ~ lain.layout.termfair.center,
}

local tags = { {
  layout = awful.layout.suit.max,
  layouts = { awful.layout.suit.tile, awful.layout.suit.max, awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier },
  master_fill_policy = 'expand',
  gap_single_client = false,
  gap = 5,
  selected = true
}, {
  layout = awful.layout.suit.tile,
  layouts = defaultLayouts,
  master_fill_policy = 'expand',
  gap_single_client = true,
  gap = 2
}, {
  layout = awful.layout.suit.tile,
  layouts = defaultLayouts,
  master_fill_policy = 'expand',
  gap_single_client = false,
  gap = 5
}, {
  layout = awful.layout.suit.tile,
  layouts = defaultLayouts,
  master_fill_policy = 'expand',
  gap_single_client = false,
  gap = 5
}, {
  layout = awful.layout.suit.tile,
  layouts = defaultLayouts,
  master_fill_policy = 'expand',
  gap_single_client = false,
  gap = 5
}, {
  layout = awful.layout.suit.tile,
  layouts = defaultLayouts,
  master_fill_policy = 'expand',
  gap_single_client = false,
  gap = 5
}, {
  layout = awful.layout.suit.tile,
  layouts = defaultLayouts,
  master_fill_policy = 'expand',
  gap_single_client = false,
  gap = 5
}, {
  layout = awful.layout.suit.tile,
  layouts = defaultLayouts,
  master_fill_policy = 'expand',
  gap_single_client = false,
  gap = 5
}, {
  layout = awful.layout.suit.tile,
  layouts = defaultLayouts,
  master_fill_policy = 'expand',
  gap_single_client = false,
  gap = 5
} }

for s in screen do
  for i = 1, 9 do
    local props = tags[i]
    props.screen = s
    awful.tag.add(tagnames[i], props)
  end
end

awful.layout.suit.tile.left.mirror = true

awful.layout.append_default_layouts = defaultLayouts
awful.layout.layouts = defaultLayouts

awful.util.taglist_buttons = my_table.join(awful.button({}, 1, function(t)
  t:view_only()
end),
  awful.button({ mod_key }, 1, function(t)
    if client.focus then
      client.focus:move_to_tag(t)
    end
  end),
  awful.button({}, 3, awful.tag.viewtoggle), awful.button({ mod_key }, 3, function(t)
    if client.focus then
      client.focus:toggle_tag(t)
    end
  end),
  awful.button({}, 4, function(t)
    awful.tag.viewnext(t.screen)
  end),
  awful.button({}, 5, function(t)
    awful.tag.viewprev(t.screen)
  end))

awful.util.tasklist_buttons = my_table.join(awful.button({}, 1, function(c)
  if c == client.focus then
    c.minimized = true
  else
    -- c:emit_signal("request::activate", "tasklist", {raise = true})<Paste>

    -- Without this, the following
    -- :isvisible() makes no sense
    c.minimized = false
    if not c:isvisible() and c.first_tag then
      c.first_tag:view_only()
    end
    -- This will also un-minimize
    -- the client, if needed
    client.focus = c
    c:raise()
  end
end), awful.button({}, 3, function()
  local instance = nil

  return function()
    if instance and instance.wibox.visible then
      instance:hide()
      instance = nil
    else
      instance = awful.menu.clients({
        theme = {
          width = dpi(250)
        }
      })
    end
  end
end), awful.button({}, 4, function()
  awful.client.focus.byidx(1)
end), awful.button({}, 5, function()
  awful.client.focus.byidx(-1)
end))

return {
  tags = tags,
  tagnames = tagnames
}

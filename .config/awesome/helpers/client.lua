log('Enter Module => ' .. ...)

local awful = require('awful')
local xresources = require('beautiful.xresources')
local dpi = xresources.apply_dpi

-- Resize DWIM (Do What I Mean)
-- Resize client or factor
-- Constants --
local floating_resize_amount = dpi(20)
local tiling_resize_factor = 0.05

---------------

function resize_client(c, direction)
  if c and c.floating or awful.layout.get(mouse.screen) == awful.layout.suit.floating then
    if direction == 'up' then
      c:relative_move(0, 0, 0, -floating_resize_amount)
    elseif direction == 'down' then
      c:relative_move(0, 0, 0, floating_resize_amount)
    elseif direction == 'left' then
      c:relative_move(0, 0, -floating_resize_amount, 0)
    elseif direction == 'right' then
      c:relative_move(0, 0, floating_resize_amount, 0)
    end
  elseif awful.layout.get(mouse.screen) ~= awful.layout.suit.floating then
    if direction == 'up' then
      awful.client.incwfact(-tiling_resize_factor)
    elseif direction == 'down' then
      awful.client.incwfact(tiling_resize_factor)
    elseif direction == 'left' then
      awful.tag.incmwfact(-tiling_resize_factor)
    elseif direction == 'right' then
      awful.tag.incmwfact(tiling_resize_factor)
    end
  end
end

function move_to_edge(c, direction)
  -- local workarea = awful.screen.focused().workarea
  -- local client_geometry = c:geometry()
  if direction == 'up' then
    local old_x = c:geometry().x
    awful.placement.top(c, {
      honor_padding = true,
      honor_workarea = true,
      honor_padding = true,
    })
    c.x = old_x
    -- c:geometry({ nil, y = workarea.y + beautiful.screen_margin * 2, nil, nil })
  elseif direction == 'down' then
    local old_x = c:geometry().x
    awful.placement.bottom(c, {
      honor_padding = true,
      honor_workarea = true,
      honor_padding = true,
    })
    c.x = old_x
    -- c:geometry({ nil, y = workarea.height + workarea.y - client_geometry.height - beautiful.screen_margin * 2 - beautiful.border_width * 2, nil, nil })
  elseif direction == 'left' then
    local old_y = c:geometry().y
    awful.placement.left(c, {
      honor_padding = true,
      honor_workarea = true,
      honor_padding = true,
    })
    c.y = old_y
    -- c:geometry({ x = workarea.x + beautiful.screen_margin * 2, nil, nil, nil })
  elseif direction == 'right' then
    local old_y = c:geometry().y
    awful.placement.right(c, {
      honor_padding = true,
      honor_workarea = true,
      honor_padding = true,
    })
    c.y = old_y
    -- c:geometry({ x = workarea.width + workarea.x - client_geometry.width - beautiful.screen_margin * 2 - beautiful.border_width * 2, nil, nil, nil })
  end
end

-- Move client DWIM (Do What I Mean)
-- Move to edge if the client / layout is floating
-- Swap by index if maximized
-- Else swap client by direction
function move_client(c, direction)
  dump(c, "Client", 3)
  log('Client Class: ' .. c.class)
  log('Client Instance: ' .. c.instance)
  log('Client Name: ' .. c.name)

  if (c.instance == 'QuakeTerminal') then
    log("ignore QuakeTerminal")
    return
  end
  
  log('Move Client: ' .. c.name .. ' (' .. direction .. ')')

  if c.floating or (awful.layout.get(mouse.screen) == awful.layout.suit.floating) then
    move_to_edge(c, direction)
  elseif awful.layout.get(mouse.screen) == awful.layout.suit.max then
    if direction == 'up' or direction == 'left' then
      awful.client.swap.byidx(-1, c)
    elseif direction == 'down' or direction == 'right' then
      awful.client.swap.byidx(1, c)
    end
  else
    awful.client.swap.bydirection(direction, c, nil)
  end
end

return {
  resize_client = resize_client,
  move_client = move_client,
  move_to_edge = move_to_edge
}

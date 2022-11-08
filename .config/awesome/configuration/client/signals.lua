log('Enter Module => ' .. ...)

local awesome, client = awesome, client

local awful = require('awful')
local beautiful = require('beautiful')
local config = require('configuration.config')

local notify = require('utilities.notify')
local sound = require('utilities.sound')

local update_client = function(c)
  -- Set client's shape based on its tag's layout and status (floating, maximized, etc.)
  local current_layout = awful.tag.getproperty(c.first_tag, 'layout')
  if current_layout == awful.layout.suit.max and (not c.floating) then
    c.shape = beautiful.client_shape_rectangle
  elseif c.maximized or c.fullscreen then
    c.shape = beautiful.client_shape_rectangle
  elseif (not c.round_corners) then
    c.shape = beautiful.client_shape_rectangle
  else
    c.shape = beautiful.client_shape_rounded
  end
end

-- Signal function to execute when a new client appears.
client.connect_signal(
  'manage',
  function(c)
    log('spawn::manage')
    -- Focus, raise and activate
    c:emit_signal(
    -- 'request::activate',
      'mouse_enter',
      {
        raise = true
      }
    )

    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    if not awesome.startup then
      awful.client.setslave(c)
    end

    if awesome.startup and not c.size_hints.user_position and
      not c.size_hints.program_position then
      -- Prevent clients from being unreachable after screen count changes.
      awful.placement.no_offscreen(c)
    end

    -- Update client shape
    update_client(c)
  end
)

-- Enable sloppy focus, so that focus follows mouse then raises it.
client.connect_signal(
  'mouse::enter',
  function(c)
    -- log('spawn::mouse::enter')
    c:emit_signal(
      'mouse_enter',
      {
        raise = true
      }
    )
  end
)

client.connect_signal(
  'focus',
  function(c)
    log('spawn::focus')
    c.border_color = beautiful.border_focus
  end
)

client.connect_signal(
  'unfocus',
  function(c)
    log('spawn::unfocus')
    c.border_color = beautiful.border_normal
  end
)

-- Manipulate client shape on fullscreen/non-fullscreen
client.connect_signal(
  'property::fullscreen',
  function(c)
    log('spawn::property::fullscreen')
    if c.fullscreen then
      c.shape = beautiful.client_shape_rectangle
    else
      update_client(c)
    end
  end
)

-- Manipulate client shape on maximized
client.connect_signal(
  'property::maximized',
  function(c)
    log('spawn::property::maximized')
    local current_layout = awful.tag.getproperty(c.first_tag, 'layout')
    if c.maximized then
      c.shape = beautiful.client_shape_rectangle
    else
      update_client(c)
    end
  end
)

-- Manipulate client shape on floating
client.connect_signal(
  'property::floating',
  function(c)
    log('spawn::property::floating')
    local current_layout = awful.tag.getproperty(c.first_tag, 'layout')
    if c.floating and not c.maximized then
      c.shape = beautiful.client_shape_rounded
    else
      if current_layout == awful.layout.suit.max then
        c.shape = beautiful.client_shape_rectangle
      end
    end
  end
)

client.connect_signal('swapped', function(c, source, is_source)
  log('spawn::window-switch')
  sound('window-switch')
end)

client.connect_signal('raised', function(c)
  log('spawn::window-attention-active')

  dump(c, ' => client', 2)
  log(' -> client class: ' .. tostring(c.class))
  log(' -> client instance: ' .. tostring(c.instance))
  log(' -> client name: ' .. tostring(c.name))
  log(' -> client type: ' .. tostring(c.type))
  log(' -> client role: ' .. tostring(c.role))

  sound('window-attention-active')
end)

client.connect_signal('lowered', function(c)
  log('spawn::window-attention-inactive')
  sound('window-attention-inactive')
end)

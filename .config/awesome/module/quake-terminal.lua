local log = require('utilities.debug').log
local dump = require('utilities.debug').dump
log("Enter Module => module/quake-terminal.lua" )

local awesome, client, screen = awesome, client, screen

local awful = require('awful')
local ruled = require('ruled')
local beautiful = require('beautiful')
local app = require('configuration.apps').default.quake
local client_keys = require('configuration.client.keys')
local client_buttons = require('configuration.client.buttons')
local quake_id = nil
local quake_client = nil
local quake_opened = false

local quake_properties = function()
  return {
    skip_decoration = false,
    titlebars_enabled = true,
    switch_to_tags = false,
    opacity = 0.95,
    floating = true,
    skip_taskbar = true,
    ontop = true,
    above = true,
    sticky = true,
    hidden = not quake_opened,
    maximized_horizontal = true,
    skip_center = true,
    round_corners = false,
    keys = client_keys,
    buttons = client_buttons,
    placement = awful.placement.top,
    shape = beautiful.client_shape_rectangle
  }
end

ruled.client.connect_signal(
  'request::rules',
  function()
    ruled.client.append_rule {
      id         = 'quake_terminal',
      rule_any   = {
        instance = {
          'QuakeTerminal'
        }
      },
      properties = quake_properties()
    }
  end
)

local create_quake = function()
  log('quake_terminal::create_quake')

  -- Check if there's already an instance of 'QuakeTerminal'.
  -- If yes, recover it - use it again.
  local quake_term = function(c)
    return ruled.client.match(c, { instance = 'QuakeTerminal' })
  end

  for c in awful.client.iterate(quake_term) do
    log('quake_terminal::detected')
    -- 'QuakeTerminal' instance detected
    -- Re-apply its properties
    ruled.client.execute(c, quake_properties())
    quake_id = c.pid
    c:emit_signal('request::activate')
    return
  end
  -- No 'QuakeTerminal' instance, spawn one
  quake_id = awful.spawn(app, quake_properties())
end

local quake_open = function()
  quake_client.hidden = false
  quake_client:emit_signal('request::activate')
end

local quake_close = function()
  quake_client.hidden = true
end

local quake_toggle = function()
  quake_opened = not quake_opened
  if not quake_client then
    create_quake()
  else
    if quake_opened then
      quake_open()
    else
      quake_close()
    end
  end
end

awesome.connect_signal(
  'module::quake_terminal:toggle',
  function()
    log('module::quake_terminal:toggle')
    quake_toggle();
  end
)

client.connect_signal(
  'manage',
  function(c)
    -- log('quake_terminal::manage')
    -- dump(c, 'quake_terminal::manage::c', 2)
    if c.pid == quake_id then
      -- log('quake_terminal::manage::quake')
      -- dump(c, 'quake_terminal::manage::quake', 2)
      quake_client = c
    end
  end
)

client.connect_signal(
  'unmanage',
  function(c)
    -- log('quake_terminal::unmanage')
    -- dump(c, 'quake_terminal::unmanage::c', 2)
    if c.pid == quake_id then
      -- log('quake_terminal::unmanage::quake')
      quake_opened = false
      quake_client = nil
    end
  end
)

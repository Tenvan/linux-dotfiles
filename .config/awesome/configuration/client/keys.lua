log('Enter Module => ' .. ...)

local awful = require('awful')


require('awful.autofocus')

local keys = require('configuration.keys.mod')

local dpi = require('beautiful').xresources.apply_dpi

local move_client = require('helpers.client').move_client
local move_focus = require('helpers.client').move_focus

local modkey = keys.mod_key
local altkey = keys.alt_key

-- modkey or mod4 = super key
local controlkey = keys.control_key
local downkey = keys.down_key
local escapekey = keys.escape_key
local leftkey = keys.left_key
local returnkey = keys.return_key
local rightkey = keys.right_key
local shiftkey = keys.shift_key
local spacekey = keys.space_key
local tabkey = keys.tab_key
local upkey = keys.up_key

-- key groups
local kgClient = keys.kgClient

local client_keys = awful.util.table.join(

-- ░█▄█░█▀█░█░█░█▀▀░░░█▀▀░█░░░▀█▀░█▀▀░█▀█░▀█▀
-- ░█░█░█░█░▀▄▀░█▀▀░░░█░░░█░░░░█░░█▀▀░█░█░░█░
-- ░▀░▀░▀▀▀░░▀░░▀▀▀░░░▀▀▀░▀▀▀░▀▀▀░▀▀▀░▀░▀░░▀░
-- Move client by direction
  awful.key({ shiftkey, modkey }, leftkey, function(c)
    move_client(c, 'left')
  end, {
    description = 'move client left',
    group = keys.kgClient
  }),
  awful.key({ shiftkey, modkey }, rightkey, function(c)
    move_client(c, 'right')
  end, {
    description = 'move client right',
    group = keys.kgClient
  }),

  awful.key({ shiftkey, modkey }, upkey, function(c)
    move_client(c, 'up')
  end, {
    description = 'move client up',
    group = keys.kgClient
  }),
  awful.key({ shiftkey, modkey }, downkey, function(c)
    move_client(c, 'down')
  end, {
    description = 'move client down',
    group = keys.kgClient
  }),
  awful.key({ shiftkey, modkey }, returnkey, function(c)
    local si = c.screen.index
    local sin = 1 + (2 - si)
    log('client from ' .. tostring(si) .. ' to next screen ' .. tostring(sin))

    c.screen = sin
    c:emit_signal('request::activate')
    c:raise()
  end, {
    description = 'move client to next screen',
    group = keys.kgClient
  }),

  -- ░█▄█░█▀█░█░█░█▀▀░░░█▀▀░█▀█░█▀▀░█░█░█▀▀
  -- ░█░█░█░█░▀▄▀░█▀▀░░░█▀▀░█░█░█░░░█░█░▀▀█
  -- ░▀░▀░▀▀▀░░▀░░▀▀▀░░░▀░░░▀▀▀░▀▀▀░▀▀▀░▀▀▀
  -- Focus client by direction
  -- global (over screens)

  awful.key({ modkey }, downkey, function(c)
    move_focus(c, 'down')
  end, {
    description = 'focus down',
    group = keys.kgClient
  }),
  awful.key({ modkey }, upkey, function(c)
    move_focus(c, 'up')
  end, {
    description = 'focus up',
    group = keys.kgClient
  }),
  awful.key({ modkey }, leftkey, function(c)
    move_focus(c, 'left')
  end, {
    description = 'focus left',
    group = keys.kgClient
  }),
  awful.key({ modkey }, rightkey, function(c)
    move_focus(c, 'right')
  end, {
    description = 'focus right',
    group = keys.kgClient
  }),

  awful.key({ modkey }, 'f', function(c)
    c.fullscreen = not c.fullscreen
    c:raise()
  end, {
    description = 'toggle fullscreen',
    group = kgClient
  }),

  awful.key({ modkey }, 'q', function(c)
    c:kill()
  end, {
    description = 'close',
    group = kgClient
  }),

  awful.key({ modkey }, 'n', function(c)
    c.minimized = true
  end, {
    description = 'minimize client',
    group = kgClient
  }),

  awful.key({ modkey, shiftkey }, 'c', function(c)
    local focused = awful.screen.focused()

    awful.placement.centered(c, {
      honor_workarea = true
    })
  end, {
    description = 'align a client to the center of the focused screen',
    group = kgClient
  }),

  awful.key({ modkey, altkey }, spacekey, function(c)
    c.fullscreen = false
    c.maximized = false
    c.floating = not c.floating
    c:raise()
  end, {
    description = 'toggle floating',
    group = kgClient
  }),
  awful.key({ modkey }, upkey, function(c)
    c:relative_move(0, dpi(-10), 0, 0)
  end, {
    description = 'move floating client up by 10 px',
    group = kgClient
  }),

  awful.key({ modkey, shiftkey }, downkey, function(c)
    c:relative_move(0, 0, 0, dpi(10))
  end, {
    description = 'increase floating client size vertically by 10 px down',
    group = kgClient
  }),

  awful.key({ modkey, shiftkey }, leftkey, function(c)
    c:relative_move(dpi(-10), 0, dpi(10), 0)
  end, {
    description = 'increase floating client size horizontally by 10 px left',
    group = kgClient
  }),

  awful.key({ modkey, shiftkey }, rightkey, function(c)
    c:relative_move(0, 0, dpi(10), 0)
  end, {
    description = 'increase floating client size horizontally by 10 px right',
    group = kgClient
  }),

  awful.key({ modkey, controlkey }, upkey, function(c)
    if c.height > 10 then
      c:relative_move(0, 0, 0, dpi(-10))
    end
  end, {
    description = 'decrease floating client size vertically by 10 px up',
    group = kgClient
  }),

  awful.key({ modkey, controlkey }, downkey, function(c)
    local c_height = c.height
    c:relative_move(0, 0, 0, dpi(-10))
    if c.height ~= c_height and c.height > 10 then
      c:relative_move(0, dpi(10), 0, 0)
    end
  end, {
    description = 'decrease floating client size vertically by 10 px down',
    group = kgClient
  }),
  awful.key({ modkey, controlkey }, leftkey, function(c)
    if c.width > 10 then
      c:relative_move(0, 0, dpi(-10), 0)
    end
  end, {
    description = 'decrease floating client size horizontally by 10 px left',
    group = kgClient
  }),
  awful.key({ modkey, controlkey }, rightkey, function(c)
    local c_width = c.width
    c:relative_move(0, 0, dpi(-10), 0)
    if c.width ~= c_width and c.width > 10 then
      c:relative_move(dpi(10), 0, 0, 0)
    end
  end, {
    description = 'decrease floating client size horizontally by 10 px right',
    group = kgClient
  }))

function GetCustomKeys(client)
  local customClientKeys = os.getenv('HOME') .. '/.config/awesome/customs/awesome/keys-client.lua'
  local file = io.open(customClientKeys, 'r') -- r read mode
  if not file then
    log("custom client keys '" .. customClientKeys .. "' NOT found")
    return
  end

  log("custom client keys '" .. customClientKeys .. "' found")

  local keys = require('customs.awesome.keys-client')

  return keys
end

dump(client_keys, 'client keys before merge', 1)

local custom_keys = GetCustomKeys()
dump(custom_keys, 'custom keys', 1)

client_keys =  awful.util.table.join(client_keys, custom_keys)
dump(client_keys, 'client keys after merge', 1)

return client_keys

log('Enter Module => ' .. ...)

local awesome, client, screen = awesome, client, screen

local awful         = require('awful')
local hotkeys_popup = require('awful.hotkeys_popup').widget

local apps    = require('configuration.apps')
local keys    = require('configuration.keys.mod')
local specs   = require('layout.specs')
local helpers = require('helpers')

local switcher = require('module.window-switcher')
local resize_client = require('helpers.client').resize_client
local move_client = require('helpers.client').move_client
local move_focus = require('helpers.client').move_focus

local modkey = keys.mod_key
local altkey = keys.alt_key

-- ░█░█░█▀▀░█░█░░░█▄█░█▀█░█▀▄░▀█▀░█▀▀░▀█▀░█▀▀░█▀▄░█▀▀░█▀▀
-- ░█▀▄░█▀▀░░█░░░░█░█░█░█░█░█░░█░░█▀▀░░█░░█▀▀░█▀▄░█▀▀░▀▀█
-- ░▀░▀░▀▀▀░░▀░░░░▀░▀░▀▀▀░▀▀░░▀▀▀░▀░░░▀▀▀░▀▀▀░▀░▀░▀▀▀░▀▀▀
local controlkey = keys.control_key
local shiftkey   = keys.shift_key
local returnkey  = keys.return_key
local spacekey   = keys.space_key
local escapekey  = keys.escape_key
local printkey   = keys.print_key
local tabkey     = keys.tab_key
local downkey    = keys.down_key
local upkey      = keys.up_key
local leftkey    = keys.left_key
local rightkey   = keys.right_key

local menuCoords = {
  coords = { x = specs.leftPanel.actionBarWidth * 2, y = specs.topPanel.height * 2 },
}

-- ░█░█░█▀▀░█░█░░░█▀▄░▀█▀░█▀█░█▀▄░▀█▀░█▀█░█▀▀░█▀▀
-- ░█▀▄░█▀▀░░█░░░░█▀▄░░█░░█░█░█░█░░█░░█░█░█░█░▀▀█
-- ░▀░▀░▀▀▀░░▀░░░░▀▀░░▀▀▀░▀░▀░▀▀░░▀▀▀░▀░▀░▀▀▀░▀▀▀
local global_keys = awful.util.table.join(

-- ░█▀█░█░█░█▀▀░█▀▀░█▀█░█▄█░█▀▀
-- ░█▀█░█▄█░█▀▀░▀▀█░█░█░█░█░█▀▀
-- ░▀░▀░▀░▀░▀▀▀░▀▀▀░▀▀▀░▀░▀░▀▀▀
  awful.key({ modkey }, 'F1', hotkeys_popup.show_help, {
    description = 'show help',
    group = keys.kgAwesome
  }),
  awful.key({ modkey, controlkey }, 'r', awesome.restart, {
    description = 'reload awesome',
    group = keys.kgAwesome
  }),
  awful.key({ modkey, controlkey }, 'q', awesome.quit, {
    description = 'quit awesome',
    group = keys.kgAwesome
  }),

  -- ░█▄█░█▀█░█▀▀░▀█▀░█▀▀░█▀▄
  -- ░█░█░█▀█░▀▀█░░█░░█▀▀░█▀▄
  -- ░▀░▀░▀░▀░▀▀▀░░▀░░▀▀▀░▀░▀
  awful.key({ altkey, shiftkey }, 'l', function()
    awful.tag.incmwfact(0.05)
  end, {
    description = 'increase master width factor',
    group = keys.kgLayout
  }),
  awful.key({ altkey, shiftkey }, 'h', function()
    awful.tag.incmwfact(-0.05)
  end, {
    description = 'decrease master width factor',
    group = keys.kgLayout
  }),
  awful.key({ modkey, shiftkey }, 'h', function()
    awful.tag.incnmaster(1, nil, true)
  end, {
    description = 'increase the number of master clients',
    group = keys.kgLayout
  }),
  awful.key({ modkey, shiftkey }, 'l', function()
    awful.tag.incnmaster(-1, nil, true)
  end, {
    description = 'decrease the number of master clients',
    group = keys.kgLayout
  }),
  awful.key({ modkey, controlkey }, 'h', function()
    awful.tag.incncol(1, nil, true)
  end, {
    description = 'increase the number of columns',
    group = keys.kgLayout
  }),
  awful.key({ modkey, controlkey }, 'l', function()
    awful.tag.incncol(-1, nil, true)
  end, {
    description = 'decrease the number of columns',
    group = keys.kgLayout
  }),

  -- awful.key({ modkey, shiftkey }, leftkey, function()
  --   awful.tag.incmwfact(-0.05)
  -- end, {
  --   description = 'increase master size',
  --   group = keys.kgClient
  -- }),
  -- awful.key({ modkey, shiftkey }, rightkey, function()
  --   awful.tag.incmwfact(0.05)
  -- end, {
  --   description = 'decrease master size',
  --   group = keys.kgClient
  -- }),

  -- ░█░░░█▀█░█░█░█▀█░█░█░▀█▀
  -- ░█░░░█▀█░░█░░█░█░█░█░░█░
  -- ░▀▀▀░▀░▀░░▀░░▀▀▀░▀▀▀░░▀░
  awful.key({ modkey, controlkey }, spacekey, function()
    awful.layout.inc(1)
    log('=> Layout: [' .. awful.layout.get(awful.screen.focused()).name .. ']')
  end, {
    description = 'select next layout',
    group = keys.kgLayout
  }),
  awful.key({ modkey, shiftkey }, spacekey, function()
    awful.layout.inc(-1)
    log('=> Layout: [' .. awful.layout.get(awful.screen.focused()).name .. ']')
  end, {
    description = 'select previous layout',
    group = keys.kgLayout
  }),
  awful.key({ modkey }, 'o', function()
    awful.tag.incgap(1)
  end, {
    description = 'increase gap',
    group = keys.kgLayout
  }),
  awful.key({ modkey, shiftkey }, 'o', function()
    awful.tag.incgap(-1)
  end, {
    description = 'decrease gap',
    group = keys.kgLayout
  }),

  -- ░█▀▄░█▀▀░█▀▀░▀█▀░▀▀█░█▀▀░░░█░█░▀█▀░█▀█░█▀▄░█▀█░█░█
  -- ░█▀▄░█▀▀░▀▀█░░█░░▄▀░░█▀▀░░░█▄█░░█░░█░█░█░█░█░█░█▄█
  -- ░▀░▀░▀▀▀░▀▀▀░▀▀▀░▀▀▀░▀▀▀░░░▀░▀░▀▀▀░▀░▀░▀▀░░▀▀▀░▀░▀
  awful.key({ modkey, shiftkey }, 'Next', function()
    awful.client.moveresize(20, 20, -40, -40)
  end, {
    description = 'increase window size',
    group = keys.kgClient
  }),
  awful.key({ modkey, shiftkey }, 'Prior', function()
    awful.client.moveresize(-20, -20, 40, 40)
  end, {
    description = 'decrease window size',
    group = keys.kgClient
  }),
  -- awful.key({ modkey, shiftkey }, downkey, function()
  --   awful.client.incwfact(0.05)
  -- end, {
  --   description = 'increase window size',
  --   group = keys.kgClient
  -- }),
  -- awful.key({ modkey, shiftkey }, upkey, function()
  --   awful.client.incwfact(-0.05)
  -- end, {
  --   description = 'decrease window size',
  --   group = keys.kgClient
  -- }),

  awful.key({ modkey, controlkey }, 'n', function()
    local c = awful.client.restore()
    -- Focus restored client
    if c then
      c:emit_signal('request::activate')
      c:raise()
    end
  end, {
    description = 'restore minimized',
    group = keys.kgClient
  }),


  -- ░█▄█░█▀█░█░█░█▀▀░░░█▀▀░█▀█░█▀▀░█░█░█▀▀
  -- ░█░█░█░█░▀▄▀░█▀▀░░░█▀▀░█░█░█░░░█░█░▀▀█
  -- ░▀░▀░▀▀▀░░▀░░▀▀▀░░░▀░░░▀▀▀░▀▀▀░▀▀▀░▀▀▀
  -- local clients (on current screen)
  awful.key({ modkey, altkey }, rightkey, function()
    awful.client.focus.byidx(1)
  end, {
    description = 'focus next by index',
    group = keys.kgClient
  }),
  awful.key({ modkey, altkey }, leftkey, function()
    awful.client.focus.byidx(-1)
  end, {
    description = 'focus previous by index',
    group = keys.kgClient
  }),

  -- screens
  awful.key({ modkey }, '.', function()
    awful.screen.focus_relative(1)
  end, {
    description = 'focus the next screen',
    group = keys.kgScreen
  }),
  awful.key({ modkey }, ',', function()
    awful.screen.focus_relative(-1)
  end, {
    description = 'focus the previous screen',
    group = keys.kgScreen
  }),

  awful.key({ modkey, shiftkey }, 'F1', function()
    awful.screen.focus_relative(-1)
  end, {
    description = 'focus the previous screen',
    group = keys.kgScreen
  }),
  awful.key({ modkey, shiftkey }, 'F2', function()
    awful.screen.focus_relative(1)
  end, {
    description = 'focus the next screen',
    group = keys.kgScreen
  }),

  -- ░█▀▄░█▀▀░█▀▀░▀█▀░▀▀█░█▀▀
  -- ░█▀▄░█▀▀░▀▀█░░█░░▄▀░░█▀▀
  -- ░▀░▀░▀▀▀░▀▀▀░▀▀▀░▀▀▀░▀▀▀
  -- -- Resize focused client
  awful.key({ modkey, controlkey }, upkey, function(c)
    resize_client(awful.client.focus, 'up')
  end,
    { description = 'resize to the up',
      group = keys.kgClient }),
  awful.key({ modkey, controlkey }, downkey, function(c)
    resize_client(awful.client.focus, 'down')
  end,
    { description = 'resize to the down',
      group = keys.kgClient }),
  awful.key({ modkey, controlkey }, leftkey, function(c)
    resize_client(awful.client.focus, 'left')
  end,
    { description = 'resize to the left',
      group = keys.kgClient }),
  awful.key({ modkey, controlkey }, rightkey, function(c)
    resize_client(awful.client.focus, 'right')
  end,
    { description = 'resize to the right',
      group = keys.kgClient }),


  -- ░█▀█░█▀█░█░█░▀█▀░█▀▀░█▀█░▀█▀░▀█▀░█▀█░█▀█
  -- ░█░█░█▀█░▀▄▀░░█░░█░█░█▀█░░█░░░█░░█░█░█░█
  -- ░▀░▀░▀░▀░░▀░░▀▀▀░▀▀▀░▀░▀░░▀░░▀▀▀░▀▀▀░▀░▀
  awful.key({ modkey }, 'u', awful.client.urgent.jumpto, {
    description = 'jump to urgent client',
    group = keys.kgNavigation
  }),

  awful.key({ modkey, controlkey }, 'w', function()
    -- tag_view_nonempty(-1)
    local focused = awful.screen.focused()
    for i = 1, #focused.tags do
      awful.tag.viewidx(-1, focused)
      if #focused.clients > 0 then
        return
      end
    end
  end, {
    description = 'view previous non-empty tag',
    group = keys.kgTag
  }),

  awful.key({ modkey, controlkey }, 's', function()
    -- tag_view_nonempty(1)
    local focused = awful.screen.focused()
    for i = 1, #focused.tags do
      awful.tag.viewidx(1, focused)
      if #focused.clients > 0 then
        return
      end
    end
  end, {
    description = 'view next non-empty tag',
    group = keys.kgTag
  }),

  awful.key({ modkey }, tabkey, function()
    switcher(0, 'Super_L', tabkey, tabkey)
  end, {
    description = 'view window switcher',
    group = keys.kgClient
  }),

  awful.key({ controlkey, modkey }, tabkey, function()
    switcher(1, 'Super_L', tabkey, tabkey)
  end, {
    description = 'switch next task and view window switcher',
    group = keys.kgClient
  }),

  awful.key({ controlkey, shiftkey, modkey }, tabkey, function()
    switcher(1, 'Super_L', tabkey, tabkey)
  end, {
    description = 'switch previous task and view window switcher',
    group = keys.kgClient
  }),

  awful.key({ modkey }, 'u',
    awful.client.urgent.jumpto, {
    description = 'jump to urgent client',
    group = keys.kgClient
  }),

  -- ░█▀▄░█▀▄░▀█▀░█▀▀░█░█░▀█▀░█▀█░█▀▀░█▀▀░█▀▀
  -- ░█▀▄░█▀▄░░█░░█░█░█▀█░░█░░█░█░█▀▀░▀▀█░▀▀█
  -- ░▀▀░░▀░▀░▀▀▀░▀▀▀░▀░▀░░▀░░▀░▀░▀▀▀░▀▀▀░▀▀▀
  awful.key({}, 'XF86MonBrightnessUp', function()
    awful.spawn('light -A 10', false)
    emit('widget::brightness')
    emit('module::brightness_osd:show', true)
  end, {
    description = 'increase brightness by 10%',
    group = keys.kgHotkeys
  }),
  awful.key({}, 'XF86MonBrightnessDown', function()
    awful.spawn('light -U 10', false)
    emit('widget::brightness')
    emit('module::brightness_osd:show', true)
  end, {
    description = 'decrease brightness by 10%',
    group = keys.kgHotkeys
  }),

  --
  -- ░█▀█░█░░░█▀▀░█▀█░░░█▀▄░█▀▀░█░█░▀█▀░█▀▀░█▀▀░░░█▀▀░█▀█░█▀█░▀█▀░█▀▄░█▀█░█░░
  -- ░█▀█░█░░░▀▀█░█▀█░░░█░█░█▀▀░▀▄▀░░█░░█░░░█▀▀░░░█░░░█░█░█░█░░█░░█▀▄░█░█░█░░
  -- ░▀░▀░▀▀▀░▀▀▀░▀░▀░░░▀▀░░▀▀▀░░▀░░▀▀▀░▀▀▀░▀▀▀░░░▀▀▀░▀▀▀░▀░▀░░▀░░▀░▀░▀▀▀░▀▀▀
  awful.key({ modkey, controlkey }, 'KP_Add', function()
    awful.spawn.with_shell('~/.bin/audio-next')
  end, {
    description = 'Nächste Soundkarte',
    group = keys.kgSound
  }),
  awful.key({ modkey, controlkey }, 'KP_Subtract', function()
    awful.spawn.with_shell('~/.bin/audio-prev')
  end, {
    description = 'Vorherige Soundkarte',
    group = keys.kgSound
  }),

  --
  -- ░█▀█░█░░░█▀▀░█▀█░░░█░█░█▀█░█░░░█░█░█▄█░█▀▀░░░█▀▀░█▀█░█▀█░▀█▀░█▀▄░█▀█░█░░
  -- ░█▀█░█░░░▀▀█░█▀█░░░▀▄▀░█░█░█░░░█░█░█░█░█▀▀░░░█░░░█░█░█░█░░█░░█▀▄░█░█░█░░
  -- ░▀░▀░▀▀▀░▀▀▀░▀░▀░░░░▀░░▀▀▀░▀▀▀░▀▀▀░▀░▀░▀▀▀░░░▀▀▀░▀▀▀░▀░▀░░▀░░▀░▀░▀▀▀░▀▀▀
  awful.key({}, 'XF86AudioRaiseVolume', function()
    awful.spawn('amixer -D pulse sset Master 5%+', false)
    emit('widget::volume')
    emit('module::volume_osd:show', true)
  end, {
    description = 'increase volume up by 5%',
    group = keys.kgHotkeys
  }),
  awful.key({ modkey, altkey }, 'KP_Add', function()
    awful.spawn('amixer -D pulse sset Master 5%+', false)
    emit('widget::volume')
    emit('module::volume_osd:show', true)
  end, {
    description = '+5% Volume',
    group = keys.kgSound
  }),
  awful.key({}, 'XF86AudioLowerVolume', function()
    awful.spawn('amixer -D pulse sset Master 5%-', false)
    emit('widget::volume')
    emit('module::volume_osd:show', true)
  end, {
    description = '-5% Volume',
    group = keys.kgHotkeys
  }),
  awful.key({ modkey, altkey }, 'KP_Subtract', function()
    awful.spawn('amixer -D pulse sset Master 5%-', false)
    emit('widget::volume')
    emit('module::volume_osd:show', true)
  end, {
    description = '-5% Volume',
    group = keys.kgSound
  }),
  awful.key({}, 'XF86AudioMute', function()
    awful.spawn('amixer -D pulse set Master 1+ toggle', false)
    emit('widget::volume')
    emit('module::volume_osd:show', true)
  end, {
    description = 'toggle mute',
    group = keys.kgHotkeys
  }),
  awful.key({ modkey, altkey }, 'KP_Multiply', function()
    awful.spawn('amixer -D pulse set Master 1+ toggle', false)
    emit('widget::volume')
    emit('module::volume_osd:show', true)
  end, {
    description = 'Mute Volume',
    group = keys.kgSound
  }),
  awful.key({}, 'XF86AudioMicMute', function()
    awful.spawn('amixer set Capture toggle', false)
  end, {
    description = 'mute microphone',
    group = keys.kgHotkeys
  }),

  --
  -- ░█▀█░█░░░█▀█░█░█░█▀▀░█▀▄░░░█▀▀░█▀█░█▀█░▀█▀░█▀▄░█▀█░█░░
  -- ░█▀▀░█░░░█▀█░░█░░█▀▀░█▀▄░░░█░░░█░█░█░█░░█░░█▀▄░█░█░█░░
  -- ░▀░░░▀▀▀░▀░▀░░▀░░▀▀▀░▀░▀░░░▀▀▀░▀▀▀░▀░▀░░▀░░▀░▀░▀▀▀░▀▀▀
  awful.key({}, 'XF86AudioNext', function()
    awful.spawn('playerctl -a next', false)
  end, {
    description = 'next music',
    group = keys.kgHotkeys
  }),
  awful.key({}, 'XF86AudioPrev', function()
    awful.spawn('playerctl -a previous', false)
  end, {
    description = 'previous music',
    group = keys.kgHotkeys
  }),
  awful.key({}, 'XF86AudioPlay', function()
    awful.spawn('playerctl -a play-pause', false)
  end, {
    description = 'Player Start/Pause',
    group = keys.kgHotkeys
  }),
  awful.key({}, 'XF86AudioStop', function()
    notify('playerctl -a stop')
    awful.spawn('playerctl -a stop', false)
  end, {
    description = 'Player Stop',
    group = keys.kgHotkeys
  }),

  -- ░█▄█░█▀▀░█▀▄░▀█▀░█▀█░░░▀█▀░█▀█░█▀▀░▀█▀░█▀▀░█▀█
  -- ░█░█░█▀▀░█░█░░█░░█▀█░░░░█░░█▀█░▀▀█░░█░░█▀▀░█░█
  -- ░▀░▀░▀▀▀░▀▀░░▀▀▀░▀░▀░░░░▀░░▀░▀░▀▀▀░░▀░░▀▀▀░▀░▀
  awful.key({}, 'XF86Calculator', function()
    awful.spawn('gnome-calculator')
  end), -- Menu Shortcuts
  awful.key({}, 'XF86PowerDown', function()
    --
  end, {
    description = 'shutdown skynet',
    group = keys.kgHotkeys
  }),
  awful.key({}, 'XF86PowerOff', function()
    emit('module::exit_screen:show')
  end, {
    description = 'toggle exit screen',
    group = keys.kgHotkeys
  }),
  awful.key({}, 'XF86Display', function()
    awful.spawn.single_instance('arandr', false)
  end, {
    description = 'arandr',
    group = keys.kgHotkeys
  }),
  awful.key({ shiftkey, modkey }, 'q', function()
    emit('module::exit_screen:show')
  end, {
    description = 'toggle exit screen',
    group = keys.kgHotkeys
  }),

  awful.key({ altkey, modkey }, returnkey, function()
    emit('module::quake_terminal:toggle')
  end, {
    description = 'dropdown terminal (all screens)',
    group = keys.kgLauncher
  }),

  awful.key({ controlkey, modkey }, returnkey, function()
    local QuakeTerminal = require('module.scratchpad').quake
    QuakeTerminal:toggle()
  end, {
    description = 'dropdown terminal (primary)',
    group = keys.kgLauncher
  }),

  awful.key({ modkey, shiftkey }, 'm', function()
    if awful.screen.focused().musicpop then
      emit('widget::music', 'keyboard')
    end
  end, {
    description = 'toggle music widget',
    group = keys.kgLauncher
  }),
  awful.key({}, 'Print', function()
    awful.spawn.easy_async_with_shell(apps.utils.full_screenshot, function()
    end)
  end, {
    description = 'fullscreen screenshot',
    group = keys.kgUtils
  }),
  awful.key({ modkey, shiftkey }, 's', function()
    awful.spawn.easy_async_with_shell(apps.utils.area_screenshot, function()
    end)
  end, {
    description = 'area/selected screenshot',
    group = keys.kgUtils
  }),
  awful.key({ modkey }, ']', function()
    emit('widget::blur:increase')
  end, {
    description = 'increase blur effect by 10%',
    group = keys.kgUtils
  }),
  awful.key({ modkey }, '[', function()
    emit('widget::blur:decrease')
  end, {
    description = 'decrease blur effect by 10%',
    group = keys.kgUtils
  }),
  awful.key({ modkey }, 'b', function()
    emit('widget::blue_light:toggle')
  end, {
    description = 'toggle redshift filter',
    group = keys.kgUtils
  }),

  -- ░█▀▀░█░█░█▀▀░▀█▀░█▀▀░█▄█░░░█▄█░█▀▀░█▀█░█░█░█▀▀
  -- ░▀▀█░░█░░▀▀█░░█░░█▀▀░█░█░░░█░█░█▀▀░█░█░█░█░▀▀█
  -- ░▀▀▀░░▀░░▀▀▀░░▀░░▀▀▀░▀░▀░░░▀░▀░▀▀▀░▀░▀░▀▀▀░▀▀▀

  awful.key({ modkey }, 'a', function()
    local menu = require('configuration.menu').APP_MENU()
    awful.menu(menu):show(menuCoords)
  end, {
    description = 'Applikations Menü',
    group = keys.kgMenus
  }),
  awful.key({ modkey }, 'd', function()
    local menu = require('configuration.menu').DEVELOP_MENU()
    awful.menu(menu):show(menuCoords)
  end, {
    description = 'Developer Menü',
    group = keys.kgMenus
  }),
  awful.key({ modkey }, 'e', function()
    local menu = require('configuration.menu').EDIT_CONFIG()
    awful.menu(menu):show(menuCoords)
  end, {
    description = 'System Edit Menü',
    group = keys.kgMenus
  }),
  awful.key({ modkey }, 't', function()
    local menu = require('configuration.menu').SYSTEM_TOOLS_MENU()
    awful.menu(menu):show(menuCoords)
  end, {
    description = 'System Tools Menü',
    group = keys.kgMenus
  }),
  awful.key({ modkey }, 'm', function()
    local menu = require('configuration.menu').SYSTEM_MONITOR()
    awful.menu(menu):show(menuCoords)
  end, {
    description = 'System Monitors Menü',
    group = keys.kgMenus
  }),
  awful.key({ modkey }, 'x', function()
    local menu = require('configuration.menu').SYSTEM_MENU()
    awful.menu(menu):show(menuCoords)
  end, {
    description = 'System Power Menü',
    group = keys.kgMenus
  }),

  -- ░█▀█░█▀█░█▀█░█░░░▀█▀░█▀▀░█▀█░▀█▀░▀█▀░█▀█░█▀█░█▀▀
  -- ░█▀█░█▀▀░█▀▀░█░░░░█░░█░░░█▀█░░█░░░█░░█░█░█░█░▀▀█
  -- ░▀░▀░▀░░░▀░░░▀▀▀░▀▀▀░▀▀▀░▀░▀░░▀░░▀▀▀░▀▀▀░▀░▀░▀▀▀
  awful.key({ modkey }, returnkey, function()
    awful.spawn(apps.default.terminal)
  end, {
    description = 'open default terminal',
    group = keys.kgLauncher
  }),
  awful.key({ modkey }, 'z', function()
    awful.spawn.with_shell('sh ~/.scripts/menu/rofi.sh -show combi')
  end, {
    description = 'Rofi Menü',
    group = keys.kgMenus
  }),
  awful.key({ modkey, shiftkey }, 'z', function()
    awful.spawn.with_shell(apps.default.app_launcher)
  end, {
    description = 'open app launcher',
    group = keys.kgApps
  }),
  awful.key({ modkey, shiftkey }, 'e', function()
    awful.spawn(apps.default.file_manager)
  end, {
    description = 'open default file manager',
    group = keys.kgLauncher
  }),
  awful.key({ modkey, shiftkey }, 'f', function()
    awful.spawn(apps.default.web_browser)
  end, {
    description = 'open default web browser',
    group = keys.kgLauncher
  }),
  awful.key({ modkey, shiftkey }, 'w', function()
    awful.spawn.with_shell("$(xdg-settings get default-web-browser | cut -f1 -d '.')")
  end, {
    description = 'Standard Browser',
    group = keys.kgApps
  }),

  --
  -- ░█▀█░█▀▄░▀█▀░█▀█░▀█▀░█▀▀░█▀▄░░░█▀▀░█░█░█▀█░█▀▄░▀█▀░█▀▀░█░█░▀█▀░█▀▀
  -- ░█▀▀░█▀▄░░█░░█░█░░█░░█▀▀░█▀▄░░░▀▀█░█▀█░█░█░█▀▄░░█░░█░░░█░█░░█░░▀▀█
  -- ░▀░░░▀░▀░▀▀▀░▀░▀░░▀░░▀▀▀░▀░▀░░░▀▀▀░▀░▀░▀▀▀░▀░▀░░▀░░▀▀▀░▀▀▀░░▀░░▀▀▀
  awful.key({}, 'Print', function()
    awful.spawn('spectacle -i')
  end, {
    description = 'Screenshot App',
    group = keys.kgScreenshot
  }),
  awful.key({ altkey }, 'Print', function()
    awful.spawn('spectacle -i -r')
  end, {
    description = 'Screenshot Rect',
    group = keys.kgScreenshot
  }),
  awful.key({ controlkey }, 'Print', function()
    awful.spawn('spectacle -i -a')
  end, {
    description = 'Screenshot Fenster',
    group = keys.kgScreenshot
  }),

  --
  -- ░█▀▀░█░█░█▀▀░▀█▀░█▀▀░█▄█░░░▀█▀░█▀█░█▀█░█░░░█▀▀
  -- ░▀▀█░░█░░▀▀█░░█░░█▀▀░█░█░░░░█░░█░█░█░█░█░░░▀▀█
  -- ░▀▀▀░░▀░░▀▀▀░░▀░░▀▀▀░▀░▀░░░░▀░░▀▀▀░▀▀▀░▀▀▀░▀▀▀
  awful.key({ modkey, altkey }, 't', function()
    local message = '\nDies ist eine Test Nachicht.\n' ..
      'Amet dolor amet elitr sea justo eirmod ipsum sit.\n' ..
      'Sit sed eos dolore vero vero ea, ea magna at et.'

    notify('Teams Test Nachricht',
      '<b>CRITICAL</b>\nDies ist eine Teams Test Nachicht.' .. message, 'critical', 'audio-card', 'teams-for-linux')

    notify('Spotify Test Nachricht',
      '<b>CRITICAL</b>\nDies ist eine Spotify Test Nachicht.' .. message, 'critical', 'audio-card', 'Spotify')

    notify('Vivaldi Test Nachricht',
      '<b>CRITICAL</b>\nDies ist eine Vivaldi Test Nachicht.' .. message, 'critical', 'audio-card', 'vivaldi-stable')

    notify('KDE Connect Test Nachricht',
      '<b>CRITICAL</b>\nDies ist eine KDE Connect Test Nachicht.' .. message, 'critical', 'audio-card', 'KDE Connect')

    notify('Test Nachricht 1',
      '<b>LOW</b>\nDies ist eine Test Nachicht.\n' .. message,
      'low', '/usr/share/icons/hicolor/scalable/status/tablet.svg')

    notify('Test Nachricht 2',
      '<b>NORMAL</b>\nDies ist eine Test Nachicht.\n' .. message, 'normal', 'audio-card')

    notify('Test Nachricht 3',
      '<b>CRITICAL</b>\nDies ist eine Test Nachicht.\n' .. message,
      'critical', '/usr/share/icons/hicolor/scalable/status/tablet.svg')
    notify('Test Nachricht 4',
      '<b>LOW</b>\nDies ist eine Test Nachicht.\n' .. message)
  end, {
    description = 'Test Benachrichtigung',
    group = keys.kgSystem
  }),
  awful.key({ modkey, controlkey }, 'x', function()
    awful.spawn.with_shell('kitty --hold --title CF:XProp --name CF:XProp xprop')
  end, {
    description = 'Xprop',
    group = keys.kgSystem
  }),
  awful.key({ modkey, controlkey }, 't', function()
    awful.spawn.with_shell('sh ~/.scripts/picom-toggle-awesome.sh')
  end, {
    description = 'Picom Toggle',
    group = keys.kgSystem
  }),
  awful.key({ modkey }, escapekey, function()
    awful.spawn('xkill')
  end, {
    description = 'XKill',
    group = keys.kgSystem
  }),

  awful.key({ modkey }, 'F3', function()
    local focused = awful.screen.focused()

    if focused.left_panel and focused.left_panel.opened then
      focused.left_panel:toggle()
    end

    if focused.right_panel then
      if _G.right_panel_mode == 'today_mode' or not focused.right_panel.visible then
        focused.right_panel:toggle()
        switch_rdb_pane('today_mode')
      else
        switch_rdb_pane('today_mode')
      end

      _G.right_panel_mode = 'today_mode'
    end
  end, {
    description = 'open today pane',
    group = keys.kgLauncher
  }),

  awful.key({ modkey }, 'F4', function()
    local focused = awful.screen.focused()

    if focused.left_panel and focused.left_panel.opened then
      focused.left_panel:toggle()
    end

    if focused.right_panel then
      if _G.right_panel_mode == 'notif_mode' or not focused.right_panel.visible then
        focused.right_panel:toggle()
        switch_rdb_pane('notif_mode')
      else
        switch_rdb_pane('notif_mode')
      end

      _G.right_panel_mode = 'notif_mode'
    end
  end, {
    description = 'open notification center',
    group = keys.kgLauncher
  })
)

function GetCustomKeys()
  local customGlobalKeys = os.getenv('HOME') .. '/.config/awesome/customs/awesome/keys-global.lua'
  local file = io.open(customGlobalKeys, 'r') -- r read mode
  if not file then
    log("custom global keys '" .. customGlobalKeys .. "' NOT found")
    return
  end

  log("custom global keys '" .. customGlobalKeys .. "' found")

  local keys = require('customs.awesome.keys-global')

  return keys
end

local custom_keys = GetCustomKeys()
global_keys = awful.util.table.join(global_keys, custom_keys)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
  -- Hack to only show tags 1 and 9 in the shortcut window (mod+s)
  local descr_view, descr_toggle, descr_move, descr_toggle_focus
  if i == 1 or i == 9 then
    descr_view = {
      description = 'view tag #',
      group = keys.kgTag
    }
    descr_toggle = {
      description = 'toggle tag #',
      group = keys.kgTag
    }
    descr_move = {
      description = 'move focused client to tag #',
      group = keys.kgTag
    }
    descr_toggle_focus = {
      description = 'toggle focused client on tag #',
      group = keys.kgTag
    }
  end

  global_keys = awful.util.table.join(global_keys, -- View tag only.
    awful.key({ modkey }, '#' .. i + 9, function()
      local focused = awful.screen.focused()
      local tag = focused.tags[i]
      if tag then
        tag:view_only()
      end
    end, descr_view), -- Toggle tag display.

    awful.key({ modkey, controlkey }, '#' .. i + 9, function()
      local focused = awful.screen.focused()
      local tag = focused.tags[i]
      if tag then
        awful.tag.viewtoggle(tag)
      end
    end, descr_toggle), -- Move client to tag.

    awful.key({ modkey, shiftkey }, '#' .. i + 9, function()
      if client.focus then
        local tag = client.focus.screen.tags[i]
        if tag then
          client.focus:move_to_tag(tag)
        end
      end
    end, descr_move), -- Toggle tag on focused client.

    awful.key({ modkey, controlkey, shiftkey }, '#' .. i + 9, function()
      if client.focus then
        local tag = client.focus.screen.tags[i]
        if tag then
          client.focus:toggle_tag(tag)
        end
      end
    end, descr_toggle_focus))
end

return global_keys

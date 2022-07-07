log("Enter Module => " .. ... )

local awesome, client, screen = awesome, client, screen

local awful         = require('awful')
local hotkeys_popup = require('awful.hotkeys_popup').widget

local apps  = require('configuration.apps')
local keys  = require('configuration.keys.mod')
local specs = require('layout.specs')

local modkey = keys.mod_key
local altkey = keys.alt_key

-- modkey or mod4 = super key
local controlkey = keys.control_key
local shiftkey   = keys.shift_key
local returnkey  = keys.return_key
local spacekey   = keys.space_key
local escapekey  = keys.escape_key
local tabkey     = keys.tab_key
local downkey    = keys.down_key
local upkey      = keys.up_key
local leftkey    = keys.left_key
local rightkey   = keys.right_key

-- ░█░█░█▀▀░█░█░░░█▀▀░█▀▄░█▀█░█░█░█▀█░█▀▀
-- ░█▀▄░█▀▀░░█░░░░█░█░█▀▄░█░█░█░█░█▀▀░▀▀█
-- ░▀░▀░▀▀▀░░▀░░░░▀▀▀░▀░▀░▀▀▀░▀▀▀░▀░░░▀▀▀
local kgAwesome    = 'AwesomeWM'
local kgApps       = 'Anwendungen'
local kgUtils      = 'Tools'
local kgMenus      = 'Menüs'
local kgClient     = 'Client Aktionen'
local kgLayout     = 'Layout Aktionen'
local kgMaster     = 'Master Aktionen'
local kgScreen     = 'Screen Aktionen'
local kgScreenshot = 'Screenshot'
local kgSound      = 'Audio'
local kgSystem     = 'System'
local kgTag        = 'Tags'
local kgHotkeys    = 'Hotkeys'
local kgLauncher   = 'Starter'

local menuCoords = {
  coords = { x = specs.leftPanel.actionBarWidth * 2, y = specs.topPanel.height * 2 },
  -- placement = awful.placement.bottom_right
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
    group = 'awesome'
  }),
  awful.key({ modkey, controlkey }, 'r', awesome.restart, {
    description = 'reload awesome',
    group = kgAwesome
  }),
  awful.key({ modkey, controlkey }, 'q', awesome.quit, {
    description = 'quit awesome',
    group = kgAwesome
  }),

  -- ░█▄█░█▀█░█▀▀░▀█▀░█▀▀░█▀▄
  -- ░█░█░█▀█░▀▀█░░█░░█▀▀░█▀▄
  -- ░▀░▀░▀░▀░▀▀▀░░▀░░▀▀▀░▀░▀
  awful.key({ altkey, shiftkey }, 'l', function()
    awful.tag.incmwfact(0.05)
  end, {
    description = 'increase master width factor',
    group = kgLayout
  }),
  awful.key({ altkey, shiftkey }, 'h', function()
    awful.tag.incmwfact(-0.05)
  end, {
    description = 'decrease master width factor',
    group = kgLayout
  }),
  awful.key({ modkey, shiftkey }, 'h', function()
    awful.tag.incnmaster(1, nil, true)
  end, {
    description = 'increase the number of master clients',
    group = kgLayout
  }),
  awful.key({ modkey, shiftkey }, 'l', function()
    awful.tag.incnmaster(-1, nil, true)
  end, {
    description = 'decrease the number of master clients',
    group = kgLayout
  }),
  awful.key({ modkey, controlkey }, 'h', function()
    awful.tag.incncol(1, nil, true)
  end, {
    description = 'increase the number of columns',
    group = kgLayout
  }),
  awful.key({ modkey, controlkey }, 'l', function()
    awful.tag.incncol(-1, nil, true)
  end, {
    description = 'decrease the number of columns',
    group = kgLayout
  }),
  awful.key({ modkey, shiftkey }, 'Left', function()
    awful.tag.incmwfact(-0.05)
  end, {
    description = 'increase master size',
    group = kgClient
  }),
  awful.key({ modkey, shiftkey }, 'Right', function()
    awful.tag.incmwfact(0.05)
  end, {
    description = 'decrease master size',
    group = kgClient
  }),

  -- ░█░░░█▀█░█░█░█▀█░█░█░▀█▀
  -- ░█░░░█▀█░░█░░█░█░█░█░░█░
  -- ░▀▀▀░▀░▀░░▀░░▀▀▀░▀▀▀░░▀░
  awful.key({ modkey, controlkey }, spacekey, function()
    awful.layout.inc(1)
  end, {
    description = 'select next layout',
    group = kgLayout
  }),
  awful.key({ modkey, shiftkey }, spacekey, function()
    awful.layout.inc(-1)
  end, {
    description = 'select previous layout',
    group = kgLayout
  }),
  awful.key({ modkey }, 'o', function()
    awful.tag.incgap(1)
  end, {
    description = 'increase gap',
    group = kgLayout
  }),
  awful.key({ modkey, shiftkey }, 'o', function()
    awful.tag.incgap(-1)
  end, {
    description = 'decrease gap',
    group = kgLayout
  }),
  awful.key({ modkey, shiftkey }, tabkey, awful.tag.viewprev, {
    description = 'view previous tag',
    group = kgTag
  }),
  awful.key({ modkey }, tabkey, awful.tag.viewnext, {
    description = 'view next tag',
    group = kgTag
  }),
  awful.key({ modkey }, escapekey, awful.tag.history.restore, {
    description = 'alternate between current and previous tag',
    group = kgTag
  }),

  -- ░█░█░▀█▀░█▀█░█▀▄░█▀█░█░█░█▀▀
  -- ░█▄█░░█░░█░█░█░█░█░█░█▄█░▀▀█
  -- ░▀░▀░▀▀▀░▀░▀░▀▀░░▀▀▀░▀░▀░▀▀▀
  awful.key({ modkey, shiftkey }, 'Next', function()
    awful.client.moveresize(20, 20, -40, -40)
  end, {
    description = 'increase window size',
    group = kgClient
  }),
  awful.key({ modkey, shiftkey }, 'Prior', function()
    awful.client.moveresize(-20, -20, 40, 40)
  end, {
    description = 'decrease window size',
    group = kgClient
  }),
  awful.key({ modkey, shiftkey }, 'Down', function()
    awful.client.incwfact(0.05)
  end, {
    description = 'increase window size',
    group = kgClient
  }),
  awful.key({ modkey, shiftkey }, 'Up', function()
    awful.client.incwfact(-0.05)
  end, {
    description = 'decrease window size',
    group = kgClient
  }),
  awful.key({ modkey }, 'Right', function()
    awful.client.focus.byidx(1)
  end, {
    description = 'focus next by index',
    group = kgClient
  }),
  awful.key({ modkey }, 'Left', function()
    awful.client.focus.byidx(-1)
  end, {
    description = 'focus previous by index',
    group = kgClient
  }),
  awful.key({ controlkey, modkey }, downkey, function()
    awful.client.focus.global_bydirection('down')
    if awful.client.focus then
      awful.client.focus:raise()
    end
  end, {
    description = 'focus down',
    group = kgClient
  }),
  awful.key({ controlkey, modkey }, upkey, function()
    awful.client.focus.global_bydirection('up')
    if awful.client.focus then
      awful.client.focus:raise()
    end
  end, {
    description = 'focus up',
    group = kgClient
  }),
  awful.key({ controlkey, modkey }, leftkey, function()
    awful.client.focus.global_bydirection('left')
    if awful.client.focus then
      awful.client.focus:raise()
    end
  end, {
    description = 'focus left',
    group = kgClient
  }),
  awful.key({ controlkey, modkey }, rightkey, function()
    awful.client.focus.global_bydirection('right')
    if awful.client.focus then
      awful.client.focus:raise()
    end
  end, {
    description = 'focus right',
    group = kgClient
  }),
  awful.key({ modkey }, downkey, function()
    awful.client.swap.byidx(1)
  end, {
    description = 'swap with next client by index',
    group = kgClient
  }),
  awful.key({ modkey }, upkey, function()
    awful.client.swap.byidx(-1)
  end, {
    description = 'swap with previous client by index',
    group = kgClient
  }),
  awful.key({ modkey }, '.', function()
    awful.screen.focus_relative(1)
  end, {
    description = 'focus the next screen',
    group = kgScreen
  }),
  awful.key({ modkey }, ',', function()
    awful.screen.focus_relative(-1)
  end, {
    description = 'focus the previous screen',
    group = kgScreen
  }),
  awful.key({ modkey }, 'u', awful.client.urgent.jumpto, {
    description = 'jump to urgent client',
    group = kgClient
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
    group = kgTag
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
    group = kgTag
  }),
  awful.key({ modkey, shiftkey }, 'F1', function()
    awful.screen.focus_relative(-1)
  end, {
    description = 'focus the previous screen',
    group = kgScreen
  }),
  awful.key({ modkey, shiftkey }, 'F2', function()
    awful.screen.focus_relative(1)
  end, {
    description = 'focus the next screen',
    group = kgScreen
  }),
  awful.key({ modkey, controlkey }, 'n', function()
    local c = awful.client.restore()
    -- Focus restored client
    if c then
      c:emit_signal('request::activate')
      c:raise()
    end
  end, {
    description = 'restore minimized',
    group = kgScreen
  }),

  -- ░█▀▄░█▀▄░▀█▀░█▀▀░█░█░▀█▀░█▀█░█▀▀░█▀▀░█▀▀
  -- ░█▀▄░█▀▄░░█░░█░█░█▀█░░█░░█░█░█▀▀░▀▀█░▀▀█
  -- ░▀▀░░▀░▀░▀▀▀░▀▀▀░▀░▀░░▀░░▀░▀░▀▀▀░▀▀▀░▀▀▀
  awful.key({}, 'XF86MonBrightnessUp', function()
    awful.spawn('light -A 10', false)
    awesome.emit_signal('widget::brightness')
    awesome.emit_signal('module::brightness_osd:show', true)
  end, {
    description = 'increase brightness by 10%',
    group = kgHotkeys
  }),
  awful.key({}, 'XF86MonBrightnessDown', function()
    awful.spawn('light -U 10', false)
    awesome.emit_signal('widget::brightness')
    awesome.emit_signal('module::brightness_osd:show', true)
  end, {
    description = 'decrease brightness by 10%',
    group = kgHotkeys
  }),

  --
  -- ░█▀█░█░░░█▀▀░█▀█░░░█▀▄░█▀▀░█░█░▀█▀░█▀▀░█▀▀░░░█▀▀░█▀█░█▀█░▀█▀░█▀▄░█▀█░█░░
  -- ░█▀█░█░░░▀▀█░█▀█░░░█░█░█▀▀░▀▄▀░░█░░█░░░█▀▀░░░█░░░█░█░█░█░░█░░█▀▄░█░█░█░░
  -- ░▀░▀░▀▀▀░▀▀▀░▀░▀░░░▀▀░░▀▀▀░░▀░░▀▀▀░▀▀▀░▀▀▀░░░▀▀▀░▀▀▀░▀░▀░░▀░░▀░▀░▀▀▀░▀▀▀
  awful.key({ modkey, controlkey }, 'KP_Add', function()
    awful.spawn.with_shell('~/.bin/audio-next')
  end, {
    description = 'Nächste Soundkarte',
    group = kgSound
  }),
  awful.key({ modkey, controlkey }, 'KP_Subtract', function()
    awful.spawn.with_shell('~/.bin/audio-prev')
  end, {
    description = 'Vorherige Soundkarte',
    group = kgSound
  }),

  --
  -- ░█▀█░█░░░█▀▀░█▀█░░░█░█░█▀█░█░░░█░█░█▄█░█▀▀░░░█▀▀░█▀█░█▀█░▀█▀░█▀▄░█▀█░█░░
  -- ░█▀█░█░░░▀▀█░█▀█░░░▀▄▀░█░█░█░░░█░█░█░█░█▀▀░░░█░░░█░█░█░█░░█░░█▀▄░█░█░█░░
  -- ░▀░▀░▀▀▀░▀▀▀░▀░▀░░░░▀░░▀▀▀░▀▀▀░▀▀▀░▀░▀░▀▀▀░░░▀▀▀░▀▀▀░▀░▀░░▀░░▀░▀░▀▀▀░▀▀▀
  awful.key({}, 'XF86AudioRaiseVolume', function()
    awful.spawn('amixer -D pulse sset Master 5%+', false)
    awesome.emit_signal('widget::volume')
    awesome.emit_signal('module::volume_osd:show', true)
  end, {
    description = 'increase volume up by 5%',
    group = kgHotkeys
  }),
  awful.key({ modkey, altkey }, 'KP_Add', function()
    awful.spawn('amixer -D pulse sset Master 5%+', false)
    awesome.emit_signal('widget::volume')
    awesome.emit_signal('module::volume_osd:show', true)
  end, {
    description = '+5% Volume',
    group = kgSound
  }),
  awful.key({}, 'XF86AudioLowerVolume', function()
    awful.spawn('amixer -D pulse sset Master 5%-', false)
    awesome.emit_signal('widget::volume')
    awesome.emit_signal('module::volume_osd:show', true)
  end, {
    description = '-5% Volume',
    group = kgHotkeys
  }),
  awful.key({ modkey, altkey }, 'KP_Subtract', function()
    awful.spawn('amixer -D pulse sset Master 5%-', false)
    awesome.emit_signal('widget::volume')
    awesome.emit_signal('module::volume_osd:show', true)
  end, {
    description = '-5% Volume',
    group = kgSound
  }),
  awful.key({}, 'XF86AudioMute', function()
    awful.spawn('amixer -D pulse set Master 1+ toggle', false)
    awesome.emit_signal('widget::volume')
    awesome.emit_signal('module::volume_osd:show', true)
  end, {
    description = 'toggle mute',
    group = kgHotkeys
  }),
  awful.key({ modkey, altkey }, 'KP_Multiply', function()
    awful.spawn('amixer -D pulse set Master 1+ toggle', false)
    awesome.emit_signal('widget::volume')
    awesome.emit_signal('module::volume_osd:show', true)
  end, {
    description = 'Mute Volume',
    group = kgSound
  }),
  awful.key({}, 'XF86AudioMicMute', function()
    awful.spawn('amixer set Capture toggle', false)
  end, {
    description = 'mute microphone',
    group = kgHotkeys
  }),

  --
  -- ░█▀█░█░░░█▀█░█░█░█▀▀░█▀▄░░░█▀▀░█▀█░█▀█░▀█▀░█▀▄░█▀█░█░░
  -- ░█▀▀░█░░░█▀█░░█░░█▀▀░█▀▄░░░█░░░█░█░█░█░░█░░█▀▄░█░█░█░░
  -- ░▀░░░▀▀▀░▀░▀░░▀░░▀▀▀░▀░▀░░░▀▀▀░▀▀▀░▀░▀░░▀░░▀░▀░▀▀▀░▀▀▀
  awful.key({}, 'XF86AudioNext', function()
    awful.spawn('playerctl next', false)
  end, {
    description = 'next music',
    group = kgHotkeys
  }),
  awful.key({}, 'XF86AudioPrev', function()
    awful.spawn('playerctl previous', false)
  end, {
    description = 'previous music',
    group = kgHotkeys
  }),
  awful.key({}, 'XF86AudioPlay', function()
    awful.spawn('playerctl play-pause', false)
  end, {
    description = 'Player Start/Pause',
    group = kgHotkeys
  }),
  awful.key({}, 'XF86AudioStop', function()
    notify('playerctl stop')
    awful.spawn('playerctl stop', false)
  end, {
    description = 'Player Stop',
    group = kgHotkeys
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
    group = kgHotkeys
  }),
  awful.key({}, 'XF86PowerOff', function()
    awesome.emit_signal('module::exit_screen:show')
  end, {
    description = 'toggle exit screen',
    group = kgHotkeys
  }),
  awful.key({}, 'XF86Display', function()
    awful.spawn.single_instance('arandr', false)
  end, {
    description = 'arandr',
    group = kgHotkeys
  }),
  awful.key({ modkey, shiftkey }, 'q', function()
    awesome.emit_signal('module::exit_screen:show')
  end, {
    description = 'toggle exit screen',
    group = kgHotkeys
  }),
  awful.key({ modkey, controlkey }, returnkey, function()
    awesome.emit_signal('module::quake_terminal:toggle')
  end, {
    description = 'dropdown application',
    group = kgLauncher
  }),
  awful.key({ modkey, shiftkey }, 'm', function()
    if awful.screen.focused().musicpop then
      awesome.emit_signal('widget::music', 'keyboard')
    end
  end, {
    description = 'toggle music widget',
    group = kgLauncher
  }),
  awful.key({}, 'Print', function()
    awful.spawn.easy_async_with_shell(apps.utils.full_screenshot, function()
    end)
  end, {
    description = 'fullscreen screenshot',
    group = kgUtils
  }),
  awful.key({ modkey, shiftkey }, 's', function()
    awful.spawn.easy_async_with_shell(apps.utils.area_screenshot, function()
    end)
  end, {
    description = 'area/selected screenshot',
    group = kgUtils
  }),
  awful.key({ modkey }, 'x', function()
    awesome.emit_signal('widget::blur:toggle')
  end, {
    description = 'toggle blur effects',
    group = kgUtils
  }),
  awful.key({ modkey }, ']', function()
    awesome.emit_signal('widget::blur:increase')
  end, {
    description = 'increase blur effect by 10%',
    group = kgUtils
  }),
  awful.key({ modkey }, '[', function()
    awesome.emit_signal('widget::blur:decrease')
  end, {
    description = 'decrease blur effect by 10%',
    group = kgUtils
  }),
  awful.key({ modkey }, 'b', function()
    awesome.emit_signal('widget::blue_light:toggle')
  end, {
    description = 'toggle redshift filter',
    group = kgUtils
  }),
  awful.key({ controlkey }, 'Escape', function()
    if screen.primary.systray then
      if not screen.primary.tray_toggler then
        local systray = screen.primary.systray
        systray.visible = not systray.visible
      else
        awesome.emit_signal('widget::systray:toggle')
      end
    end
  end, {
    description = 'toggle systray visibility',
    group = kgUtils
  }),
  awful.key({ modkey }, 'l', function()
    awful.spawn(apps.default.lock, false)
  end, {
    description = 'lock the screen',
    group = kgUtils
  }),
  awful.key({ controlkey, shiftkey }, 'Escape', function()
    awful.spawn(apps.default.terminal .. ' ' .. 'htop')
  end, {
    description = 'open system monitor',
    group = kgLauncher
  }),
  awful.key({ modkey }, 'l', function()
    local focused = awful.screen.focused()

    if focused.right_panel and focused.right_panel.visible then
      focused.right_panel.visible = false
    end
    screen.primary.left_panel:toggle()
  end, {
    description = 'open sidebar',
    group = kgLauncher
  }),
  awful.key({ modkey }, 'r', function()
    local focused = awful.screen.focused()

    if focused.right_panel and focused.right_panel.visible then
      focused.right_panel.visible = false
    end
    screen.primary.left_panel:toggle(true)
  end, {
    description = 'open sidebar and global search',
    group = kgLauncher
  }),

  -- ░█▀▀░█░█░█▀▀░▀█▀░█▀▀░█▄█░░░█▄█░█▀▀░█▀█░█░█░█▀▀
  -- ░▀▀█░░█░░▀▀█░░█░░█▀▀░█░█░░░█░█░█▀▀░█░█░█░█░▀▀█
  -- ░▀▀▀░░▀░░▀▀▀░░▀░░▀▀▀░▀░▀░░░▀░▀░▀▀▀░▀░▀░▀▀▀░▀▀▀

  awful.key({ modkey }, 'a', function()
    local menu = require('configuration.menu').APP_MENU()
    awful.menu(menu):show(menuCoords)
  end, {
    description = 'Applikations Menü',
    group = kgMenus
  }),
  awful.key({ modkey }, 'd', function()
    local menu = require('configuration.menu').DEVELOP_MENU()
    awful.menu(menu):show(menuCoords)
  end, {
    description = 'Developer Menü',
    group = kgMenus
  }),
  awful.key({ modkey }, 'e', function()
    local menu = require('configuration.menu').EDIT_CONFIG()
    awful.menu(menu):show(menuCoords)
  end, {
    description = 'System Edit Menü',
    group = kgMenus
  }),
  awful.key({ modkey }, 't', function()
    local menu = require('configuration.menu').SYSTEM_TOOLS_MENU()
    awful.menu(menu):show(menuCoords)
  end, {
    description = 'System Tools Menü',
    group = kgMenus
  }),
  awful.key({ modkey }, 'm', function()
    local menu = require('configuration.menu').SYSTEM_MONITOR()
    awful.menu(menu):show(menuCoords)
  end, {
    description = 'System Monitors Menü',
    group = kgMenus
  }),
  awful.key({ modkey }, 'x', function()
    local menu = require('configuration.menu').SYSTEM_MENU()
    awful.menu(menu):show(menuCoords)
  end, {
    description = 'System Power Menü',
    group = kgMenus
  }),

  -- ░█▀█░█▀█░█▀█░█░░░▀█▀░█▀▀░█▀█░▀█▀░▀█▀░█▀█░█▀█░█▀▀
  -- ░█▀█░█▀▀░█▀▀░█░░░░█░░█░░░█▀█░░█░░░█░░█░█░█░█░▀▀█
  -- ░▀░▀░▀░░░▀░░░▀▀▀░▀▀▀░▀▀▀░▀░▀░░▀░░▀▀▀░▀▀▀░▀░▀░▀▀▀
  awful.key({ modkey }, returnkey, function()
    awful.spawn(apps.default.terminal)
  end, {
    description = 'open default terminal',
    group = kgLauncher
  }),
  awful.key({ modkey }, 'z', function()
    awful.spawn.with_shell('sh ~/.scripts/menu/rofi.sh -show combi')
  end, {
    description = 'Rofi Menü',
    group = kgMenus
  }),
  awful.key({ modkey, shiftkey }, 'z', function()
    awful.spawn.with_shell(apps.default.app_launcher)
  end, {
    description = 'open app launcher',
    group = kgApps
  }),
  awful.key({ modkey, shiftkey }, 'e', function()
    awful.spawn(apps.default.file_manager)
  end, {
    description = 'open default file manager',
    group = kgLauncher
  }),
  awful.key({ modkey, shiftkey }, 'f', function()
    awful.spawn(apps.default.web_browser)
  end, {
    description = 'open default web browser',
    group = kgLauncher
  }),
  awful.key({ modkey, shiftkey }, 'w', function()
    awful.spawn.with_shell("$(xdg-settings get default-web-browser | cut -f1 -d '.')")
  end, {
    description = 'Standard Browser',
    group = kgApps
  }),

  --
  -- ░█▀█░█▀▄░▀█▀░█▀█░▀█▀░█▀▀░█▀▄░░░█▀▀░█░█░█▀█░█▀▄░▀█▀░█▀▀░█░█░▀█▀░█▀▀
  -- ░█▀▀░█▀▄░░█░░█░█░░█░░█▀▀░█▀▄░░░▀▀█░█▀█░█░█░█▀▄░░█░░█░░░█░█░░█░░▀▀█
  -- ░▀░░░▀░▀░▀▀▀░▀░▀░░▀░░▀▀▀░▀░▀░░░▀▀▀░▀░▀░▀▀▀░▀░▀░░▀░░▀▀▀░▀▀▀░░▀░░▀▀▀
  awful.key({}, 'Print', function()
    awful.spawn('spectacle -i')
  end, {
    description = 'Screenshot App',
    group = kgScreenshot
  }),
  awful.key({ altkey }, 'Print', function()
    awful.spawn('spectacle -i -r')
  end, {
    description = 'Screenshot Rect',
    group = kgScreenshot
  }),
  awful.key({ controlkey }, 'Print', function()
    awful.spawn('spectacle -i -a')
  end, {
    description = 'Screenshot Fenster',
    group = kgScreenshot
  }),

  --
  -- ░█▀▀░█░█░█▀▀░▀█▀░█▀▀░█▄█░░░▀█▀░█▀█░█▀█░█░░░█▀▀
  -- ░▀▀█░░█░░▀▀█░░█░░█▀▀░█░█░░░░█░░█░█░█░█░█░░░▀▀█
  -- ░▀▀▀░░▀░░▀▀▀░░▀░░▀▀▀░▀░▀░░░░▀░░▀▀▀░▀▀▀░▀▀▀░▀▀▀
  awful.key({ modkey, altkey }, 't', function()
    notify('Test Nachricht 1',
      '<b>LOW</b>\nDies ist eine Test Nachicht.\nAmet dolor amet elitr sea justo eirmod ipsum sit.\nSit sed eos dolore vero vero ea, ea magna at et.'
      ,
      'low', true, '/usr/share/icons/hicolor/scalable/status/tablet.svg')
    notify('Test Nachricht 2',
      '<b>NORMAL</b>\nDies ist eine Test Nachicht.\nAmet dolor amet elitr sea justo eirmod ipsum sit.\nSit sed eos dolore vero vero ea, ea magna at et.'
      ,
      'normal', true, 'audio-card')
    notify('Test Nachricht 3',
      '<b>CRITICAL</b>\nDies ist eine Test Nachicht.\nAmet dolor amet elitr sea justo eirmod ipsum sit.\nSit sed eos dolore vero vero ea, ea magna at et.'
      ,
      'critical', true, '/usr/share/icons/hicolor/scalable/status/tablet.svg')
  end, {
    description = 'Test Benachrichtigung',
    group = kgSystem
  }),
  awful.key({ modkey, controlkey }, 'x', function()
    awful.spawn.with_shell('kitty --hold --title CF:XProp --name CF:XProp xprop')
  end, {
    description = 'Xprop',
    group = kgSystem
  }),
  awful.key({ modkey, controlkey }, 't', function()
    awful.spawn.with_shell('sh ~/.scripts/picom-toggle-awesome.sh')
  end, {
    description = 'Picom Toggle',
    group = kgSystem
  }),
  awful.key({ modkey, shiftkey }, 'Escape', function()
    awful.spawn('xkill')
  end, {
    description = 'XKill',
    group = kgSystem
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
    group = kgLauncher
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
    group = kgLauncher
  }))

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
  -- Hack to only show tags 1 and 9 in the shortcut window (mod+s)
  local descr_view, descr_toggle, descr_move, descr_toggle_focus
  if i == 1 or i == 9 then
    descr_view = {
      description = 'view tag #',
      group = kgTag
    }
    descr_toggle = {
      description = 'toggle tag #',
      group = kgTag
    }
    descr_move = {
      description = 'move focused client to tag #',
      group = kgTag
    }
    descr_toggle_focus = {
      description = 'toggle focused client on tag #',
      group = kgTag
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

log('Enter Module => configuration/menu.lua')

local awesome, client, screen = awesome, client, screen

local awful = require('awful')
local freedesktop = require('freedesktop')
local hotkeys_popup = require('awful.hotkeys_popup').widget
local apps = require('configuration.apps')
local beautiful = require('beautiful')
local gtable = require('gears.table')

local readJson = require('utilities.json').readJsonFile

local gears = require('gears')
local filesystem = gears.filesystem
local config_dir = filesystem.get_configuration_dir()

local awesomemenu = {
  {
    'Hotkeys',
    function()
      hotkeys_popup.show_help(nil, awful.screen.focused())
    end,
  },
  { 'Manual', apps.default.terminal .. ' -e man awesome' },
  { 'Edit Config', apps.default.text_editor .. ' ' .. awesome.conffile },
  { 'Restart', awesome.restart },
  {
    'Quit',
    function()
      awesome.quit()
    end,
  },
}

local mainmenu = freedesktop.menu.build({
  before = {
    { 'Awesome',
      awesomemenu,
      beautiful.awesome_icon }
    -- { "Atom", "atom" },
    -- other triads can be put here
  },
  after = {
    { 'Terminal', apps.default.terminal },
    { 'Log out', function()
      awesome.quit()
    end },
    { 'Sleep', 'systemctl suspend' },
    { 'Restart', 'systemctl reboot' },
    { 'Shutdown', 'systemctl poweroff' } -- other triads can be put here
  }
})

local function arrayToMenu(array)
  -- dump(array, 'Menu array', 3)
  local menu = {}

  for i, c in pairs(array) do
    menu[i] = { c[1],
      function()
        log('Execute: ' .. c[2])
        awful.spawn.with_shell(c[2])
      end
    }
  end

  -- dump(menu, 'New Menu', 3)
  return menu
end

local function getMenu(name)
  local menuJson = config_dir .. 'configuration/menu.json'
  local menus = readJson(menuJson)
  local customMenuJson = os.getenv('CUSTOMS') .. '/menu.json'
  local customMenus = readJson(customMenuJson)

  local beforeMenu = (customMenus[name] or {}).BEFORE_MENU
  local afterMenu = (customMenus[name] or {}).AFTER_MENU
  
  local menuPart = gtable.join(beforeMenu, menus[name], afterMenu)

  return arrayToMenu(menuPart)
end

return {
  mainmenu          = mainmenu,
  APP_MENU          = function()
    return getMenu('APP_MENU')
  end,
  DEVELOP_MENU      = function()
    return getMenu('DEVELOP_MENU')
  end,
  EDIT_CONFIG       = function()
    return getMenu('EDIT_CONFIG')
  end,
  SYSTEM_TOOLS_MENU = function()
    return getMenu('SYSTEM_TOOLS_MENU')
  end,
  SYSTEM_MENU       = function()
    return getMenu('SYSTEM_MENU')
  end,
  SYSTEM_MONITOR    = function()
    return getMenu('SYSTEM_MONITOR')
  end,
}

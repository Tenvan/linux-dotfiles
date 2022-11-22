log("Enter Module => " .. ... )

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

local function getMenuData(name)
  local menuJson = config_dir .. 'configuration/menu.json'
  local menus = readJson(menuJson)
  local customMenuJson = os.getenv('CUSTOMS') .. '/awesome/menu.json'
  local customMenus = readJson(customMenuJson)

  local beforeMenu = (customMenus[name] or {}).BEFORE_MENU or {}
  local afterMenu = (customMenus[name] or {}).AFTER_MENU or {}

  local menuPart = gtable.join(beforeMenu, menus[name], afterMenu)

  return menuPart
end

--- action parsen, variablen durch platzhalter ersetzen und dann ausführen
---@param action string
local function menuAction(action)
  local variables = getMenuData('VARIABLES')

  local command = action

  for i, c in pairs(variables) do
    log(c[1] .. ' -> ' .. c[2])

    if c[2] == '' then
      local ENV = os.getenv(c[1]) or c[2]
      command = string.gsub(command, "$" .. c[1], ENV)
    else
      command = string.gsub(command, c[1], c[2])
    end
  end
  log("Command: " .. command)
  awful.spawn.with_shell(command)
end

local function arrayToMenu(array)
  local menu = {}

  for i, c in pairs(array) do
    menu[i] = { c[1],
      function()
        menuAction(c[2])
      end }
  end
  return menu
end

return {
  mainmenu          = mainmenu,
  APP_MENU          = function()
    return arrayToMenu(getMenuData('APP_MENU'))
  end,
  DEVELOP_MENU      = function()
    return arrayToMenu(getMenuData('DEVELOP_MENU'))
  end,
  EDIT_CONFIG       = function()
    return arrayToMenu(getMenuData('EDIT_CONFIG'))
  end,
  SYSTEM_TOOLS_MENU = function()
    return arrayToMenu(getMenuData('SYSTEM_TOOLS_MENU'))
  end,
  SYSTEM_MENU       = function()
    return arrayToMenu(getMenuData('SYSTEM_MENU'))
  end,
  SYSTEM_MONITOR    = function()
    return arrayToMenu(getMenuData('SYSTEM_MONITOR'))
  end,
}

local log = require('utilities.debug').log
local dump = require('utilities.debug').dump
log('Enter Module => configuration/menu.lua')

local awesome, client, screen = awesome, client, screen

local awful = require('awful')
local freedesktop = require('freedesktop')
local hotkeys_popup = require('awful.hotkeys_popup').widget
local apps = require('configuration.apps')
local beautiful = require('beautiful')
local config = require('configuration.config')
local menus = config.menus

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
  before = { { 'Awesome', awesomemenu, beautiful.awesome_icon } -- { "Atom", "atom" },
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
  dump()
end

local appMenu = arrayToMenu(menus.APP_MENU)
local devMenu = {}
local sysEditMenu = {}
local sysToolMenu = arrayToMenu(menus.SYSTEM_TOOLS_MENU)
local sysMonsMenu = {}
local sysPowerMenu = {}

mymainmenu = mainmenu

return {
  mainmenu = mainmenu,
  appMenu = appMenu,
  devMenu = devMenu,
  sysEditMenu = sysEditMenu,
  sysToolMenu = sysToolMenu,
  sysMonsMenu = sysMonsMenu,
  sysPowerMenu = sysPowerMenu
}

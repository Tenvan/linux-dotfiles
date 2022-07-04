local log = require('utilities.debug').log
local dump = require('utilities.debug').dump
log('Enter Module => layout/specs.lua')

local beautiful = require('beautiful')
local dpi = beautiful.xresources.apply_dpi


return {
  topPanel = {
    height = dpi(40),
  },
  leftPanel = {
    actionBarWidth = dpi(40),
    contentWidth = dpi(350)
  },
  bottomPanel = {
    height = dpi(32),
  },
}

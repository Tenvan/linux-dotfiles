local log = require('utilities.debug').log
local dump = require('utilities.debug').dump
log("Enter Module => " .. ... )

local beautiful = require('beautiful')
local dpi = beautiful.xresources.apply_dpi

local elementSize = dpi(32)

return {
  elementSize = elementSize,
  topPanel = {
    height = elementSize,
  },
  leftPanel = {
    actionBarWidth = elementSize,
    contentWidth = dpi(350)
  },
  bottomPanel = {
    height = elementSize,
  },
}

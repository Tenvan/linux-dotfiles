local gdebug = require('gears.debug')
local config = require('configuration.config')

local function log(message, level)
  if config.debug_mode then
    gdebug.print_warning('[' .. (level or 'DEBUG') .. ']: ' .. message)
  end
end

local function dump(object, tag, depth)
  if config.debug_mode then
    log('==> Dump Tracback\n' .. debug.traceback())
  end
  local traceback = debug.traceback()
  local level = 'DUMP'
  log('==> Dump', level)
  log(gdebug.dump_return(object, tag, depth), level)
  log(traceback, level)
  log('<== Dump', level)
end

return {
  log = log,
  dump = dump
}

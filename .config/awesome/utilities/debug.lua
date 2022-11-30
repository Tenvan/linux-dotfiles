local gdebug = require('gears.debug')
local config = require('configuration.config')

local function log(message, level)
  gdebug.print_warning('[' .. (level or 'DEBUG') .. ']: ' .. message)
end

local function trace(message, level)
  if config.trace_mode then
    gdebug.print_warning('[' .. (level or 'TRACE') .. ']: ' .. message)
  end
end

local function dump(object, tag, depth)
  local traceback = debug.traceback()
  local level = 'DUMP'
  log('Dump: [[', level)
  log(gdebug.dump_return(object, tag, depth), level)
  if config.debug_mode then
    log(traceback, level)
  end
  log(']]', level)
end

return {
  log = log,
  dump = dump,
  trace = trace
}

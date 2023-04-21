local gdebug = require('gears.debug')
local config = require('configuration.config')

local function log(message, level)
  gdebug.print_warning('[' .. (level or 'LOG') .. ']: ' .. message)
end

local function logd(message, level)
  if config.debug_mode then
    gdebug.print_warning('[' .. (level or 'DEBUG') .. ']: ' .. message)
  end
end

local function trace(message, level)
  if config.trace_mode then
    gdebug.print_warning('[' .. (level or 'TRACE') .. ']: ' .. message)
  end
end

local function dump(object, tag, depth)
  local traceback = debug.traceback()
  local level = 'DUMP'
  log('[[', level)
  log(gdebug.dump_return(object, tag, depth), level)
  trace(traceback, level)
  log(']]', level)
end

return {
  logd = logd,
  log = log,
  dump = dump,
  trace = trace
}

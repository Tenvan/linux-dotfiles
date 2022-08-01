local gdebug = require('gears.debug')
local config = require('configuration.config')

local function log(message)
  if config.debug_mode then
    gdebug.print_warning('[DEBUG]: ' .. message)
  end
end

local function dump(object, tag, depth)
  if config.debug_mode then
    log(gdebug.dump_return(object, tag, depth))
  end
end

return {
  log = log,
  dump = dump
}

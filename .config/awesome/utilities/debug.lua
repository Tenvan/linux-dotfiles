local gdebug = require('gears.debug')

local function log(message)
    gdebug.print_warning(message)
end

local function dump(object, tag, depth)
    log(gdebug.dump_return(object, tag, depth))
end

return {
    log = log,
    dump = dump
}

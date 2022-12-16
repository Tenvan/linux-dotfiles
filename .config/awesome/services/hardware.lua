log('Enter Module => ' .. ...)

local result = io.popen("cat /proc/stat | grep '^cpu'"):read('*all')
log('result: ' .. result)

local cpudata = gears.string.split(result, '\n')
local cpu_count = #cpudata - 2

return {
  --- @type number
  --- number of current cpu cores
  core_count = cpu_count
}
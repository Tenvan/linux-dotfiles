log('Enter Module => ' .. ...)

-- Provides:
-- system:updates
--      packages to update
local awful = require('awful')

local update_interval = 60 * 60 -- every hour

local service_script = [[
    bash -c "~/.scripts/services/check-updates.sh"
]]

awful.widget.watch(service_script, update_interval, function(_, stdout)
  local packages = stdout
  local _, count = string.gsub(packages, '\n', '\n')
  emit('system:updates', packages, count)
end
)

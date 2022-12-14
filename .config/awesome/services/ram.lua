log('Enter Module => ' .. ...)

-- Provides:
-- service::ram
--      { total, used, free, shared, buff_cache, available, total_swap, used_swap, free_swap }
--      all: (integer - mega bytes)

local watch = require('awful.widget.watch')

local update_interval = 5
local ram_idle_script = 'bash -c "LANGUAGE=en_US free | grep -z \"Mem.*Swap.*\""'

watch(
  ram_idle_script,
  update_interval,
  function(_, stdout)
    local total, used, free, shared, buff_cache, available, total_swap, used_swap, free_swap =
    stdout:match('(%d+)%s*(%d+)%s*(%d+)%s*(%d+)%s*(%d+)%s*(%d+)%s*Swap:%s*(%d+)%s*(%d+)%s*(%d+)')
    emit('service::ram',
      { tonumber(total), tonumber(used), tonumber(free), tonumber(shared),
        tonumber(buff_cache), tonumber(available),
        tonumber(total_swap), tonumber(used_swap), tonumber(free_swap) })
  end
)

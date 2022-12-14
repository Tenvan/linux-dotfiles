log('Enter Module => ' .. ...)

-- Provides:
-- evil::cpu
--      used percentage (integer)
--      total, diff_idle, diff_total, diff_usage,
--      user, nice, system, idle, iowait, irq, softirq, steal, guest, guest_nice

local watch = require('awful.widget.watch')

local update_interval = 1
local cpu_idle_script = [[bash -c "
	cat /proc/stat | grep '^cpu '
	"]]

watch(
  cpu_idle_script, update_interval,
  function(_, stdout)
    -- trace('CPU Service: ' .. stdout)
    local user, nice, system, idle, iowait, irq, softirq, steal, guest, guest_nice =
    stdout:match('(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s')

    local total = user + nice + system + idle + iowait + irq + softirq + steal

    local diff_idle = idle - idle_prev
    local diff_total = total - total_prev
    local diff_usage = (1000 * (diff_total - diff_idle) / diff_total + 5) / 10

    emit('evil::cpu',
      tonumber(total), tonumber(diff_idle), tonumber(diff_total), tonumber(diff_usage),
      tonumber(user), tonumber(nice), tonumber(system), tonumber(idle),
      tonumber(irq), tonumber(softirq),
      tonumber(steal), tonumber(guest), tonumber(guest_nice))
  end
)

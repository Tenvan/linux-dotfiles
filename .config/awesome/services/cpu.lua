log('Enter Module => ' .. ...)

-- Provides:
-- service::cpu
--      used percentage (integer)
--      core_count, diff_core_data
--        diff_core_data[1] = Full Usage
--        diff_core_data[2...] = Core Usage

local watch = require('awful.widget.watch')

local update_interval = 1
local cpu_idle_script = [[bash -c "
cat /proc/stat | grep '^cpu'
"]]

local cpu_data = {}

watch(
  cpu_idle_script,
  update_interval,
  function(_, stdout)
    logd('CPU Service: ' .. stdout)

    local cpudata = gears.string.split(stdout, '\n')

    local core_count = #cpudata - 2
    logd('core count: ' .. tostring(core_count))

    local core_usage_data = {}

    for i = 1, core_count + 1 do
      if cpu_data[i] == nil then cpu_data[i] = {} end

      local core_cpu = cpudata[i]
      logd('core cpu data: ' .. core_cpu)

      local user, nice, system, idle, iowait, irq, softirq, steal, guest, guest_nice =
      core_cpu:match('(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)')

      local total = user + nice + system + idle + iowait + irq + softirq + steal

      local diff_idle = idle - tonumber(cpu_data[i]['idle_prev'] == nil and 0 or cpu_data[i]['idle_prev'])
      local diff_total = total -
        tonumber(cpu_data[i]['total_prev'] == nil and 0 or cpu_data[i]['total_prev'])
      local diff_usage = (1000 * (diff_total - diff_idle) / diff_total) / 10

      if i == 1 then
        core_usage_data[i] = diff_usage
      else
        core_usage_data[i] = 100- diff_usage
      end

      cpu_data[i]['total_prev'] = total
      cpu_data[i]['idle_prev'] = tonumber(idle)
    end

    emit('service::cpu', core_count, core_usage_data)
  end
)

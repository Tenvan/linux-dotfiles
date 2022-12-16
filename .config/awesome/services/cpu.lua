log('Enter Module => ' .. ...)

-- Provides:
-- service::cpu
--      cpu_usages
--        cpu_usages[0] = Full Usage
--        cpu_usages[1...cores] = Core Usage

local core_idle_script = [[bash -c "
mpstat -P ALL 1
"]]

local cpu_data = {}
local cpu_usages = {}
for i = 0, hardware.core_count do
  cpu_usages[i] = 0
end

awful.spawn.with_line_callback(core_idle_script, {
  stdout = function(line)
    local cpu = gears.string.split(line, ' ')

    local core = 0
    if cpu[2] == 'all' then
      local usage = tonumber(cpu[3])
      local core = 0
      cpu_usages[core] = usage
      emit('service::cpu', cpu_usages)
    elseif not (cpu[2] == 'CPU' or cpu[1] ~= nil) then
      -- log(string.format('CORE %d: %d', tonumber(cpu[2]), tonumber(cpu[3])))
    else
      local core = tonumber(cpu[2]) or 0
      local usage = tonumber(cpu[3]) or 0
      cpu_usages[core] = usage
    end
  end
})

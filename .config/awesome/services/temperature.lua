log('Enter Module => ' .. ...)

-- Provides:
-- service::temperature
--      temperature (integer - in Celcius)
local update_interval = 5
local temp_script = [[
  sh -c "
  sensors | grep Package | awk '{print $4}' | cut -c 2-3
  "]]

-- Periodically get temperature info
awful.widget.watch(temp_script, update_interval, function(widget, stdout)
  emit('service::temperature', tonumber(stdout))
end)

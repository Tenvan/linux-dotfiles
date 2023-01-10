log('Enter Module => ' .. ...)

-- Provides:
-- service::network
--      speed_rx (integer) = received bytes per interval
--      speed_tx (integer) = send bytes per interval

local update_interval = 0.3
-- scripts

-- Use /dev/sdxY according to your setup
local net_speed_script = [[bash -c "cat /sys/class/net/*/statistics/*_bytes"]]

local prev_rx = 0
local prev_tx = 0

local function split(string_to_split, separator)
    if separator == nil then separator = '%s' end
    local t = {}

    for str in string.gmatch(string_to_split, '([^' .. separator .. ']+)') do
        table.insert(t, str)
    end

    return t
end

-- Periodically get disk space info
awful.widget.watch(net_speed_script, update_interval, function(_, stdout)
    local cur_vals = split(stdout, '\r\n')

    local cur_rx = 0
    local cur_tx = 0

    for i, v in ipairs(cur_vals) do
        if i % 2 == 1 then cur_rx = cur_rx + v end
        if i % 2 == 0 then cur_tx = cur_tx + v end
    end

    local speed_rx = (cur_rx - prev_rx) / update_interval
    local speed_tx = (cur_tx - prev_tx) / update_interval

    prev_rx = cur_rx
    prev_tx = cur_tx

    trace(string.format('network speed: %s send: %s', tostring(speed_rx), tostring(speed_tx)))

    emit('service::network', speed_rx, speed_tx)
end)

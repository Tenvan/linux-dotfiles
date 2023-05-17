log('Enter Module => ' .. ...)

-- Provides:
-- service::volume
--      percentage (integer)
local awful = require('awful')

local volume_old = -1
local function emit_volume_info()
    awful.spawn.easy_async_with_shell('ponymix get-volume', function(stdout)
        trace(string.format('get volume => %s', stdout))
        local line = stdout:gsub('^[^\n]*\n/g', '')
        trace(string.format('line -> %s', line))

        if line ~= '' then
            local volume = stdout or 0
            local volume_int = tonumber(volume)

            trace(string.format('get volume => %s -> %s', tostring(volume_old), tostring(volume_int)))

            if volume_int ~= volume_old then
                trace('new volume: ' .. tostring(volume_int))
                emit('service::volume', volume_int)
                volume_old = volume_int
            end
        end
    end)
end

-- Run once to initialize widgets
emit_volume_info()

-- Sleeps until pactl detects an event (volume up/down/toggle mute)
local volume_script = [[
    bash -c "
    LANGUAGE=en pactl subscribe 2> /dev/null | grep --line-buffered \"Event 'change' on sink #\"
    "]]

-- Kill old pactl subscribe processes
awful.spawn.easy_async({ 'pkill', '--full', '--uid', os.getenv('USER'), '^pactl subscribe' }, function()
    -- Run emit_volume_info() with each line printed
    awful.spawn.with_line_callback(volume_script, {
        stdout = function()
            emit_volume_info()
        end
    })
end)

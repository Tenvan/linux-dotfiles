log('Enter Module => ' .. ...)

local config = require('configuration.config')

local awful = require('awful')

local function emit_signal(signal, ...)
  local count = select('#', ...)
  if config.trace_mode then
    trace(string.format('====> %s [params: %s]', signal, count))
  end

  if count == 1 then
    trace(string.format('  --> %s', tostring(...)))
  elseif count > 1 then
    for i = 1, count do
      local v = ({ ... })
      if config.trace_mode and v ~= nil then
        trace(string.format('  --> %i: %s', i, tostring((({ ... }) or {})[i])))
      end
    end
  end

  awesome.emit_signal(signal, ...)
end

local function connect_signal(signal, callback)
  log(string.format('==> register signal: %s', signal))

  awesome.connect_signal(signal, function(...)
    local count = select('#', ...)
    trace(string.format('<==== %s [params: %s]', signal, count))

    if count == 1 then
      trace(string.format('  --> %s', tostring(...)))
    elseif count > 1 then
      for i = 1, count do
        if config.trace_mode then
          trace(string.format('  --> %i: %s', i, tostring((({ ... }) or {})[i])))
        end
      end
    end

    callback(...)
  end)
end

return {
  emit = emit_signal,
  connect = connect_signal,
}

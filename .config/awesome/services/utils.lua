log('Enter Module => ' .. ...)

local awful = require('awful')

local function emit_signal(signal, ...)
  local count = select('#', ...)
  log(string.format('====> %s [params: %s]', signal, count))

  if count == 1 then
    trace(string.format('  --> %s', tostring(...)))
  elseif count > 1 then
    for i = 1, count do
      local v = ({ ... })
      if v ~= nil then
        trace(string.format('  --> %i: %s', i, tostring(({ ... })[i])))
      end
    end
  end

  awesome.emit_signal(signal, ...)
end

local function connect_signal(signal, callback)
  log(string.format('==> register signal: %s', signal))
  dump(signal)

  awesome.connect_signal(signal, function(...)
    local count = select('#', ...)
    log(string.format('<==== %s [params: %s]', signal, count))

    if count == 1 then
      trace(string.format('  --> %s', tostring(...)))
    elseif count > 1 then
      for i = 1, count do
        trace(string.format('  --> %i: %s', i, tostring(({ ... })[i])))
      end
    end

    callback(...)
  end)
end

return {
  emit = emit_signal,
  connect = connect_signal,
}

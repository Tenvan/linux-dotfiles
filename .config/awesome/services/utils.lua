log('Enter Module => ' .. ...)

local function emit_signal(signal, ...)
  awesome.emit_signal(signal, ...)
end

local function connect_signal(signal, callback)
  awesome.connect_signal(signal, function(...)
    callback(...)
  end)
end

return {
  emit = emit_signal,
  connect = connect_signal,
}

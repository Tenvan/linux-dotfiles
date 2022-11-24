log('Enter Module => ' .. ...)

local open = io.open

local function read_file(path)
  local file = open(path, 'r') -- r read mode
  if not file then
    return nil
  end
  io.input(file)
  local content = file:read('*a') -- *a or *all reads the whole file
  file:close()
  return content
end

local function write_file(path, content)
  local file = open(path, 'w+') -- w+ clear and write mode
  if not file then
    return nil
  end

  io.output(file)
  local io = file:write(content)
  file:close()
  return io
end

local function file_exists(path)
  -- Try to open it
  local f = io.open(path)
  if f then
    f:close()
    return true
  end
  return false
end

return {
  read_file = read_file,
  write_file = write_file,
  file_exists = file_exists
}

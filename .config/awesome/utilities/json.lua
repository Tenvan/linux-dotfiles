log('Enter Module => ' .. ...)


local open = io.open

local json = require('library.json')

local function read_file(path)
  log("==> read_file: " .. path)
  local file = open(path, 'r') -- r read mode
  if not file then
    log(' --> Failed to open file ' .. path)
    return nil
  end
  io.input(file)
  local content = file:read('*a') -- *a or *all reads the whole file
  file:close()
  log(" -> file closed")
  return content
end

local function write_file(path, content)
  log("==> write_file: " .. path)
  local file = open(path, 'w+') -- w+ clear and write mode
  if not file then
    log(' --> Failed to write file ' .. path)
    return nil
  end

  io.output(file)
  local io = file:write(content)
  file:close()
  log(" -> file closed")
  return io
end

local function readJsonFile(path)
  -- log("Read Json: " .. path)
  local content = read_file(path)
  -- dump(content)
  local jsonContent = json.parse(content)
  -- dump(jsonContent)
  return jsonContent
end

local function writeJsonFile(path, jsonContent)
  log('==> Write Json: ' .. path)
  local content = json.stringify(jsonContent, false)
  log(' -> content' .. tostring(content))
  local io = write_file(path, content)
  return io
end

return {
  read_file = read_file,
  write_file = write_file,
  readJsonFile = readJsonFile,
  writeJsonFile = writeJsonFile
}

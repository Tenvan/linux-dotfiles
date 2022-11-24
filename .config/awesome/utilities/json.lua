log('Enter Module => ' .. ...)

local open = io.open

local json = require('library.json')

---@deprecated use utilities.file
---@param path any
---@return nil
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

---@deprecated use utilities.file
---@param path any
---@return nil
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

local function readJsonFile(path)
  local content = read_file(path)

  local jsonContent
  if content ~= nil then
    jsonContent = json.parse(content)
  else
    jsonContent = {}
  end
  return jsonContent
end

local function writeJsonFile(path, jsonContent)
  local content = json.stringify(jsonContent, false)
  local io = write_file(path, content)
  return io
end

return {
  read_file = read_file,
  write_file = write_file,
  readJsonFile = readJsonFile,
  writeJsonFile = writeJsonFile
}

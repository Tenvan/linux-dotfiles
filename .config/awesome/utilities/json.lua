log('Enter Module => ' .. ...)

local open = io.open

local json = require('library.json')

local function read_file(path)
  local file = open(path, 'rb') -- r read mode and b binary mode
  if not file then return nil end
  local content = file:read '*a' -- *a or *all reads the whole file
  file:close()
  return content
end

local function write_file(path, content)
  local file = open(path, 'wb') -- r read mode and b binary mode
  if not file then return nil end
  local io = file:write(content)
  dump(io)
  file:close()
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
  log('Write Json: ' .. path)
  dump(json, 'json', 3)

  local content = json.stringify(jsonContent, false)
  dump(content, 'content', 3)

  local io = write_file(path, content)
  dump(jsonContent)

  return io
end

return {
  read_file = read_file,
  write_file = write_file,
  readJsonFile = readJsonFile,
  writeJsonFile = writeJsonFile
}

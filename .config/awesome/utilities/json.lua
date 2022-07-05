log('Enter Module => utilities/json.lua')

local open = io.open

local json = require('library.json')

local function read_file(path)
  local file = open(path, 'rb') -- r read mode and b binary mode
  if not file then return nil end
  local content = file:read '*a' -- *a or *all reads the whole file
  file:close()
  return content
end

local function readJsonFile(path)
  -- log("Read Json: " .. path)
  local content = read_file(path)
  -- dump(content)
  local jsonContent = json.parse(content)
  -- dump(jsonContent)
  return jsonContent
end

return {
  read_file = read_file,
  readJsonFile = readJsonFile
}

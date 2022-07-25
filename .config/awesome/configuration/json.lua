log('Enter Module => ' .. ...)

local gears = require('gears')
local filesystem = gears.filesystem
local config_dir = filesystem.get_configuration_dir()

local readJson = require('utilities.json').readJsonFile

local configJson = config_dir .. 'configuration/config.json'
local config = readJson(configJson)

return config

log('Enter Module => ' .. ...)

local gears = require('gears')
local gtable = require('gears.table')

local filesystem = gears.filesystem
local config_dir = filesystem.get_configuration_dir()

local readJson = require('utilities.json').readJsonFile

local configJson = config_dir .. 'configuration/config.json'
local customConfigJson = os.getenv('CUSTOMS') .. '/awesome/config.json'

log('config json file: ' .. configJson)
log('custom config json file: ' .. customConfigJson)

local config = readJson(configJson)
local customConfig = readJson(customConfigJson)

local finalConfig = {}

gtable.crush(finalConfig, config)
gtable.crush(finalConfig, customConfig)

dump(config, 'config')
dump(customConfig, 'customConfig')
dump(finalConfig, 'finalConfig')

return finalConfig

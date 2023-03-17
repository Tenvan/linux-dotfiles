local config = require('configuration.config-system')

function GetCustomConfig()
  local customConfigFile = os.getenv('HOME') .. '/.config/awesome/customs/awesome/config.lua'
  local file = io.open(customConfigFile, 'r') -- r read mode
  if not file then
    return
  end

  return require('customs.awesome.config')
end

local custom_config = GetCustomConfig()
config =  awful.util.table.join(config, custom_config)

return config

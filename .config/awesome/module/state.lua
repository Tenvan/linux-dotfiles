log('Enter Module => ' .. ...)

local readJson = require('utilities.json').readJsonFile
local writeJson = require('utilities.json').writeJsonFile


local statePath = os.getenv('HOME') .. '/.local/state/awesome/'
local stateFile = statePath .. 'awesome-state.json'

os.execute('mkdir -p ' .. statePath)

local stateData = readJson(stateFile) or {}

local state = {
  getState = function()
    return stateData
  end,

  setState = function(state)
    stateData = state
    writeJson(stateFile, stateData)
  end,

  setStateValue = function(path, value)
  end
}
return state

log('Enter Module => ' .. ...)

local file = require('utilities.file')
local readJson = require('utilities.json').readJsonFile
local writeJson = require('utilities.json').writeJsonFile

local statePath = string.format('%s/.local/state/awesome/', os.getenv('HOME'))
local stateFile = statePath .. 'awesome-state.json'

file.ensureDir(statePath)
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

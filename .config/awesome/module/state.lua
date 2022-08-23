log('Enter Module => ' .. ...)

local readJson = require('utilities.json').readJsonFile
local writeJson = require('utilities.json').writeJsonFile

local json = require('library.json')

local stateData = {
  version = 1
}

local stateFile = os.getenv('HOME') .. '/.local/state/awesome/awesome-state.json'

local state = {
  getState = function()
    return stateData
  end,

  setState = function(state)
    stateData = state
    writeJson(stateFile, stateData)
  end,
}
return state

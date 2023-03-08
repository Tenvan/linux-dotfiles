log('Enter Module => ' .. ...)

local function makeColorTransparent(colorkey, opacity)
  if colorkey == nil then
    error('makeColorTransparent: wrong color string: ' .. tostring(colorkey))
    return colorkey
  end

  local colorMain = string.sub(colorkey, 2, 7)
  local transColor = '#' .. colorMain .. opacity
  log('make transparent: ' .. colorkey .. ' / ' .. opacity .. ' -> ' .. transColor)
  return transColor
end

local function getScreen(screenId)
  local result = screen[1]
  if screen[2] ~= nil then
    result = screen[screenId]
  end
  
  log('Detected Screen: ' .. tostring(screenId) .. ' -> ' .. tostring(result.index))
  return result
end

local function getScreenIndex(screen)
  local result = getScreen(screen)
  return result.index
end

return {
  makeColorTransparent = makeColorTransparent,
  getScreenIndex = getScreenIndex,
  getScreen = getScreen
}

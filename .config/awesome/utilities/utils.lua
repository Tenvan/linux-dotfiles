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

return {
  makeColorTransparent = makeColorTransparent
}

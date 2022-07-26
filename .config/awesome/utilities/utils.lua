log("Enter Module => " .. ... )

local function makeColorTransparent(colorkey, opacity)
    local colorMain = string.sub(colorkey, 2, 7)
    local transColor = "#" .. colorMain .. opacity
    return transColor
end

return {
    makeColorTransparent = makeColorTransparent
}

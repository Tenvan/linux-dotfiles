local function makeColorTransparent(colorkey, opacity)
    -- gdebug.print_warning("ColorKey: " .. colorkey)
    local colorMain = string.sub(colorkey, 2, 7)
    local transColor = "#" .. colorMain .. opacity
    -- gdebug.print_warning("Color: " .. transColor)
    return transColor
end

return {
    makeColorTransparent = makeColorTransparent
}

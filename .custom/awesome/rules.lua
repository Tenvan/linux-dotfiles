-- Main Browser
local rules = {
    {
        rule = {
            class = 'Spotify'
        },
        properties = {
            screen = (screen[2] or screen[1]).index,
            tag = (screen[2] or screen[1]).tags[5],
            maximized = true,
            floating = false
        }
    },
    {
        rule = {
            instance = 'vivaldi*'
        },
        properties = {
            screen = 1,
            tag = screen[1].tags[8],
            switch_to_tags = true,
            maximized = true,
            floating = false
        }
    }
}

return rules

-- Main Browser
local rules = {
    {
        rule = {
            class = 'Spotify'
        },
        properties = {
            screen = 2,
            tag = screen[2].tags[5],
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

local filesystem = require('gears.filesystem')
local theme_dir = filesystem.get_configuration_dir() .. '/theme'

local cobalt_bg = "#072539"; -- Background
local cobalt_fg = "#e1efff"; -- Foreground
local cobalt_highlight = "#19acc6"; -- Highlight
local cobalt_accent_blue = "#0050a4"; -- Accent 1 (Blue)
local cobalt_accent_yellow = "#FFC600"; -- Accent 2 (Yellow)
local cobalt_window_bg = "#2e3a50"; -- Window Background

local makeColorTransparent = require('module.utils').makeColorTransparent

local base00 = "#232423" -- ---- red
local base01 = "#BA2922" -- ---  orange
local base02 = "#7E807E" -- --   yellow
local base03 = "#4C4F4D" -- -    green
local base04 = "#16A085" -- +    aqua/cyan
local base05 = "#43746A" -- ++   blue
local base06 = "#00CCCC" -- +++  purple
local base07 = "#E0E0E0" -- ++++ brown
local base08 = "#282928" -- red
local base09 = "#CC372C" -- orange
local base0A = "#8D8F8D" -- yellow
local base0B = "#4E524F" -- green
local base0C = "#13BF9D" -- aqua/cyan
local base0D = "#487D72" -- blue
local base0E = "#00D1D1" -- purple
local base0F = "#E8E8E8" -- brown

local theme = {}

theme.icons = theme_dir .. '/icons/'
theme.font = 'Inter Regular 10'
theme.font_bold = 'Inter Bold 10'

-- Colorscheme
-- theme.system_black_dark = '#3D4C5F'
-- theme.system_black_light = '#56687E'

-- theme.system_red_dark = '#EE4F84'
-- theme.system_red_light = '#F48FB1'

-- theme.system_green_dark = '#53E2AE'
-- theme.system_green_light = '#A1EFD3'

-- theme.system_yellow_dark = '#F1FF52'
-- theme.system_yellow_light = '#F1FA8C'

-- theme.system_blue_dark = '#6498EF'
-- theme.system_blue_light = '#92B6F4'

-- theme.system_magenta_dark = '#985EFF'
-- theme.system_magenta_light = '#BD99FF'

-- theme.system_cyan_dark = '#24D1E7'
-- theme.system_cyan_light = '#87DFEB'

-- theme.system_white_dark = '#E5E5E5'
-- theme.system_white_light = '#F8F8F2'

theme.system_black_dark = base00
theme.system_black_light = base08

theme.system_red_dark = base01
theme.system_red_light = base09

theme.system_green_dark = base02
theme.system_green_light = base0A

theme.system_yellow_dark = base03
theme.system_yellow_light = base0B

theme.system_blue_dark = base04
theme.system_blue_light = base0C

theme.system_magenta_dark = base05
theme.system_magenta_light = base0D

theme.system_cyan_dark = base06
theme.system_cyan_light = base0E

theme.system_white_dark = base07
theme.system_white_light = base0F

-- Accent color
theme.accent = theme.system_blue_dark

-- Background color
theme.background = makeColorTransparent(cobalt_window_bg, "80")

-- Transparent
theme.transparent = makeColorTransparent(cobalt_window_bg, "20")

-- Awesome icon
theme.awesome_icon = theme.icons .. 'awesome.svg'

local awesome_overrides = function(theme)
end

return {
    theme = theme,
    awesome_overrides = awesome_overrides
}

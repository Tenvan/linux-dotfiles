local awful = require("awful")

require("module.bling")
require("module.rubato")
require("module.layout-machi")
require("module.better-resize")
require("module.exit-screen")
require("module.savefloats")
require("module.bling.widget.window_switcher").enable({
    filterClients = awful.widget.tasklist.filter.allscreen
})

-- require("module.window_switcher").enable()
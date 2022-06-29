local awful = require("awful")

require("module.vicious")
require("module.bling")
require("module.rubato")
require("module.layout-machi")
require("module.better-resize")
require("module.bling.widget.window_switcher").enable({
    filterClients = awful.widget.tasklist.filter.allscreen
})

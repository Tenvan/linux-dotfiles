-- Special Key Definitions for Development Environment

require("definitions")

-- Standard awesome library
local gears              = require("gears")
local awful              = require("awful")

-- Notification library
local naughty            = require("naughty")

-- Notification library
local hotkeys_popup      = require("awful.hotkeys_popup")

local workdir            = "/media/WORKSPACE/Node/OneTime"

local shell_cmd          = terminal .. " --title='OneTimeConsole' --working-directory " .. workdir

local mydevelop          = {}

mydevelop.mydevelopitems = {
  { " shell", shell_cmd .. " --hold" },
  { " yarn", shell_cmd .. " --hold -e yarn" },
  { " generate", shell_cmd .. " --hold -e yarn generate" },
  { " check updates", shell_cmd .. " --hold -e yarn outdated" },
  { " start server", shell_cmd .. " --hold -e yarn server:dev" },
  { " pug watch", shell_cmd .. "/src/client --hold -e yarn pug:watch" },
  { " start", shell_cmd .. "/src/client --hold -e yarn start" },
  { " start hmr", shell_cmd .. "/src/client --hold -e yarn start:client:hmr --port 4201" },
  { " start aot", shell_cmd .. "/src/client --hold -e yarn start:client:dev --aot --port 4202" },
  { " yarn install", shell_cmd .. " --hold -e yarn install --ignore-scripts" },
  { " yarn update", shell_cmd .. " --hold -e yarn run update:all" },
}

mydevelop.mydevelopmenu      = awful.menu(
  {
    items = mydevelop.mydevelopitems
  })

-- {{{ Key bindings
mydevelop.mydevkeys          = gears.table.join(
--- system tools
  awful.key({ modkey, "Shift" }, "d",
            function()
              mydevelop.mydevelopmenu:show()
            end,
            { description = "show menu", group = "OneTime" }
  ),
  awful.key({ modkey, "Shift" }, "k",
            function()
              awful.spawn.easy_async_with_shell(
                shell_cmd .. " -e killall node",
                function()
                  naughty.notify({
                                   preset = naughty.config.presets.normal,
                                   title  = "Info!",
                                   text   = " Node Prozesse abgeschossen.."
                                 })
                  awful.spawn(shell_cmd .. ' --hold -e ps -lC node')
                end
              );
            end,
            { description = "kill nodes", group = "OneTime" }
  )
)
-- }}}

return mydevelop

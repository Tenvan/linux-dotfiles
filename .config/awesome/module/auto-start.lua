-- MODULE AUTO-START
-- Run all the apps listed in configuration/apps.lua as run_on_start_up only once when awesome start
log('Enter Module => ' .. ...)

local awful = require('awful')
local apps = require('configuration.apps')

local run_once = function(cmd)
  log ("====> try to autostart: " .. cmd)
  local findme = cmd
  local firstspace = cmd:find(' ')
  if firstspace then
    findme = cmd:sub(0, firstspace - 1)
  end
  awful.spawn.easy_async_with_shell(
    string.format('pgrep -u $USER -x %s > /dev/null || (%s)', findme, cmd),
    function(stdout, stderr)
      -- Debugger
      if not stderr or stderr == '' then
        trace('  ->  auto started: ' .. cmd)
        return
      end

      log(' ->  error on start: ' .. cmd)
      log('  ERROR: ' .. stderr:gsub('%\n', ''))

      notify('<b>Oof! Error detected when starting an application!</b>',
        string.format("Command: %s\nError: %s", cmd,   stderr:gsub('%\n', '')),
        'critical',
        nil,
        'Start-up Applications')
    end
  )
end

for _, app in ipairs(apps.run_on_start_up) do
  run_once(app)
end

log('Enter Module => ' .. ...)

-- ░█▀█░█▀█░▀█▀░▀█▀░█▀▀░▀█▀░█▀▀░█▀█░▀█▀░▀█▀░█▀█░█▀█░░░█░░░▀█▀░█▀▄░█▀▄░█▀█░█▀▄░█░█
-- ░█░█░█░█░░█░░░█░░█▀▀░░█░░█░░░█▀█░░█░░░█░░█░█░█░█░░░█░░░░█░░█▀▄░█▀▄░█▀█░█▀▄░░█░
-- ░▀░▀░▀▀▀░░▀░░▀▀▀░▀░░░▀▀▀░▀▀▀░▀░▀░░▀░░▀▀▀░▀▀▀░▀░▀░░░▀▀▀░▀▀▀░▀▀░░▀░▀░▀░▀░▀░▀░░▀░
local naughty = require('naughty')
local beautiful = require('beautiful')

local function notify(titel, message, urgency, icon, app_name)
  -- Icon Definitions: https://specifications.freedesktop.org/icon-naming-spec/latest/ar01s04.html

  -- log('Notify Title: ' .. titel)
  -- log('Notify Message: ' .. message)

  if (titel ~= nil or message ~= nil) then
    local notification = {
      title    = titel,
      message  = message,
      urgency  = urgency or 'normal',
      app_name = app_name or 'System Notification',
      icon     = icon or nil
    }

    dump(notification, 'note', 2)

    naughty.notification(notification)
  end
end

return notify

log("Enter Module => " .. ... )

-- ░█▀█░█▀█░▀█▀░▀█▀░█▀▀░▀█▀░█▀▀░█▀█░▀█▀░▀█▀░█▀█░█▀█░░░█░░░▀█▀░█▀▄░█▀▄░█▀█░█▀▄░█░█
-- ░█░█░█░█░░█░░░█░░█▀▀░░█░░█░░░█▀█░░█░░░█░░█░█░█░█░░░█░░░░█░░█▀▄░█▀▄░█▀█░█▀▄░░█░
-- ░▀░▀░▀▀▀░░▀░░▀▀▀░▀░░░▀▀▀░▀▀▀░▀░▀░░▀░░▀▀▀░▀▀▀░▀░▀░░░▀▀▀░▀▀▀░▀▀░░▀░▀░▀░▀░▀░▀░░▀░
local naughty = require('naughty')

local function notify(titel, message, urgency, playSound, icon)
  -- Icon Definitions: https://specifications.freedesktop.org/icon-naming-spec/latest/ar01s04.html

  -- log('Notify Title: ' .. titel)
  -- log('Notify Message: ' .. message)

  if (titel ~= nil or message ~= nil) then
    naughty.notification {
      urgency  = urgency,
      title    = titel,
      message  = message,
      app_name = 'System Notification',
      icon     = icon
    }
  end
end

return notify

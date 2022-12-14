log('Enter Module => ' .. ...)

-- Monitoring
require("services.cpu")
require("services.ram")
-- require("services.temperature")
-- require("services.battery")
-- require("services.disk")

-- User controlled
-- require("services.volume")
-- require("services.microphone")
-- require("services.mpd")
-- require("services.brightness")
require("services.spotify")
require("services.spotify-bookmarks")

-- Internet access required
-- Note: These daemons use a temp file to store the retrieved values in order
-- to check its modification time and decide if it is time to update or not.
-- No need to worry that you will be updating too often when restarting AwesomeWM :)
-- This is useful because some APIs have a limit on the number of calls per hour.
-- require("services.coronavirus")
-- require("services.weather")

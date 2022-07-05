log('Enter Module => widget/weather/weather-aw.lua')

local beautiful = require('beautiful')

local weather_curl_widget = require('awesome-wm-widgets.weather-widget.weather')
local config = require('configuration.config').widget

return {
  widget = weather_curl_widget({
    font_name = beautiful.font_family,
    api_key = config.weather.key,
    coordinates = { 51.77501, 9.38155 },
    time_format_12h = false,
    units = config.weather.units,
    both_units_widget = false,
    show_hourly_forecast = true,
    show_daily_forecast = true,
    size = 600
  }),
}

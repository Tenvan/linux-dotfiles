local awesome, root = awesome, root
local gears = require('gears')
local awful = require('awful')

local mainmenu = require('configuration.menu').mainmenu

root.buttons(
  gears.table.join(
    awful.button({}, 1, function()
      if mainmenu then
        mainmenu:hide()
      end
    end),
    awful.button({}, 3, function()
      if mainmenu then
        mainmenu:toggle()
      end
    end),
    awful.button({ 'Control' }, 2, function()
      emit('module::exit_screen:show')
    end),
    awful.button({ 'Shift' }, 2, function()
      emit('widget::blue_light:toggle')
    end),
    awful.button({}, 4, function()
      awful.spawn('light -A 10', false)
      emit('widget::brightness')
      emit('module::brightness_osd:show', true)
    end),
    awful.button({}, 5, function()
      awful.spawn('light -U 10', false)
      emit('widget::brightness')
      emit('module::brightness_osd:show', true)
    end),
    awful.button({ 'Control' }, 4, function()
      awful.spawn('amixer -D pulse sset Master 5%+', false)
      emit('widget::volume')
      emit('module::volume_osd:show', true)
    end),
    awful.button({ 'Control' }, 5, function()
      awful.spawn('amixer -D pulse sset Master 5%-', false)
      emit('widget::volume')
      emit('module::volume_osd:show', true)
    end)))

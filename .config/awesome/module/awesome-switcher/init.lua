-- work in progress

log('Enter Module => ' .. ...)

local cairo = require('lgi').cairo
local mouse = mouse
local screen = screen
local wibox = require('wibox')
local table = table
local timer = timer
local keygrabber = keygrabber
local math = require('math')
local awful = require('awful')
local gears = require('gears')
local client = client
awful.client = require('awful.client')

local naughty = require('naughty')
local string = string
local tostring = tostring
local tonumber = tonumber
local debug = debug
local pairs = pairs
local unpack = unpack or table.unpack

local find_clients = require('helpers').find_clients

local function get_clients()
  local clients = find_clients({
    minimized = false,
    skip_taskbar = false,
    hidden = false,
    screen = awful.screen.focused()
  }) or {}

  return clients
end

--module("awesome-switcher-preview")

local surface = cairo.ImageSurface(cairo.Format.RGB24, 20, 20)
local cr = cairo.Context(surface)

-- settings

local settings = {
  preview_box = true,
  preview_box_bg = '#ddddddaa',
  preview_box_border = '#22222200',
  preview_box_fps = 30,
  preview_box_delay = 150,
  preview_box_title_font = { 'sans', 'italic', 'normal' },
  preview_box_title_font_size_factor = 0.8,
  preview_box_title_color = { 0, 0, 0, 1 },

  client_opacity = false,
  client_opacity_value_selected = 1,
  client_opacity_value_in_focus = 0.5,
  client_opacity_value = 0.5,
}

-- Create a wibox to contain all the client-widgets
local preview_wbox = wibox({ width = screen[mouse.screen].geometry.width })
preview_wbox.border_width = 3
preview_wbox.ontop = true
preview_wbox.visible = false

local preview_live_timer = gears.timer({}) --( {timeout = 1/settings.preview_box_fps} )
local preview_widgets = {}

local altTabTable = {}
local altTabIndex = 1

local source = string.sub(debug.getinfo(1, 'S').source, 2)
local path = string.sub(source, 1, string.find(source, '/[^/]*$'))
local noicon = path .. 'noicon.png'

-- simple function for counting the size of a table
local function tableLength(T)
  local count = 0
  for _ in pairs(T) do count = count + 1 end
  return count
end

-- this function returns the list of clients to be shown.
local function getClients()
  return get_clients()
end

-- here we populate altTabTable using the list of clients taken from
-- getClients(). In case we have altTabTable with some value, the list of the
-- old known clients is restored.
local function populateAltTabTable()
  local clients = getClients()

  if tableLength(altTabTable) then
    for ci = 1, #clients do
      for ti = 1, #altTabTable do
        if altTabTable[ti].client == clients[ci] then
          altTabTable[ti].client.opacity = altTabTable[ti].opacity
          altTabTable[ti].client.minimized = altTabTable[ti].minimized
          break
        end
      end
    end
  end

  altTabTable = {}

  for i = 1, #clients do
    table.insert(altTabTable, {
      client = clients[i],
      minimized = clients[i].minimized,
      opacity = clients[i].opacity
    })
  end
end

-- If the length of list of clients is not equal to the length of altTabTable,
-- we need to repopulate the array and update the UI. This function does this
-- check.
local function clientsHaveChanged()
  local clients = getClients()
  return tableLength(clients) ~= tableLength(altTabTable)
end

local function createPreviewText(client)
  if client.class then
    return ' - ' .. client.class
  else
    return ' - ' .. client.name
  end
end

-- Preview is created here.
local function preview()
  if not settings.preview_box then return end

  -- Apply settings
  preview_wbox:set_bg(settings.preview_box_bg)
  preview_wbox.border_color = settings.preview_box_border

  -- Make the wibox the right size, based on the number of clients
  local n = math.max(7, #altTabTable)
  local W = screen[mouse.screen].geometry.width -- + 2 * preview_wbox.border_width
  local w = W / n -- widget width
  local h = w * 0.75 -- widget height
  local textboxHeight = w * 0.125

  local x = screen[mouse.screen].geometry.x - preview_wbox.border_width
  local y = screen[mouse.screen].geometry.y + (screen[mouse.screen].geometry.height - h - textboxHeight) / 2
  preview_wbox:geometry({ x = x, y = y, width = W, height = h + textboxHeight })

  -- create a list that holds the clients to preview, from left to right
  local leftRightTab = {}
  local nLeft
  local nRight
  if #altTabTable == 2 then
    nLeft = 0
    nRight = 2
  else
    nLeft = math.floor(#altTabTable / 2)
    nRight = math.ceil(#altTabTable / 2)
  end

  for i = 1, nLeft do
    table.insert(leftRightTab, altTabTable[#altTabTable - nLeft + i].client)
  end
  for i = 1, nRight do
    table.insert(leftRightTab, altTabTable[i].client)
  end

  -- determine fontsize -> find maximum classname-length
  local text, textWidth, textHeight, maxText
  local maxTextWidth = 0
  local maxTextHeight = 0
  local bigFont = textboxHeight / 2
  cr:set_font_size(fontSize)
  for i = 1, #leftRightTab do
    text = createPreviewText(leftRightTab[i])
    textWidth = cr:text_extents(text).width
    textHeight = cr:text_extents(text).height
    if textWidth > maxTextWidth or textHeight > maxTextHeight then
      maxTextHeight = textHeight
      maxTextWidth = textWidth
      maxText = text
    end
  end

  while true do
    cr:set_font_size(bigFont)
    textWidth = cr:text_extents(maxText).width
    textHeight = cr:text_extents(maxText).height

    if textWidth < w - textboxHeight and textHeight < textboxHeight then
      break
    end

    bigFont = bigFont - 1
  end
  local smallFont = bigFont * settings.preview_box_title_font_size_factor

  preview_widgets = {}

  -- create all the widgets
  for i = 1, #leftRightTab do
    preview_widgets[i] = wibox.widget.base.make_widget()
    preview_widgets[i].fit = function(preview_widget, width, height)
      return w, h
    end

    preview_widgets[i].draw = function(preview_widget, preview_wbox, cr, width, height)
      if width ~= 0 and height ~= 0 then

        local c = leftRightTab[i]
        local a = 0.8
        local overlay = 0.6
        local fontSize = smallFont
        if c == altTabTable[altTabIndex].client then
          a = 0.9
          overlay = 0
          fontSize = bigFont
        end

        local sx, sy, tx, ty

        -- Icons
        local icon
        if c.icon == nil then
          icon = gears.surface(gears.surface.load(noicon))
        else
          icon = gears.surface(c.icon)
        end

        local iconboxWidth = 0.9 * textboxHeight
        local iconboxHeight = iconboxWidth

        -- Titles
        cr:select_font_face(unpack(settings.preview_box_title_font))
        cr:set_font_face(cr:get_font_face())
        cr:set_font_size(fontSize)

        text = createPreviewText(c)
        textWidth = cr:text_extents(text).width
        textHeight = cr:text_extents(text).height

        local titleboxWidth = textWidth + iconboxWidth
        local titleboxHeight = textboxHeight

        -- Draw icons
        tx = (w - titleboxWidth) / 2
        ty = h
        sx = iconboxWidth / icon.width
        sy = iconboxHeight / icon.height

        cr:translate(tx, ty)
        cr:scale(sx, sy)
        cr:set_source_surface(icon, 0, 0)
        cr:paint()
        cr:scale(1 / sx, 1 / sy)
        cr:translate(-tx, -ty)

        -- Draw titles
        tx = tx + iconboxWidth
        ty = h + (textboxHeight + textHeight) / 2

        cr:set_source_rgba(unpack(settings.preview_box_title_color))
        cr:move_to(tx, ty)
        cr:show_text(text)
        cr:stroke()

        -- Draw previews
        local st = awful.screen.focused().selected_tag
        if c.first_tag == st then

          local cg = c:geometry()
          if cg.width > cg.height then
            sx = a * w / cg.width
            sy = math.min(sx, a * h / cg.height)
          else
            sy = a * h / cg.height
            sx = math.min(sy, a * h / cg.width)
          end

          tx = (w - sx * cg.width) / 2
          ty = (h - sy * cg.height) / 2

          local tmp = gears.surface(c.content)
          cr:translate(tx, ty)
          cr:scale(sx, sy)
          cr:set_source_surface(tmp, 0, 0)
          cr:paint()
          tmp:finish()

          -- Overlays
          cr:scale(1 / sx, 1 / sy)
          cr:translate(-tx, -ty)
          cr:set_source_rgba(0, 0, 0, overlay)
          cr:rectangle(tx, ty, sx * cg.width, sy * cg.height)
          cr:fill()
        end
      end
    end
  end

  -- Spacers left and right
  local spacer = wibox.widget.base.make_widget()
  spacer.fit = function(leftSpacer, width, height)
    return (W - w * #altTabTable) / 2, preview_wbox.height
  end
  spacer.draw = function(preview_widget, preview_wbox, cr, width, height) end

  --layout
  preview_layout = wibox.layout.fixed.horizontal()

  preview_layout:add(spacer)
  for i = 1, #leftRightTab do
    preview_layout:add(preview_widgets[i])
  end
  preview_layout:add(spacer)

  preview_wbox:set_widget(preview_layout)
end

-- This is called any settings.preview_box_fps milliseconds. In case the list
-- of clients is changed, we need to redraw the whole preview box. Otherwise, a
-- simple widget::updated signal is enough
local function updatePreview()
  if clientsHaveChanged() then
    populateAltTabTable()
    preview()
  end

  for i = 1, #preview_widgets do
    preview_widgets[i]:emit_signal('widget::updated')
  end
end

local function clientOpacity()
  if not settings.client_opacity then return end

  local opacity = settings.client_opacity_value
  if opacity > 1 then opacity = 1 end
  for i, data in pairs(altTabTable) do
    data.client.opacity = opacity
  end

  if client.focus == altTabTable[altTabIndex].client then
    -- Let's normalize the value up to 1.
    local opacityFocusSelected = settings.client_opacity_value_selected + settings.client_opacity_value_in_focus
    if opacityFocusSelected > 1 then opacityFocusSelected = 1 end
    client.focus.opacity = opacityFocusSelected
  else
    -- Let's normalize the value up to 1.
    local opacityFocus = settings.client_opacity_value_in_focus
    if opacityFocus > 1 then opacityFocus = 1 end
    local opacitySelected = settings.client_opacity_value_selected
    if opacitySelected > 1 then opacitySelected = 1 end

    client.focus.opacity = opacityFocus
    altTabTable[altTabIndex].client.opacity = opacitySelected
  end
end

-- This starts the timer for updating and it shows the preview UI.
local function showPreview()
  preview_live_timer.timeout = 1 / settings.preview_box_fps
  preview_live_timer:connect_signal('timeout', updatePreview)
  preview_live_timer:start()

  preview()
  preview_wbox.visible = true

  clientOpacity()
end

local function cycle(dir)
  -- Switch to next client
  altTabIndex = altTabIndex + dir
  if altTabIndex > #altTabTable then
    altTabIndex = 1 -- wrap around
  elseif altTabIndex < 1 then
    altTabIndex = #altTabTable -- wrap around
  end

  updatePreview()

  altTabTable[altTabIndex].client.minimized = false

  if not settings.preview_box and not settings.client_opacity then
    client.focus = altTabTable[altTabIndex].client
  end

  local nextClient = altTabTable[altTabIndex].client
  log(' --> Jump to: ' .. tostring(nextClient))
  nextClient:raise()
  nextClient:jump_to()

  if settings.client_opacity and preview_wbox.visible then
    clientOpacity()
  end
end

local function switch(dir, alt, tab, shift_tab)
  populateAltTabTable()

  if #altTabTable == 0 then
    return
  elseif #altTabTable == 1 then
    altTabTable[1].client.minimized = false
    altTabTable[1].client:raise()
    return
  end

  -- reset index
  altTabIndex = 1

  -- preview delay timer
  local previewDelay = settings.preview_box_delay / 1000
  local previewDelayTimer = gears.timer({ timeout = previewDelay })
  previewDelayTimer:connect_signal('timeout', function()
    previewDelayTimer:stop()
    showPreview()
  end)
  previewDelayTimer:start()

  -- Now that we have collected all windows, we should run a keygrabber
  -- as long as the user is alt-tabbing:
  keygrabber.run(
    function(mod, key, event)
      -- Stop alt-tabbing when the alt-key is released
      if key == alt or key == 'Escape' and event == 'release' then
        if preview_wbox.visible == true then
          preview_wbox.visible = false
          preview_live_timer:stop()
        else
          previewDelayTimer:stop()
        end

        if key == 'Escape' then
          for i = 1, #altTabTable do
            altTabTable[i].client.opacity = altTabTable[i].opacity
            altTabTable[i].client.minimized = altTabTable[i].minimized
          end
        else
          -- Raise clients in order to restore history
          local c
          for i = 1, altTabIndex - 1 do
            c = altTabTable[altTabIndex - i].client
            if not altTabTable[i].minimized then
              c:raise()
              client.focus = c
            end
          end

          -- raise chosen client on top of all
          c = altTabTable[altTabIndex].client
          c:raise()
          client.focus = c

          -- restore minimized clients
          for i = 1, #altTabTable do
            if i ~= altTabIndex and altTabTable[i].minimized then
              altTabTable[i].client.minimized = true
            end
            altTabTable[i].client.opacity = altTabTable[i].opacity
          end
        end

        keygrabber.stop()
        return

        -- Move to next client on each Tab-press
      elseif (key == tab or key == 'Right') and event == 'press' then
        cycle(1)

        -- Move to previous client on Shift-Tab
      elseif (key == shift_tab or key == 'Left') and event == 'press' then
        cycle(-1)
      end
    end
  )

  -- switch to next client
  cycle(dir)
end -- function altTab

return { switch = switch, settings = settings }

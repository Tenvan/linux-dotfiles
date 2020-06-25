-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard Definitions
require("definitions")

-- Standard awesome library
local awful = require("awful")

-- Notification library
local naughty = require("naughty")

-- Theme handling library
local beautiful = require("beautiful")

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
-- require("awful.hotkeys_popup.keys")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
  naughty.notify(
    {
      preset = naughty.config.presets.critical,
      title = "Oops, there were errors during startup!",
      text = awesome.startup_errors
    }
  )
end

-- Handle runtime errors after startup
do
  local in_error = false
  awesome.connect_signal(
    "debug::error",
    function(err)
      -- Make sure we don't go into an endless error loop
      if in_error then
        return
      end
      in_error = true

      naughty.notify(
        {
          preset = naughty.config.presets.critical,
          title = "Oops, an error happened!",
          text = tostring(err)
        }
      )
      in_error = false
    end
  )
end
-- }}}

--client.connect_signal("unmanage", function(c)
--  if client.focus == c and c.transient_for then
--    client.focus = c.transient_for
--    c.transient_for:raise()
--  end
--end)

local last_focus
client.connect_signal(
  "unfocus",
  function(c)
    gdebug.print_warning(string.format("call unfocus: %s", c))
    gdebug.dump(c)
    last_focus = c
  end
)
client.connect_signal(
  "focus",
  function(c)
    gdebug.print_warning(string.format("call focus: %s", c))
    gdebug.dump(c)
    last_focus = nil
  end
)
client.connect_signal(
  "unmanage",
  function(c)
    gdebug.print_warning("call unmanaged")
    gdebug.dump(c)
    if last_focus == c and c.transient_for then
      client.focus = c.transient_for
      c.transient_for:raise()
      gdebug.print_warning("call raised")
    end
  end
)

-- {{{ Menu
-- Standard Definitions
require("launcher")
-- }}}

-- {{{ Wibar
-- require("bars")
-- }}}

-- {{{ Key bindings
require("bindings")
-- require("collision")()
-- }}}

-- {{{ Rules
require("rules")
-- }}}

-- {{{ Signals
require("signals")
-- }}}

--- {{{ Autostart
awful.spawn.with_shell("~/.config/awesome/autorun.sh")
--- }}}

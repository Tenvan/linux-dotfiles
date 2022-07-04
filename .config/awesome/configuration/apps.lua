local log = require('utilities.debug').log
local dump = require('utilities.debug').dump
log('Enter Module => configuration/apps.lua')

local filesystem = require('gears.filesystem')
local config_dir = filesystem.get_configuration_dir()
local utils_dir = config_dir .. 'utilities/'

local apps = {
  -- Default Applications
  default = {
    -- Default terminal emulator
    terminal = 'kitty',
    -- Defalut music client
    music_player = 'kitty --class music -e ncmpcpp',
    -- Default text editor
    text_editor = 'kitty -e micro',
    -- Default code editor
    code_editor = 'code',
    -- Default web browser
    web_browser = 'vivaldi-stable',
    -- Default file manager
    file_manager = 'nemo',
    -- Default bluetooth manager
    bluetooth_manager = 'blueman-manager',
    -- Default power manager
    power_manager = 'xfce4-power-manager',
    -- Default rofi global menu
    app_launcher = 'xfce4-appfinder',
    -- Default media player
    multimedia = 'vlc',
    -- Default game, can be a launcher like steam
    game = 'supertuxkart',
    -- Default graphics editor
    graphics = 'gimp-2.10',
    -- Default sandbox
    sandbox = 'virtualbox',
    -- Default IDE
    development = '',
    -- Default network manager
    network_manager = 'kitty iwctl',
    -- Default GUI package manager
    package_manager = 'pamac-manager',
    -- Default locker
    lock = 'awesome-client "awesome.emit_signal(\'module::lockscreen_show\')"',
    -- Default quake terminal
    quake = 'kitty --name QuakeTerminal',

  },

  -- List of apps to start once on start-up
	run_on_start_up = {
		-- Compositor
		'picom -b --experimental-backends --dbus --config ' ..	config_dir .. '/configuration/picom.conf',
		-- Blueman applet
		'blueman-applet',
		-- Music server
		'mpd',
		-- Polkit and keyring
		'/usr/bin/lxqt-policykit-agent &' ..	' eval $(gnome-keyring-daemon -s --components=pkcs11,secrets,ssh,gpg)',
		-- Audio equalizer
		'pulseeffects --gapplication-service',
		-- Lockscreen timer
		-- [[
		-- xidlehook --not-when-fullscreen --not-when-audio --timer 600 \
		-- "awesome-client 'awesome.emit_signal(\"module::lockscreen_show\")'" ""
		-- ]]

		-- You can add more start-up applications here
	},

  -- List of binaries/shell scripts that will execute for a certain task
  utils = {
    -- Fullscreen screenshot
    full_screenshot = utils_dir .. 'screensht full',
    -- Area screenshot
    area_screenshot = utils_dir .. 'screensht area',
    -- Color Picker
    color_picker    = utils_dir .. 'xcolor-pick',
    -- Update profile picture
    update_profile  = utils_dir .. 'profile-image'
  },
}

return apps

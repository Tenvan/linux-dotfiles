log('Enter Module => ' .. ...)

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
    -- Default Teams
    -- teams = 'teams',
    teams = 'teams-for-linux',
    -- Default IDE
    development = '',
    -- Default network manager
    network_manager = 'kitty iwctl',
    -- Default GUI package manager
    package_manager = 'pamac-manager',
    -- Default locker
    lock = 'awesome-client "awesome.emit_signal(\'module::lockscreen_show\')"',
    -- Default quake terminal
    quake = 'kitty --name QuakeTerminal --class QuakeTerminal',

  },

  -- List of apps to start once on start-up
  run_on_start_up = {
    -- Compositor
    -- 'picom -b --experimental-backends --dbus --config ' .. config_dir .. '/configuration/picom.conf',
    -- '$SCRIPTS/picom-toggle-awesome.sh',
    -- Blueman applet
    -- 'blueman-applet',
    -- Polkit and keyring
    -- '/usr/bin/lxqt-policykit-agent &' ..	' eval $(gnome-keyring-daemon -s --components=pkcs11,secrets,ssh,gpg)',
    -- '/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &' .. ' eval $(gnome-keyring-daemon -s --components=pkcs11,secrets,ssh,gpg)',
    -- Audio equalizer
    -- 'pulseeffects --gapplication-service',
    -- Lockscreen timer
    -- [[
    -- xidlehook --not-when-fullscreen --not-when-audio --timer 600 \
    -- "awesome-client 'awesome.emit_signal(\"module::lockscreen_show\")'" ""
    -- ]],

    -- You can add more start-up applications here
    -- 'xfce4-power-manager',
    '$HOME/.bin/volume-osd',
    'pamac-tray',
    'nm-applet',
    'pasystray',
    'alttab -n 1',
    'flameshot',
    'copyq',
    'eject-applet',
    -- 'ejectsy',

    -- 'teams-for-linux',
    -- 'kdeconnect-indicator'
    -- 'xscreensaver'
    -- 'xsettingsd'
  },

  -- List of binaries/shell scripts that will execute for a certain task
  utils = {
    -- Fullscreen screenshot
    full_screenshot = 'flameshot full -c',
    -- Area screenshot
    area_screenshot = 'flameshot gui -c',
    -- Color Picker
    color_picker    = utils_dir .. 'xcolor-pick',
    -- Update profile picture
    update_profile  = utils_dir .. 'profile-image'
  },
}

return apps

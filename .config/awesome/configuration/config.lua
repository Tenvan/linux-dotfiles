local log = require('utilities.debug').log
local dump = require('utilities.debug').dump
log('Enter Module => configuration/config.lua')

local config = {
  widget = {
    email = {
      -- Email address
      address = '',
      -- App password
      app_password = '',
      -- Imap server
      imap_server = 'imap.gmail.com',
      -- Port
      port = '993'
    },

    weather = {
      -- API Key
      key = 'cf806d5e1d1e72f3819d52e0e0441aac',
      -- City ID
      city_id = '',
      -- City ID
      city_coords = { 51.77501, 9.38155 },
      -- Units
      units = 'metric',
      -- Update in N seconds
      update_interval = 1200
    },

    network = {
      -- Wired interface
      wired_interface = 'eno1',
      -- Wireless interface
      -- wireless_interface = 'wlan0'
    },

    clock = {
      -- Clock widget format
      military_mode = false
    },

    screen_recorder = {
      -- Default record dimension
      resolution = '1366x768',
      -- X,Y coordinate
      offset = '0,0',
      -- Enable audio by default
      audio = false,
      -- Recordings directory
      save_directory = '$(xdg-user-dir VIDEOS)/Recordings/',
      -- Mic level
      mic_level = '20',
      -- FPS
      fps = '30'
    }
  },

  module = {
    auto_start = {
      -- Will create notification if true
      debug_mode = false
    },

    dynamic_wallpaper = {
      -- Will look for wallpapers here
      wall_dir = 'theme/wallpapers/',
      -- Image formats
      valid_picture_formats = { 'jpg', 'png', 'jpeg' },
      -- Leave this table empty for full auto scheduling
      wallpaper_schedule = {
        ['00:00:00'] = 'midnight-wallpaper.jpg',
        ['06:22:00'] = 'morning-wallpaper.jpg',
        ['12:00:00'] = 'noon-wallpaper.jpg',
        ['17:58:00'] = 'night-wallpaper.jpg'
        -- Example of just using auto-scheduling with keywords
        --[[
					'midnight',
					'morning',
					'noon',
					'afternoon',
					'evening',
					'night'
				--]]
      },
      -- Stretch background image across all screens(monitor)
      stretch = false
    },

    lockscreen = {
      -- Clock format
      military_clock = true,
      -- Default password if there's no PAM integration
      fallback_password = 'time',
      -- Capture intruder using webcam
      capture_intruder = true,
      -- Intruder image save location (Will create directory if it doesn't exist)
      face_capture_dir = '$(xdg-user-dir PICTURES)/Intruders/',
      -- Background directory - Defaults to 'awesome/config/theme/wallpapers/' if null
      bg_dir = nil,
      -- Will look for this image file under 'bg_dir'
      bg_image = 'locksreen-bg.jpg',
      -- Blur lockscreen background
      blur_background = false,
      -- Blurred/filtered background image path (No reason to change this)
      tmp_wall_dir = '/tmp/awesomewm/' .. os.getenv('USER') .. '/'
    }
  },

  menus = {
    APP_MENU = {
      { '🇯 JetBrains Toolbox', 'jetbrains-toolbox' },
      { '🇹 Teams', 'teams-for-linux' },
      { '🇫 Font Manager', 'font-manager' },
      { '🇻 Virtio Manager', 'virt-manager' },
      { '🇷 Remmina', 'remmina' },
      { '🗂 Dateimanager', '$FILEMANAGER' },
      { '😃 Emoji Test', '$TERMINAL --hold curl https://unicode.org/Public/emoji/5.0/emoji-test.txt' },
      { '☦ UTF8 Test', '$TERMINAL --hold curl https://www.w3.org/2001/06/utf-8-test/UTF-8-demo.html' },
      { '🌤 Wetter Brakel', '$TERMINAL --hold --title Wetter:Brakel curl wttr.in/33034?lang=de' },
      { '🌤 Wetter Höxter', '$TERMINAL --hold --title Wetter:Höxter curl wttr.in/37671?lang=de' }
    },
    SYSTEM_TOOLS_MENU = {
      { '🪄 ReInstall All Packages', '$shellCmd --hold --title Sys:Upall pacman -Qqn | pacman -S -' },
      { '🪄 Grub Update (BTRFS Snapshots)', '$shellCmd --hold --title Sys:Grubup $timeCmd sudo update-grub' },
      { '🕰 Timeshift', 'timeshift-launcher' },
      { '🕰 Timeshift create Snapshot', '$shellCmd --hold --title Sys:Install $timeCmd sudo timeshift --create' },
      { '🗂 Belegung Verzeichnisse', 'baobab' },
      { '🖥 Monitor einrichten', 'arandr' },
      { '🔌 Power Manager', 'xfce4-power-manager -c' },
      { '🎫 Erscheinungsbild (Lx)', 'lxappearance' },
      { '🎫 Erscheinungsbild (Xfce4)', 'xfce4-appearance-settings' },
      { '🎫 Erscheinungsbild (Qt5)', 'qt5ct' },
      { '📛 Boot Logs', 'qjournalctl' },
      { '📛 Log Viewer (Gui)', 'glogg' },
      { '📛 System Logs (Gui)', 'sudo ksystemlog' },
      { '📛 System Logs (Console)', '$shellCmd --hold --title Sys:Install $timeCmd journalctl -f' },
      { '📛 xsession Errors', '$shellCmd --title AWMTT multitail -cs -i $HOME/.xsession-errors' },
    },
  },
}

return config

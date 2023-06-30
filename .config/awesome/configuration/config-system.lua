local config = {
  debug_mode = false,
  trace_mode = false,
  sounds = true,
  font_size_small = 8,
  font_size_normal = 12,
  font_size_large = 16,
  font_size_big = 20,
  widget = {
    email = {
      -- Email address
      address = 'ralf.stich@gmail.com',
      -- App password
      app_password = '',
      -- Imap server
      imap_server = 'imap.gmail.com',
      -- Port
      port = '993'
    },
    weather = {
      -- API Key
      key = '<insert key>',
      -- City ID
      city_id = '',
      -- City ID
      city_coords = { 51.77501, 9.38155 },
      -- Units
      units = 'metric',
      -- Update in N seconds
      update_interval = 60 * 60
    },
    network = {
      -- Wired interface
      wired_interface = 'eno1',
      -- Wireless interface
      -- wireless_interface = 'wlan0'
    },
    clock = {
      -- Clock widget format
      military_mode = true
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
    },
    harddrives = {
      mounts = {
        '/',
        '/home',
      }
    },
    seperator = {
      enabled = true
    },
    left_widgets = {
      -- all screens
      {
        'layout_box',
        'fire_icon',
        'cpu',
        'ram',
      },
      -- screen 1
      {
      },
      -- screen 2
      {
        'color_palette',
        'net_icon',
        'net_speed',
        'media_icon',
        'spotify',
      }
    },
    center_widgets = {
      -- all screens
      {
      },
      -- screen 1
      {
        -- 'weather',
      },
      -- screen 2
      {
      }
    },
    right_widgets = {
      -- all screens
      {
        'screen_rec',
      },
      -- screen 1
      {
        'updater',
        'hard_drives',
        'systray',
        'clock_icon',
        'clock',
        'info_center_toggle',
      },
      -- screen 2
      {
        'clock_icon',
        'clock',
      }
    },
    module = {
      auto_start = {
        -- Will create notification if true
        debug_mode = true
      },
      dynamic_wallpaper = {
        -- Will look for wallpapers here
        wall_dir = 'theme/wallpapers/',
        -- Image formats
        valid_picture_formats = { 'jpg', 'png', 'jpeg' },
        -- Leave this table empty for full auto scheduling
        wallpaper_schedule = {
          -- ['00:00:00'] = 'midnight-wallpaper.jpg',
          -- ['06:22:00'] = 'morning-wallpaper.jpg',
          -- ['12:00:00'] = 'noon-wallpaper.jpg',
          -- ['17:58:00'] = 'night-wallpaper.jpg',
        }
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
  }
}

return config

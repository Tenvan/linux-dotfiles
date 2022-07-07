log('Enter Module => ' .. ...)

local gfs = require("gears.filesystem")

local lgi = require("lgi")
local glib = lgi.GLib

local theme = require("beautiful")

local unpack = unpack or table.unpack -- luacheck: globals unpack (compatibility with Lua 5.1)

local all_icon_sizes = {
  'scalable',
  '128x128',
  '96x96',
  '72x72',
  '64x64',
  '48x48',
  '36x36',
  '32x32',
  '24x24',
  '22x22',
  '16x16'
}

--- List of supported icon exts.
local supported_icon_file_exts = { png = 1, xpm = 2, svg = 3 }

local icon_lookup_path = nil

--- Get a list of icon lookup paths.
-- @treturn table A list of directories, without trailing slash.
local function get_icon_lookup_path()
  if icon_lookup_path then return icon_lookup_path end

  local function ensure_args(t, paths)
      if type(paths) == 'string' then paths = { paths } end
      return t or {}, paths
  end

  local function add_if_readable(t, paths)
      t, paths = ensure_args(t, paths)

      for _, path in ipairs(paths) do
          if gfs.dir_readable(path) then
              table.insert(t, path)
          end
      end
      return t
  end

  local function add_with_dir(t, paths, dir)
      t, paths = ensure_args(t, paths)
      dir = { nil, dir }

      for _, path in ipairs(paths) do
          dir[1] = path
          table.insert(t, glib.build_filenamev(dir))
      end
      return t
  end

  icon_lookup_path = {}
  local theme_priority = { 'hicolor' }
  if theme.icon_theme then table.insert(theme_priority, 1, theme.icon_theme) end

  local paths = add_with_dir({}, glib.get_home_dir(), '.icons')
  add_with_dir(paths, {
      glib.get_user_data_dir(),           -- $XDG_DATA_HOME, typically $HOME/.local/share
      unpack(glib.get_system_data_dirs()) -- $XDG_DATA_DIRS, typically /usr/{,local/}share
  }, 'icons')
  add_with_dir(paths, glib.get_system_data_dirs(), 'pixmaps')

  local icon_theme_paths = {}
  for _, theme_dir in ipairs(theme_priority) do
      add_if_readable(icon_theme_paths,
                      add_with_dir({}, paths, theme_dir))
  end

  local app_in_theme_paths = {}
  for _, icon_theme_directory in ipairs(icon_theme_paths) do
      for _, size in ipairs(all_icon_sizes) do
          table.insert(app_in_theme_paths,
                       glib.build_filenamev({ icon_theme_directory,
                                              size, 'apps' }))
          table.insert(app_in_theme_paths,
                       glib.build_filenamev({ icon_theme_directory,
                                              size, 'categories' }))
      end
  end
  add_if_readable(icon_lookup_path, app_in_theme_paths)

  return add_if_readable(icon_lookup_path, paths)
end

local path = get_icon_lookup_path()
dump(path, "icon lookup path")

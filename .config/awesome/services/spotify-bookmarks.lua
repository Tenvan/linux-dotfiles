log('Enter Module => ' .. ...)

local metaHelper = require('module.spotify')

local file = require('utilities.file')
local json = require('library.json')

local currentStatus = metaHelper.stoppedText
local currentBookmarks = {}

local function getBookmarksPath()
  local statePath = string.format('%s/.local/state/awesome/', os.getenv('HOME'))
  file.ensureDir(statePath)
  local stateFile = statePath .. 'bookmarks-state.json'
  return stateFile
end

-- bookmark managment
local function mergeMetaToBookmarks(meta)
  for i = 1, #currentBookmarks do
    local bookmark = currentBookmarks[i]
    if bookmark.album == meta.album then
      currentBookmarks[i] = meta
      -- dump(bookmarks, 'replaced bookmarks', 1)
      return
    end
  end

  table.insert(currentBookmarks, meta)
  -- dump(bookmarks, 'added bookmark', 1)
end

local function removeMetaToBookmarks(meta)
  for i = 1, #currentBookmarks do
    local bookmark = currentBookmarks[i]
    if bookmark.trackid == meta.trackid then
      table.remove(currentBookmarks, i)
      return
    end
  end
end

local function saveBookmarksToState()
  local bookmarksStr = json.stringify(currentBookmarks)
  local bookmarksFile = getBookmarksPath()
  file.write_file(bookmarksFile, bookmarksStr)
end

local function loadBookmarksFromState()
  local bookmarksFile = getBookmarksPath()
  local bookmarksStr = file.read_file(bookmarksFile)
  local bookmarks = json.parse(bookmarksStr)
  currentBookmarks = bookmarks

  for i = 1, #bookmarks do
    metaHelper.ValidateImage(bookmarks[i])
  end
end

-- emitter functions
local function emit_status(status)
  emit('service::spotify::status', status)
end

local function emit_bookmarks(bookmarks)
  table.sort(bookmarks, function(a, b)
    return (a.lastPlayDate or 0) > (b.lastPlayDate or 0)
  end)
  emit('service::spotify::bookmarks', bookmarks)
end

-- service subcriber
connect('service::spotify::meta', function(meta)
  local newStatus = metaHelper.GetStatus(meta)
  if (currentStatus ~= newStatus) then
    currentStatus = newStatus
    emit_status(currentStatus)
  end

  -- bookmarks workflow
  mergeMetaToBookmarks(meta)
  saveBookmarksToState()

  emit_bookmarks(currentBookmarks)
end)

connect('service::spotify::bookmarks::remove', function(meta)
  -- bookmarks workflow
  removeMetaToBookmarks(meta)
  emit_bookmarks(currentBookmarks)
  saveBookmarksToState()
end)

loadBookmarksFromState()
emit_bookmarks(currentBookmarks)

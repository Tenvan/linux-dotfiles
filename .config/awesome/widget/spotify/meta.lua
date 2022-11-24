log('Enter Module => ' .. ...)

local pausedText = ''
local stoppedText = ''
local playingText = ''

local emptyMetaData = {
  artist = '--',
  title = '--',
  status = stoppedText,
  discNumber = '--',
  trackNumber = '--',
  album = '--',
  url = '--',
  trackid = '--',
  artUrl = '--',
}

local file = require('utilities.file')

local function getTrackId(meta)
  local trackIdPart = gears.string.split(meta.trackid, '/')
  return trackIdPart[5]
end

local function getLargeMetaText(meta)
  return '<span size="x-large">Titel: ' .. meta.title .. '</span>\n' ..
    '<b>Künstler</b>: ' .. meta.artist .. '\n' ..
    '<b>Album</b>: ' .. meta.album .. '\n' ..
    '<b>Disk</b>: ' .. meta.discNumber .. '\n' ..
    '<b>Track</b>: ' .. meta.trackNumber .. '\n' ..
    '<b>Track Link</b>: ' .. meta.url .. '\n'
end

local function getSmallMetaText(meta)
  return string.format([[<b>Album</b>: <span size="large">%s</span>
<b>Titel</b>: %s
<b>Disk</b> %s / <b>Track</b> %s]],
    meta.album,
    meta.title,
    meta.discNumber,
    meta.trackNumber)
end

local function getCachePath()
  local cacheDirSpotify = string.format('%s/.cache/awesome/spotify', os.getenv('HOME'))
  log('image cache dir ==> ' .. cacheDirSpotify)

  return cacheDirSpotify
end

---comment build cached art-imagepath from metadata
---@param meta any
---@return string
local function getImagePath(meta)
  local trackid = getTrackId(meta)
  local cacheDirSpotify = getCachePath()
  local artCacheFile = string.format('%s/%s.jpeg', cacheDirSpotify, trackid)
  log('image cache file --> ' .. artCacheFile)

  return artCacheFile
end

---comment process meta data cache
---@param meta any
local function cacheMetaData(meta)
  local artCacheFile = getImagePath(meta)
  log('image cache file ===> ' .. artCacheFile)

  local get_art_script = string.format('curl -sf %s --output %s', meta.artUrl, artCacheFile)
  -- awful.spawn.easy_async_with_shell(get_art_script, function()
  --   log("link '" .. meta.artUrl .. "' fetched")
  -- end)
  awful.spawn.with_shell(get_art_script)
end

---comment validates existing of imagefile. 
--- if not, then request it again
---
---@param meta any
---@return string
local function validateImage(meta)
  local artCacheFile = getImagePath(meta)
  if not file.file_exists(artCacheFile) then
    cacheMetaData(meta)
  end

  return artCacheFile
end

local result = {
  GetTrackId = getTrackId,
  GetImagePath = getImagePath,
  GetCachePath = getCachePath,
  GetLargeMetaText = getLargeMetaText,
  GetSmallMetaText = getSmallMetaText,
  CacheMetaData = cacheMetaData,
  ValidateImage = validateImage,

  emptyMetaData = emptyMetaData,
  pausedText = pausedText,
  stoppedText = stoppedText,
  playingText = playingText,
}

return result

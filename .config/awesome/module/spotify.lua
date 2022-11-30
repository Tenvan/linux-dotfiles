log('Enter Module => ' .. ...)

local pausedText = ''
local stoppedText = ''
local playingText = ''
local prevText = ''
local nextText = ''

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
  return string.format(
    [[<b>Titel</b>: <span size="x-large">%s</span>
<b>Künstler</b>: %s
<b>Album</b>: %s
<b>Disk</b>: %s
<b>Track</b>: %s
<b>Track Link</b>: %s
<b>Zuletzt gespielt</b>: <span size="large">%s</span>]],
    meta.title,
    meta.artist,
    meta.album,
    meta.discNumber,
    meta.trackNumber,
    meta.url,
    os.date('%a %b %d, %H:%M ', meta.lastPlayDate or os.time()))
end

local function getSmallMetaText(meta)
  return string.format(
    [[<b>Titel</b>: <span size="large">%s</span>
<b>Album</b>: %s
<b>Disk</b> %s / <b>Track</b> %s
<b>Zuletzt gespielt</b>: <span size="large">%s</span>]],
    meta.album,
    meta.title,
    meta.discNumber,
    meta.trackNumber,
    os.date('%a %b %d, %H:%M ', meta.lastPlayDate or os.time()))
end

local function getCachePath()
  local cacheDirSpotify = string.format('%s/.cache/awesome/spotify', os.getenv('HOME'))
  return cacheDirSpotify
end

---comment build cached art-imagepath from metadata
---@param meta any
---@return string
local function getImagePath(meta)
  local trackid = getTrackId(meta)
  local cacheDirSpotify = getCachePath()
  local artCacheFile = string.format('%s/%s.jpeg', cacheDirSpotify, trackid)
  return artCacheFile
end

local function getStatus(meta)
  local status = stoppedText
  if meta.status == 'Paused' then
    status = playingText
  elseif meta.status == 'Playing' then
    status = pausedText
  end

  return status
end

---comment process meta data cache
---@param meta any
local function cacheMetaImage(meta)
  local artCacheFile = getImagePath(meta)
  local get_art_script = string.format('curl -sf %s --output %s', meta.artUrl, artCacheFile)

  awful.spawn.easy_async_with_shell(get_art_script, function()
    log('image file received: ' .. artCacheFile)
    emit('service::spotify::image', artCacheFile)
  end)
end

---comment validates existing of imagefile.
--- if not, then request it again
---
---@param meta any
---@return string
local function validateImage(meta)
  local artCacheFile = getImagePath(meta)

  if not file.file_exists(artCacheFile) then
    cacheMetaImage(meta)
  end

  return artCacheFile
end

local result = {
  CacheMetaImage = cacheMetaImage,
  GetCachePath = getCachePath,
  GetImagePath = getImagePath,
  GetLargeMetaText = getLargeMetaText,
  GetSmallMetaText = getSmallMetaText,
  GetStatus = getStatus,
  GetTrackId = getTrackId,
  ValidateImage = validateImage,

  emptyMetaData = emptyMetaData,
  nextText = nextText,
  pausedText = pausedText,
  playingText = playingText,
  prevText = prevText,
  stoppedText = stoppedText,
}

return result

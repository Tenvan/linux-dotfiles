local log = require('utilities.debug').log
log("Enter Module => configuration/init.lua" )

return {
	keys = require('configuration.keys'),
  	apps = require('configuration.apps')
}

Logger = (@context)->
defaultLevel = process.env.LOG_LEVEL
defaultLevel ?= 1
module.exports = logger =
  DEBUG: 0
  INFO: 1
  WARN: 2
  ERROR: 3
  logLevel: defaultLevel
  log: (msg,level = logger.INFO) ->
    if level >= @logLevel
      prefix = if @context then "#{@context}: " else ""
      console.log prefix + msg
  context: false
  forContext: (context) ->
    new Logger(context)
  dir: (obj) ->
    console.dir obj

["debug","info","warn","error"].forEach (level) ->
  logger[level] = (msg) ->
    logger.log msg, logger[level.toUpperCase()]
Logger.prototype = logger

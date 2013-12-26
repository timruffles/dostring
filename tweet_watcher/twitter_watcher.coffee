logger = require("./logger").forContext("TweetWatcher")
_ = require "underscore"

# https://dev.twitter.com/docs/streaming-api/concepts
class TweetWatcher extends require("events").EventEmitter
  constructor: (@twit,@redis) ->
    this

  makeStream: (keywords, established) ->
    twitterEvents = this
    onEstablished = _.once established
    if keywords.length == 0
      logger.log("Nothing to search for, connection contract fulfilled by not connecting")
      return onEstablished()
    @twit.stream "statuses/filter", {track: keywords}, (stream) =>
      logger.log "Connection established, tracking '#{keywords}'"
      stream.on "data", (data) =>
        # twdocs: clear connected on first response
        onEstablished()
        # tweet IDs are too long for JS, need to use the string everywhere
        logger.debug "Tweet received, #{data.id}, #{data.id_str} #{data.text}"
        data.id = data.id_str
        twitterEvents.emit("tweet",data)
      stream.on "end", (evt) =>
        logger.error "Tweet stream ended with #{evt.statusCode}"
        logger.error "Is the system clock set correctly? #{new Date().toString()} OAuth can fail if it's not" if evt.statusCode == 401
        @potentialLimiting()
      stream.on "error", (evt) =>
        logger.error "ERROR on tweet stream"
        logger.dir arguments
        @networkError()
      stream.on "destroy", =>
        logger.error "Tweet stream destroyed"
        logger.dir arguments
        @potentialLimiting()

  connectionIssues: []
  networkError: ->
    limitingIssues = @connectionIssues.filter (i) -> i == "networkError"
    # twdocks: back off linearly. Perhaps start at 250 milliseconds and cap at 16 seconds
    delay = Math.min(16 * seconds,(limitingIssues + 1) * 250)
    @connectionIssues.push "networkError"
    @reconnect(new Date + delay)
  potentialLimiting: ->
    limitingIssues = @connectionIssues.filter (i) -> i == "potentialLimiting"
    # twdocs: back off exponentially. Perhaps start with a 10 second wait, double on each subsequent failure, and finally cap the wait at 240 seconds
    delay = Math.min(240 * seconds,Math.pow(2,limitingIssues) * 10 * seconds)
    @connectionIssues.push "potentialLimiting"
    @reconnect(new Date + delay)

  reconnect: =>
    lazyDefine this, "reconnect", afterLongestDelay =>
      logger.log "Reconnecting to stream"
      @_connect()

  connect: (keywords) =>
    logger.log "Connection request with #{keywords}"
    return if keywords.length == 0
    @desiredKeywords = keywords
    @_connect()
  # twdocs: reconnect no more than twice every four minutes
  _connect: =>
    lazyDefine this, "_connect", _.throttle(( =>
      # load keywords, establish stream
      @makeStream @desiredKeywords, (newStream) =>
        logger.log "First data on new connection, it's working, removing any old connections"
        @connectionIssues = []
        if @stream
          @stream.removeAllListeners("end")
          @stream.removeAllListeners("destroy")
          @stream.destroy()
        @stream = newStream
        connected = true
    ), 120 * seconds)

afterLongestDelay = (delay,fn) ->
  currentDelay = 0
  timeout = false
  ->
    return if currentDelay > +new Date + delay
    currentDelay = +new Date + delay
    clearTimeout timeout
    timeout = setTimeout fn, delay
lazyDefine = (ctx,as,fn) ->
  ctx[as] = fn
  fn()

seconds = 1000

exports.TwitterWatcher = TweetWatcher

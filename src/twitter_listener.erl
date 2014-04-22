-module(twitter_listener).
-behaviour(supervisor_bridge).

-export([listen/0]).
-export([init/1,terminate/2]).

% PUBLIC

listen() ->
  % uses register/2 to register the process
  supervisor_bridge:start_link({local, ?MODULE}, ?MODULE, []).

% CALLBACKS

init([]) ->
  {ok,spawn(fun tweet_loop/0),unused_state}.

terminate(Reason,Pid) ->
  exit(Pid,Reason).

% PRIVATE

tweet_loop() ->
  timer:sleep(500),
  Tweet = tweets_in:get_tweet(),
  tweet_handler:run(Tweet),
  tweet_loop().


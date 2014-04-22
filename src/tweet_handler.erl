-module(tweet_handler).
-behaviour(supervisor_bridge).

-export([run/1]).
-export([init/1,terminate/2]).

% PUBLIC

run(Tweet) ->
  supervisor_bridge:start_link({local, ?MODULE}, ?MODULE, Tweet).

% CALLBACKS

init(Tweet) ->
  Run = spawn(fun() ->
    handle_tweet(Tweet),
    exit(normal)
  end),
  {ok,Run,unused}.

terminate(Reason,Pid) ->
  exit(Pid,Reason).

% PRIVATE

handle_tweet(Tweet) ->
  State = tw_store:tweet_state(Tweet),
  Reply = tweet_response:for_tweet(Tweet,State),
  case Reply of
    {tweet,T} ->
      send(T);
    _Ignored ->
      ignored
  end.

send(Tweet) ->
  case tweets_out:send_tweet(Tweet) of
    ok ->
      io:format("TOTALLY WORKING\n"),
      ok;
    Err ->
      erlang:error(Err)
  end.


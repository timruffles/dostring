-module(habitpop_app).

-include("../include/habitpop_records.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_StartType, _StartArgs) ->
  {ok, Redis} = eredis:start_link(),
  spawn_link(fun () ->
    twitter_in:startup(fun (Tweet) ->
      State = habitpop_store:on_tweet(Redis,Tweet),
      Reply = tweet_handler:on_tweet(State),
      case Reply of
        undefined ->
          ok;
        T ->
          twitter_out:send(T)
      end
    end)
  end),
  habitpop_sup:start_link().

stop(_State) ->
    ok.

habitpop_test () ->
  MakeTweet = fun () ->
    "{\"id\":\"416297496374431745\",\"text\":\"@lq_feed Hi, you mention #programming and this img for you ;) #ruby http://t.co/J9xym5vimW\",\"created_at\":\"Thu Dec 26 20:00:32 +0000 2013\",\"hashtags\":[\"programming\",\"ruby\"],\"user_id\":\"504203528\",\"screen_name\":\"DebianBleat\"}"
  end,
  application:start(habitpop),
  {ok, Redis} = eredis:start_link(),
  ok = eredis:q(Redis,["PUBLISH","tweets",MakeTweet()]).



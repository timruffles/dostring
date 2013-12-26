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
  spawn_link(fun () ->
    twitter_in:listen(fun tweet_handler:on_tweet/1)
  end),
  habitpop_sup:start_link().

stop(_State) ->
    ok.


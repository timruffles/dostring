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
  spawn(fun twitter_in:listen/0),
  habitpop_sup:start_link().

stop(_State) ->
    ok.


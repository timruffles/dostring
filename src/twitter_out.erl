-module(twitter_out).

-include("../include/habitpop_records.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([send/1]).

send (Tweet) ->
  io:format("tweet sent '~s'~n",[Tweet]),
  ok.

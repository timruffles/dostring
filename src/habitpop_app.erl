-module(habitpop_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  inets:start(),
  ssl:start(),
  io:format("Ok let's get some tweets~n"),
  spawn(fun go_tweet/0),
  habitpop_sup:start_link().

go_tweet() ->
  Headers = {os:getenv("TW_C_KEY"), os:getenv("TW_C_SECRET"),os:getenv("TW_T_KEY"), os:getenv("TW_T_SECRET")},
  {ok, Params} =  stream_client_util:keywords_to_track(["justin"]),
  Over = stream_client:connect(stream_client_util:filter_url(), Headers, Params, fun(Data) ->
    Tweet = proplists:get_value(<<"text">>, Data),
    io:format("Erlang <3: ~s~n", [Tweet])
  end),
  case Over of
    {ok, x} -> io:format("Over and out~p~n",[x]);
    {error, x} -> io:format("error: ~p~n",[x])
  end,
  io:format("Done listening..~n").



% on tweet ->
% existing user?
%   existing habit?
%     add
%     check if they're due an event (milestone etc)
%   else
%     say hi
% else
%   intro message

%% questions:
% - do hashtags get parsed out already?
% - do mentions?
% - need to subscribe for @ mentions

stop(_State) ->
    ok.


-module(habitpop_app).

-include_lib("eunit/include/eunit.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-record(tweet,{username,hashtags,text}).

start(_StartType, _StartArgs) ->
  {ok, Redis} = eredis:start_link(),
  io:format("Ok let's get some tweets~n"),
  habitpop_sup:start_link().


on_tweet(Redis,Tweet,Notifier) ->
  {ok,Is} = eredis:q(Redis,["ISMEMBER","users",Tweet#tweet.username]),
  case Is of
      <<"0">> ->
        Notifier ! {newuser,Tweet};
      <<"1">> ->
        ok
  end,
  HabitEventsCb = fun (Hashtag) ->
    {ok,Is} = eredis:q(Redis,["ISMEMBER",string:join([Tweet#tweet.username,"|","habits"]),Hashtag]),
    case Is of
      <<"0">> ->
        {newhabit,Hashtag,Tweet};
      <<"1">> ->
        {oldhabit,Hashtag,Tweet}
    end
  end,
  Events = list:filtermap(HabitEventsCb, Tweet#tweet.hashtags),
  list:foreach(fun (Event) -> Notifier ! Event end,Events).
   

on_tweet_test() ->
  false = true.

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


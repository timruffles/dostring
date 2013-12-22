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
  Events = spawn(handle_event),
  io:format("Ok let's get some tweets~n"),
  habitpop_sup:start_link().


on_tweet(Redis,Tweet,Notifier) ->
  {ok,Is} = eredis:q(Redis,["ISMEMBER","users",Tweet#tweet.username]),
  case Is of
      <<"0">> ->
        Notifier ! {newuser,[Tweet]};
      <<"1">> ->
        ok
  end,
  HabitEventsCb = fun (Hashtag) ->
    {ok,Is} = eredis:q(Redis,["ISMEMBER",string:join([Tweet#tweet.username,"|","habits"]),Hashtag]),
    case Is of
      <<"0">> ->
        Notifier ! {newhabit,[Hashtag,Tweet]};
      <<"1">> ->
        Notifier ! {oldhabit,[Hashtag,Tweet]}
    end
  end,
  list:foreach(HabitEventsCb, Tweet#tweet.hashtags),
   
new_user_message(Tweet) ->
  io_lib:format("~s welcome! Tweet @habitadd when you perform your habit, and we'll track your progress. Try it now",Tweet#tweet.username).

handle_event() ->
  receive E of
    {newuser,[T]} ->
      handle_new_user(T),
      handle_event();
    {newhabit,[H,T]} ->
      handle_new_habit(H,T),
      handle_event();
    {newhabit,[H,T]} ->
      handle_old_habit(H,T),
      handle_event()
  end.

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


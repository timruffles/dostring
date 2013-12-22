-module(habitpop_app).

-include_lib("eunit/include/eunit.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-record(tweet,{username,hashtags,text}).
-record(tweet_state,{username,habits_with_status,signedup_at}).
-record(habit_status,{streak_days,latest_days_ago,total}).

start(_StartType, _StartArgs) ->
  {ok, Redis} = eredis:start_link(),
  Events = spawn(handle_event),
  io:format("Ok let's get some tweets~n"),
  habitpop_sup:start_link().

load_tweet_state(Redis,Tweet,Notifier) ->
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

on_tweet (State) ->
  % #tweet_state{username=Username,habits_with_status=Habits,signedup_at=SignupAt} = State,
  list:flatten([handle_age(State),handle_habits(State)).

handle_habits (State) ->
  if 
    [] = State#tweet_state.habits_with_status ->
      no_habit_supplied(H,State);
    [H] = State#tweet_state.habits_with_status ->
      single_habit(H,State);
    true ->
      multiple_habits(State)
  end.

habit_event (#habit_status{streak_days=Streak,latest_days_ago=LastDays,total=0}) ->
  new_habit.

habit_event (#habit_status{streak_days=Streak,latest_days_ago=0,total=N}) ->
  same_day.

habit_event (#habit_status{streak_days=Streak,latest_days_ago=1,total=N}) ->
  continued_streak.

habit_event (#habit_status{streak_days=Streak,latest_days_ago=X,total=N}) ->
  broke_streak.


multiple_habits (State) ->
  .
   
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

handle_new_user (Tweet) ->
  TweetSender ! {tweet,

on_tweet_test() ->
  false = true.

% on tweet ->
% existing user?
%   existing habit?
%     add
%     check if they're due an event (milestone etc)
%       increment and check date
%       chain broken?
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


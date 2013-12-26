-module(tweet_handler).

-include_lib("eunit/include/eunit.hrl").
-include("../include/habitpop_records.hrl").

-export([on_tweet/1]).

on_tweet (State) ->
  % #tweet_state{username=Username,habits_with_status=Habits,signedup_at=SignupAt} = State,
  io:format("tweet_handler: ~p~n",[State]),
  ok.

handle_habits (State) ->
  case State#tweet_state.habits_with_status of 
    [] ->
      no_habit_supplied(State);
    [H] ->
      single_habit(H,State);
    Otherwise ->
      multiple_habits(State)
  end.

habit_event (#habit_status{total=0}) ->
  new_habit;
habit_event (#habit_status{latest_days_ago=0}) ->
  same_day;
habit_event (#habit_status{latest_days_ago=1}) ->
  continued_streak;
habit_event (State) ->
  broke_streak.

single_habit_message(new_habit,Habit,_S,Username) ->
  io_lib:format("@~s great start on your #~s habit!",[Username,Habit]);
single_habit_message(same_day,Habit,#habit_status{today_total=Today},Username) ->
  io_lib:format("@~s wow - hitting the #~s habit ~i times today",[Username,Habit,Today]);
single_habit_message(continued_streak,Habit,#habit_status{streak_days=Streak},Username) ->
  % TODO int to words
  ok;
single_habit_message(broke_streak,Habit,#habit_status{streak_days=Streak},Username) ->
  % TODO int to words
  ok.


no_habit_supplied(State) ->
  ok.

single_habit(Habit,State) ->
  Event = habit_event(Habit),
  single_habit_message(Event,Habit,State,State#tweet_state.username).

multiple_habits (State) ->
  ok.

handle_old_habit (Habit,State) ->
  ok.

handle_new_habit (Habit,State) ->
  ok.
   
new_user_message(Tweet) ->
  io_lib:format("~s welcome! Tweet @habitadd when you perform your habit, and we'll track your progress. Try it now",Tweet#tweet.username).

handle_event() ->
  receive
    {newuser,[T]} ->
      handle_new_user(T),
      handle_event();
    {newhabit,[H,T]} ->
      handle_new_habit(H,T),
      handle_event();
    {oldhabit,[H,T]} ->
      handle_old_habit(H,T),
      handle_event()
  end.

handle_new_user (Tweet) ->
  % TweetSender ! {tweet}.
  ok.

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


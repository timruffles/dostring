-module(tweet_response).

%% PUBLIC API
-export([for_tweet/2]).

-include_lib("eunit/include/eunit.hrl").
-include("../include/habitpop_records.hrl").

%% PUBLIC API

for_tweet (_Tweet,State) ->
  case State#tweet_state.signedup_at of
    newuser ->
      {tweet,handle_new_user(State)};
    _Otherwise ->
      {tweet,handle_habits(State)}
  end.

%% PRIVATE

handle_new_user (Tweet) ->
  io_lib:format("@~s welcome! great start on your habits. keep tweeting to track",[Tweet#tweet_state.username]).

handle_habits (State) ->
  case State#tweet_state.habits_with_status of 
    [] ->
      no_habit_supplied(State);
    [H] ->
      single_habit(H,State);
    _Otherwise ->
      multiple_habits(State)
  end.

single_habit(Habit,State) ->
  Event = habit_event(Habit),
  single_habit_message(Event,Habit,State,State#tweet_state.username).

no_habit_supplied(#tweet_state{username=Username}) ->
  % TODO actually probably do nothing
  io_lib:format("@~s didn't understand that - you need hashtag habits to track, e.g #gym",[Username]).

habit_event (new_habit) ->
  new_habit;
habit_event (#habit_status{broke_streak=true}) ->
  broke_streak;
habit_event (_State) ->
  continued_streak.

single_habit_message(new_habit,Habit,_S,Username) ->
  io_lib:format("@~s great start on your #~s habit! keep tweeting to track it",[Username,Habit]);
single_habit_message(continued_streak,Habit,_s,Username) ->
  io_lib:format("@~s your #~s habit is going well",[Username,Habit]);
single_habit_message(broke_streak,Habit,_s,Username) ->
  io_lib:format("@~s oh no, your #~s streak is broken! you'll do better next time",[Username,Habit]).

multiple_habits (State) ->
  {New,Old} = lists:partition(fun ({_,Age}) -> case Age of new_habit -> true; _O -> false end end,State#tweet_state.habits_with_status),
  {Kept,Broken} = lists:partition(fun ({_,#habit_status{broke_streak=BS}}) -> BS =:= false end,Old),
  multiple_habit_message(New,Kept,Broken,State#tweet_state.username,State).

multiple_habit_message ([],[],[_S|BS],Username,_State) ->
  io_lib:format("@~s keep it up - ~p habits at once is a challenge",[Username,length(BS) + 1]);
multiple_habit_message ([],[HabitA,HabitB],[],Username,_State) ->
  io_lib:format("@~s nice one doubling up on your ~s and ~s habits",[Username,HabitA,HabitB]);
multiple_habit_message ([NewHabit],[OldHabit],[],Username,_State) ->
  io_lib:format("@~s excellent start on your ~s habit, and continuing ~s",[Username,NewHabit,OldHabit]);
multiple_habit_message ([],[_OldHabit|_OtherHabits],[],Username,_State) ->
  io_lib:format("@~s habit combo!",[Username]);
multiple_habit_message ([_NewHabit,_NewHabitB],[],[],Username,_State) ->
  io_lib:format("@~s two new habits at once? brave",[Username]);
multiple_habit_message ([_NewHabit|NewHabits],[],[],Username,_State) ->
  io_lib:format("@~s ~i new habits at once? good luck!",[Username,length(NewHabits) + 1]);
multiple_habit_message (_NewHabits,_OldHabits,_BrokenStreaks,Username,_State) ->
  io_lib:format("@~s keep it up!",[Username]).

%% TESTS

tweet_response_test () ->
  {tweet,T} = for_tweet("foo",#tweet_state{username="tim",habits_with_status=[],signedup_at=newuser}),
  ?assertMatch("@tim" ++ _Rest,T).



-module(tweet_response).

%% PUBLIC API
-export([for_tweet/2]).

-include_lib("eunit/include/eunit.hrl").
-include("../include/habitpop_records.hrl").

-type habit_state_name() :: new_habit | continued_streak | broke_streak.

%% PUBLIC API
-spec for_tweet(any(),#tweet_state{}) -> {atom(),string()}.
for_tweet (_Tweet,State) ->
  case State#tweet_state.signedup_at of
    newuser ->
      {tweet,handle_new_user(State)};
    _Otherwise ->
      {tweet,handle_habits(State)}
  end.

%% PRIVATE
-spec handle_new_user(#tweet_state{}) -> string().
handle_new_user (Tweet) ->
  io_lib:format("@~s welcome! great start on your habits. keep tweeting to track",[Tweet#tweet_state.username]).

-spec handle_habits(#tweet_state{}) -> string().
handle_habits (State) ->
  case State#tweet_state.habits_with_status of 
    [] ->
      no_habit_supplied(State);
    [H] ->
      single_habit(H,State#tweet_state.username);
    _Otherwise ->
      multiple_habits(State)
  end.

-spec no_habit_supplied(#tweet_state{}) -> string().
no_habit_supplied(#tweet_state{username=Username}) ->
  % TODO actually probably do nothing
  io_lib:format("@~s didn't understand that - you need hashtag habits to track, e.g #gym",[Username]).

-spec single_habit(habit_status_pair(),string()) -> string().
single_habit({Habit,#habit_status{new_habit=true}},Username) ->
  io_lib:format("@~s great start on your #~s habit! keep tweeting to track it",[Username,Habit]);
single_habit({Habit,#habit_status{broke_streak=false,count_today=1}},Username) ->
  io_lib:format("@~s keeping the #~s string growing",[Username,Habit]);
single_habit({Habit,#habit_status{broke_streak=false,count_today=3}},Username) ->
  io_lib:format("@~s #~s hat-trick!",[Username,Habit]);
single_habit({Habit,#habit_status{broke_streak=true,previous_streak_length=N}},Username) ->
  io_lib:format("@~s new ~s string, last time you managed ~i days",[Username,Habit]).

-spec multiple_habits(#tweet_state{}) -> string().
multiple_habits (State) ->
  {New,Old} = lists:partition(fun ({Name,Hs}) ->
    Hs#habit_status.new_habit
  end,
  State#tweet_state.habits_with_status),
{Broken,Kept} = lists:partition(fun ({_,#habit_status{broke_streak=Bs}}) -> Bs end,Old),
  multiple_habit_message(New,Kept,Broken,State#tweet_state.username,State).

-spec multiple_habit_message([habit_status_pair()],[habit_status_pair()],[habit_status_pair()],string(),#tweet_state{}) -> string().
multiple_habit_message ([],[],[_S|BS],Username,_State) ->
  io_lib:format("@~s keep it up - ~p habits at once is a challenge",[Username,length(BS) + 1]);
multiple_habit_message ([],[{HabitA,_},{HabitB,_}],[],Username,_State) ->
  io_lib:format("@~s nice one doubling up on your ~s and ~s habits",[Username,HabitA,HabitB]);
multiple_habit_message ([{NewHabit,_}],[{OldHabit,_}],[],Username,_State) ->
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
  ?assertMatch("@tim" ++ _Rest,lists:flatten(T)).



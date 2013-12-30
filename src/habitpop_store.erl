-module(habitpop_store).

-include("../include/habitpop_records.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% pg
%% CREATE TABLE habit_instance ( user_id INT NOT NULL, habit_id varchar(150) NOT NULL, text varchar(150) NOT NULL, happened_at TIMESTAMP NOT NULL );
%% CREATE TABLE user ( id SERIAL, twitter_id bigint NOT NULL, username varchar(50) NOT NULL );

%%% redis
% user$USERNAME -> hash with general fields
% USERNAME$habits -> HASH of streaks -> [10bitday,6bitCount]

-export([on_tweet/2]).

% takes in a tweet, and gets the current state from db
on_tweet(Redis,Tweet) ->
  tweet_events(Redis,Tweet).

tweet_events(Redis,#tweet{gregorian_seconds=CreatedAt} = Tweet) ->
  {ok,Date} = eredis:q(Redis,["HGET",user_key(Tweet),"signedup_at"]),
  SignupAt = case Date of
    undefined ->
      ok = persist_signup_event (newuser,Tweet,Redis),
      newuser;
    Seconds ->
      to_int(Seconds) - calendar:datetime_to_gregorian_seconds(calendar:universal_time())
  end,
  SecondsBin = <<CreatedAt:32>>,
  HabitStatusCb = fun (HabitName) ->
    {ok,Is} = eredis:q(Redis,["HGET",habits_key(Tweet),HabitName]),
    case Is of
      undefined ->
        {ok,_} = eredis:q(Redis,["HSET",habits_key(Tweet),HabitName,SecondsBin]),
        {HabitName,new_habit};
      Items ->
        <<MostRecent:32,Rest/binary>> = Items,
        PreviousSeconds = to_int(MostRecent),
        DiffDays = streak_day_diff(CreatedAt,PreviousSeconds),
        {Event,New} = case DiffDays of
          0 ->
            {streak,<<SecondsBin/binary,Items/binary>>};
          1 ->
            {broke,<<CreatedAt:32>>}
        end,
        {ok,_} = eredis:q(Redis,["HSET",habits_key(Tweet),HabitName,New]),
        Status = #habit_status{broke_streak=case Event of streak -> false; O -> true end,streak_length=get_streak_length(New)},
        {HabitName,Status}
    end
  end,
  HabitStatus = lists:map(HabitStatusCb,Tweet#tweet.hashtags),
  #tweet_state{username=Tweet#tweet.username,habits_with_status=HabitStatus,signedup_at=SignupAt}.

get_streak_length (B) ->
  byte_size(B) / 4.

persist_signup_event (newuser,#tweet{gregorian_seconds=CreatedAt} = Tweet,Redis) ->
  {ok,_} = eredis:q(Redis,["HSET",user_key(Tweet),"signedup_at",CreatedAt]),
  ok.

user_key (#tweet{user_id=UserId}) ->
  io_lib:format("user$~i",[UserId]).

habits_key (Tweet) ->
  io_lib:format("~s|habits",[user_key(Tweet)]).

to_int (B) when is_binary(B) ->
  io:format("ARG: ~p~n",[B]),
  {Int,[]} = string:to_integer(binary:bin_to_list(B)),
  Int;
to_int (B) when is_integer(B) ->
  B.

% rules:
streak_day_diff (New,Old) ->
  if
    New < Old + 2 * ?DAY -> 0;
    true -> 1
  end.







-module(habitpop_store).

-include("../include/habitpop_records.hrl").
-include_lib("eunit/include/eunit.hrl").



%%% pg
%% CREATE TABLE habit_instance ( user_id INT NOT NULL, habit_id varchar(150) NOT NULL, text varchar(150) NOT NULL, happened_at TIMESTAMP NOT NULL );
%% CREATE TABLE user ( id SERIAL, twitter_id bigint NOT NULL, username varchar(50) NOT NULL );

%%% redis
% user$USERNAME -> hash with general fields
% USERNAME$habits -> HASH of streaks -> [10bitday,6bitCount]

%-record(tweet_state,{username,habits_with_status,signedup_at}).
%-record(habit_status,{streak_days,latest_days_ago,total,today_total}).
%on_tweet(Redis,Tweet,Notifier) ->
%  {ok,Date} = eredis:q(Redis,["HGET",io_lib:format("user$~s",[Tweet#tweet.username]),"signedup_at"]),
%  SignupAt = case Is of
%      undefined ->
%        newuser;
%      Days ->
%        days_from_cache(Days) - calendar:date_to_gregorian_days()
%  end,
%  HabitEventsCb = fun (Hashtag) ->
%    {ok,Is} = eredis:q(Redis,["HGET",string:join([Tweet#tweet.username,"|","habits"]),Hashtag]),
%    case Is of
%      undefined ->
%        new_habit;
%      Habits ->
%        streak_list_from_cache(Habits)
%    end
%  end,
%  handle_habits([]).
%


% rules:
streak_day_diff (New,Old) ->
  if
    New < Old + 2 * ?DAY -> 0;
    true -> 1
  end.







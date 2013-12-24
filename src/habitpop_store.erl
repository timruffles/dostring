-module(habitpop_store).

%%% pg
%% CREATE TABLE habit_instance ( user_id INT NOT NULL, habit_id varchar(150) NOT NULL, text varchar(150) NOT NULL, happened_at TIMESTAMP NOT NULL );
%% CREATE TABLE user ( id SERIAL, twitter_id bigint NOT NULL, username varchar(50) NOT NULL );

%%% redis
% user$USERNAME -> hash with general fields
% USERNAME$habits -> HASH of streaks -> [2bitYear,9bitday,5bitCount]

%-record(tweet_state,{username,habits_with_status,signedup_at}).
%-record(habit_status,{streak_days,latest_days_ago,total,today_total}).
load_tweet_state(Redis,Tweet,Notifier) ->
  {ok,Date} = eredis:q(Redis,["HGET",io_lib:format("user$~s",[Tweet#tweet.username]),"signedup_at"]),
  % TODO date serialisation in redis/erlang
  SignupAt = case Is of
      undefined ->
        newuser;
      Instances ->
  end,
  % TODO questions
  HabitEventsCb = fun (Hashtag) ->
    {ok,Is} = eredis:q(Redis,["HGET",string:join([Tweet#tweet.username,"|","habits"]),Hashtag]),
    case Is of
      undefined ->
        new_habit;
      Habits ->

    end
  end,
  handle_habits([]).


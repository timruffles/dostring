-module(habitpop_store).

-include_lib("eunit/include/eunit.hrl").

-define(BASE_YEAR,2013).

%%% pg
%% CREATE TABLE habit_instance ( user_id INT NOT NULL, habit_id varchar(150) NOT NULL, text varchar(150) NOT NULL, happened_at TIMESTAMP NOT NULL );
%% CREATE TABLE user ( id SERIAL, twitter_id bigint NOT NULL, username varchar(50) NOT NULL );

%%% redis
% user$USERNAME -> hash with general fields
% USERNAME$habits -> HASH of streaks -> [2bitYear,9bitday,5bitCount]

%-record(tweet_state,{username,habits_with_status,signedup_at}).
%-record(habit_status,{streak_days,latest_days_ago,total,today_total}).
% load_tweet_state(Redis,Tweet,Notifier) ->
%   {ok,Date} = eredis:q(Redis,["HGET",io_lib:format("user$~s",[Tweet#tweet.username]),"signedup_at"]),
%   % TODO date serialisation in redis/erlang
%   SignupAt = case Is of
%       undefined ->
%         newuser;
%       Instances ->
%   end,
%   % TODO questions
%   HabitEventsCb = fun (Hashtag) ->
%     {ok,Is} = eredis:q(Redis,["HGET",string:join([Tweet#tweet.username,"|","habits"]),Hashtag]),
%     case Is of
%       undefined ->
%         new_habit;
%       Habits ->
% 
%     end
%   end,
%   handle_habits([]).

% returns year and day pair, in ISO years from BASE_YEAR 
% (only important for checking streaks so all we need is something that sequences correctly)
% NOT for user interface stuff
streak_cache_year_day () ->
  {Date,_} = calendar:universal_time(),
  streak_cache_year_day(Date).
streak_cache_year_day (Date) ->
  {Y,W} = calendar:iso_week_number(Date),
  {Y - ?BASE_YEAR,W*7+calendar:day_of_the_week(Date)}.
  
streak_item_for_cache (Count) ->
  {Y,D} = streak_cache_year_day(),
  <<Y:2,D:9,Count:5>>.

streak_item_from_cache (Count) ->
  {Y,D} = streak_cache_year_day(),
  <<Y:2,D:9,Count:5>>.

continued_streak (A,B) ->
  {Ay,Ad} = A,
  {By,Bd} = B,
  case Ay = By of
    true ->
      Bd - Ad <= 1
    false ->



streak_item_for_cache_test()->
  ?debugFmt("~n~s~n",[streak_item_for_cache(5)]).

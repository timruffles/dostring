-module(dates).
-export([parse_twitter_date/1,streak_day_diff/2]).

-include_lib("eunit/include/eunit.hrl").
-include("../include/habitpop_records.hrl").

-spec month_name_to_erlang_month(string()) -> non_neg_integer().
month_name_to_erlang_month (Name) ->
  case Name of
    "Jan" -> 1;
    "Feb" -> 2;
    "Mar" -> 3;
    "Apr" -> 4;
    "May" -> 5;
    "Jun" -> 6;
    "Jul" -> 7;
    "Aug" -> 8;
    "Sep" -> 9;
    "Oct" -> 10;
    "Nov" -> 11;
    "Dec" -> 12
  end.

% Mon Sep 24 03:35:21 +0000 2012
-spec parse_twitter_date(string()) -> non_neg_integer().
parse_twitter_date (Date) ->
  {_,[_,Month,Day,Hour,Min,Seconds,TimePlus,Timezone,Year],_} = io_lib:fread("~s ~s ~d ~d:~d:~d ~-~d ~d",Date),
  AsDate = {{Year,month_name_to_erlang_month(Month),Day},{Hour,Min,Seconds}},
  calendar:datetime_to_gregorian_seconds(AsDate) + TimePlus * Timezone * ?HOUR.

-spec streak_day_diff(non_neg_integer(),non_neg_integer()) -> non_neg_integer().
streak_day_diff (New,Old) ->
  MoreThanTwoDaysLater = New > Old + 2 * ?DAY,
  if
    MoreThanTwoDaysLater -> 1;
    MoreThanTwoDaysLater == false -> 0
  end.


%
% TESTS
%
streak_day_diff_test () ->
  From = parse_twitter_date("Mon Sep 24 03:35:21 +0000 2012"),
  HabitDates = [
    {"Mon Sep 24 03:35:21 +0000 2012",0},
    {"Mon Sep 24 03:35:22 +0000 2012",0},
    {"Mon Sep 24 08:35:22 +0000 2012",0},
    {"Mon Sep 25 02:35:21 +0000 2012",0},
    {"Mon Sep 25 03:35:22 +0000 2012",0},
    {"Mon Sep 26 03:35:21 +0000 2012",0},
    {"Mon Sep 26 03:35:22 +0000 2012",1},
    {"Mon Oct 01 03:35:22 +0000 2012",1}
  ],
  lists:foreach(fun({DateStr,ExpectedDateDiff}) ->
      Date = parse_twitter_date(DateStr),
      ?assertEqual(streak_day_diff(Date,From),ExpectedDateDiff)
  end,HabitDates).

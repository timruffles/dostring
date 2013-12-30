-module(twitter_in).

-include("../include/habitpop_records.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([startup/1]).

startup (Cb) ->
  {ok, Sub} = eredis_sub:start_link(),
  listen(Sub,Cb).

listen (Sub,Cb) ->
  io:format("Ok let's get some tweets~n"),
  spawn_link(fun () ->
    ok = eredis_sub:controlling_process(Sub),
    ok = eredis_sub:subscribe(Sub, [<<"tweets">>]),
    io:format("subbed ~p~n",[eredis_sub:channels(Sub)]),
    receiver(Sub,Cb),
    io:format("DONE!!~n")
  end).

receiver(Sub,Cb) ->
  receive
    {message,_,TweetText,_} ->
      io:format("got tweet ~p~n",[TweetText]),
      {Tweet} = jiffy:decode(TweetText),
      [
        {<<"id">>,Id},
        {<<"text">>,Text},
        {<<"created_at">>,CreatedAt},
        {<<"hashtags">>,Hashtags},
        {<<"user_id">>,UserId},
        {<<"screen_name">>,ScreenName}
      ] = Tweet,
      Cb(#tweet{
        id=to_int(Id),
        username=ScreenName,
        hashtags=lists:map(fun binary_to_list/1,Hashtags),
        text=Text,
        gregorian_seconds=parse_twitter_date(binary_to_list(CreatedAt)),
        user_id=to_int(UserId)}),
      io:format("run Cb~n"),
      eredis_sub:ack_message(Sub),
      receiver(Sub,Cb);
    M ->
      io:format("heard ~p~n",[M]),
      eredis_sub:ack_message(Sub),
      receiver(Sub,Cb)
  end.

to_int (B) ->
  {Int,[]} = string:to_integer(binary:bin_to_list(B)),
  Int.

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
parse_twitter_date (Date) ->
  {_,[_,Month,Day,Hour,Min,Seconds,TimePlus,Timezone,Year],_} = io_lib:fread("~s ~s ~d ~d:~d:~d ~-~d ~d",Date),
  AsDate = {{Year,month_name_to_erlang_month(Month),Day},{Hour,Min,Seconds}},
  calendar:datetime_to_gregorian_seconds(AsDate) + TimePlus * Timezone * ?HOUR.

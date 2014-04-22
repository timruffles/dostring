-module(tw_store).
-behaviour(gen_server).

-include("../include/habitpop_records.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SERVER, ?MODULE).
-record(state,{pg,redis}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,tweet_state/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

tweet_state(Tweet)->
    gen_server:call(?SERVER,{tweet_state,Tweet}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({RedisConfig,PgConfig}) ->
    {ok, Redis} = erlang:apply(eredis,start_link,RedisConfig),
    Pg = erlang:apply(pgsql_connection,open,PgConfig),
    {ok, #state{pg=Pg,redis=Redis}}.

handle_call({tweet_state, Tweet}, _From, ServerState = #state{redis=Redis,pg=Pg}) ->
    ok = store_longterm(Pg,Tweet),
    TweetState = tweet_state(Redis,Tweet),
    {reply, TweetState, ServerState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

store_longterm (Pg,#tweet{user_id=UserId,hashtags=Hashtags,text=Text,gregorian_seconds=AtSeconds,id=Id}) ->
  InsertData = [UserId,Text,calendar:gregorian_seconds_to_datetime(AtSeconds),Id],
  HabtomId = case pgsql_connection:extended_query("INSERT INTO habtoms (user_id,text,happened_at,source_tweet_id) VALUES ($1::bigint,$2::varchar,$3::timestamptz,$4::bigint) RETURNING id",InsertData,Pg) of
    {{insert,_,_},[{Hid}]} -> Hid
  end,
  HashtagsForBatch = lists:map(fun (Ht) -> [Ht,HabtomId] end,Hashtags),
  Res = pgsql_connection:batch_query("INSERT INTO habits_to_habtoms (habit,habtom_id) VALUES ($1::varchar,$2::bigint)",HashtagsForBatch,Pg),
  ok = assert_inserts(Res),
  ok.

assert_inserts (Res) ->
  lists:foreach(fun ({{insert,_,_},[]}) ->
    ok
  end,Res),
  ok.

tweet_state(Redis,#tweet{gregorian_seconds=CreatedAt} = Tweet) ->
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
        <<MostRecent:32,_Rest/binary>> = Items,
        PreviousSeconds = to_int(MostRecent),
        DiffDays = streak_day_diff(CreatedAt,PreviousSeconds),
        {Event,New} = case DiffDays of
          0 ->
            {streak,<<SecondsBin/binary,Items/binary>>};
          1 ->
            {broke,<<CreatedAt:32>>}
        end,
        {ok,_} = eredis:q(Redis,["HSET",habits_key(Tweet),HabitName,New]),
        Status = #habit_status{broke_streak=
          case Event of
            streak -> false;
            _O -> true
          end,
          streak_length=get_streak_length(New)},
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




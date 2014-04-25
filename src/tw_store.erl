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

init({PgConfig}) ->
    Pg = erlang:apply(pgsql_connection,open,PgConfig),
    {ok, #state{pg=Pg}}.

handle_call({tweet_state, Tweet}, _From, ServerState = #state{pg=Pg}) ->
    NormalisedTweet = Tweet#tweet{hashtags=normalise_hashtags(Tweet#tweet.hashtags)},
    TweetState = persist_and_retrieve_user_state(Pg,NormalisedTweet),
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

%
% Retrieve, format, and update state
%
-spec persist_and_retrieve_user_state(any(),#tweet{}) -> #tweet_state{}.
persist_and_retrieve_user_state(Pg,#tweet{user_id=UserId,username=Username,hashtags=[]} = Tweet) ->
  UserAgeInSeconds = retrieve_user_state(Pg,UserId),
  #tweet_state{username=Username,habits_with_status=[],signedup_at=UserAgeInSeconds};
persist_and_retrieve_user_state(Pg,#tweet{gregorian_seconds=CreatedAt,hashtags=Hashtags} = Tweet) ->
  {Age,HabitStatuses} = retrieve_user_habit_states(Pg,Tweet),
  HabitNamesStatuses = lists:zip(Hashtags,HabitStatuses),
  NewState = #tweet_state{username=Tweet#tweet.username,habits_with_status=HabitNamesStatuses,signedup_at=Age},
  ok = persist_new_states_from_tweet(Pg,Tweet,NewState),
  NewState.

-spec retrieve_user_habit_states(any(),#tweet{}) -> {non_neg_integer(),[#habit_status{}]}.
retrieve_user_habit_states (Pg,#tweet{user_id=UserId,hashtags=Hashtags,gregorian_seconds=CreatedAt}) ->
  UserAgeInSeconds = retrieve_user_state(Pg,UserId),
  TweetAt = calendar:gregorian_seconds_to_datetime(CreatedAt),
  HabitStatuses = lists:map(fun(Habit) ->
    AllTime = case pgsql_connection:param_query(
      "select count(*) from streaks where user_id=? AND habit=?",[UserId,Habit],Pg) of
      {selected,[]} -> 0;
      {selected,[{N}]} -> N
    end,
    State = case pgsql_connection:param_query(
        "select length, (latest_at < date(?) - interval '1 day') AS broken
          FROM streaks
          WHERE
          user_id=?
          AND habit=?
          AND latest_at < date(?)
          ORDER BY latest_at DESC
          limit 2",
        [TweetAt,UserId,Habit,TweetAt], Pg) of
      {selected,[]} -> #habit_status{broke_streak=false,streak_length=1,previous_streak_length=0};
      {selected,[{Length,true}]} -> #habit_status{broke_streak=true,streak_length=0,previous_streak_length=Length};
      {selected,[{Length,false}]} -> #habit_status{broke_streak=false,streak_length=Length+1,previous_streak_length=0};
      {selected,[{Length,true},_]} -> #habit_status{broke_streak=true,streak_length=0,previous_streak_length=Length};
      {selected,[{Length,false},{OldLength,_}]} -> #habit_status{broke_streak=false,streak_length=Length+1,previous_streak_length=OldLength}
    end,
    State#habit_status{new_habit=AllTime == 0}
  end,Hashtags),
  {UserAgeInSeconds,HabitStatuses}.

retrieve_user_state(Pg,UserId) ->
  {selected,User} = pgsql_connection:param_query("select created_at from users where id = ? limit 1", [UserId], Pg),
  UserAgeInSeconds = case User of
    [] -> 0;
    [{CreatedAt}] -> calendar:datetime_to_gregorian_seconds(CreatedAt)
  end.

-spec persist_new_states_from_tweet(any(),#tweet{},#tweet_state{}) -> ok.
persist_new_states_from_tweet (Pg,Tweet,#tweet_state{signedup_at=Age}) ->
  ok = persist_habtoms(Pg,Tweet),
  ok = case Age of
    0 -> persist_signup_event(Pg,Tweet);
    _Else -> ok
  end.

-spec persist_habtoms(any(),#tweet{}) -> ok.
persist_habtoms (Pg,#tweet{user_id=UserId,hashtags=Hashtags,text=Text,gregorian_seconds=AtSeconds,id=Id}) ->
  AtDatetime = calendar:gregorian_seconds_to_datetime(AtSeconds),
  InsertData = [UserId,Text,AtDatetime,Id],
  {{insert,_,_},[{HabtomId}]} = pgsql_connection:extended_query("INSERT INTO habtoms (user_id,text,happened_at,source_tweet_id) VALUES ($1::bigint,$2::varchar,$3::timestamptz,$4::bigint) RETURNING id",InsertData,Pg),
  HashtagsForBatch = lists:map(fun (Ht) -> [Ht,HabtomId] end,Hashtags),
  Res = pgsql_connection:batch_query("INSERT INTO habits_to_habtoms (habit,habtom_id) VALUES ($1::varchar,$2::bigint)",HashtagsForBatch,Pg),
  ok = assert_inserts(Res),
  lists:foreach(fun(Ht) ->
    StreakParams = [AtDatetime,Ht,UserId,AtDatetime],
    case pgsql_connection:param_query("SELECT id, date(?) - date(latest_at) FROM streaks WHERE habit = ? AND user_id = ? AND latest_at >= date(?) - interval '1 day' LIMIT 1",StreakParams,Pg) of
      {selected,[{InsertId,0}]} -> % TODAY
        {updated,1} = pgsql_connection:param_query("UPDATE streaks SET latest_at = ? WHERE id = ?",[AtDatetime,InsertId],Pg);
      {selected,[{InsertId,1}]} -> % STREAK
        {updated,1} = pgsql_connection:param_query("UPDATE streaks SET length = length + 1, latest_at = ? WHERE id = ?",[AtDatetime,InsertId],Pg);
      {selected,[]} -> % BROKEN/NEW
        {updated,1} = pgsql_connection:param_query("INSERT INTO streaks (latest_at,habit,user_id,started_at,length) VALUES (?,?,?,date(?),1)",StreakParams,Pg)
    end
  end,Hashtags),
  ok.

-spec persist_signup_event(any(),#tweet{}) -> ok.
persist_signup_event (Pg,#tweet{user_id=UserId,gregorian_seconds=CreatedAt,username=Username} = Tweet) ->
  InsertData = [UserId,Username,calendar:gregorian_seconds_to_datetime(CreatedAt)],
  {{insert,_,_},[{_Id}]} = pgsql_connection:extended_query(
    "INSERT INTO users (id,username,created_at) VALUES ($1::bigint,$2::varchar,$3::timestamptz) RETURNING id",InsertData,Pg),
  ok.

assert_inserts (Res) ->
  lists:foreach(fun ({{insert,_,_},[]}) ->
    ok
  end,Res),
  ok.

normalise_hashtags (Hts) ->
  lists:map(fun string:to_lower/1,Hts).


%
% TESTS
%


functional_test_() ->
  {
    setup,
    fun() ->
      {ok,_} = pgsql_connection_sup:start_link(), 
      Pg = erlang:apply(pgsql_connection,open,test_pg_conf()),
      ok = initialize_db(Pg),
      {ok,Pid} = test_start_link(),
      {Pg,Pid}
    end,
    fun({Pg,Pid}) -> exit(Pid,normal) end,
    fun({Pg,_}) ->
      [?_test(
       begin
          % simple ensure can see a streak
          FirstState = tweet(["gym"],{1,10}),
          assert_new_habit(FirstState,"gym"),
          StateAfterDay1 = tweet(["gym","sandwich"],{1,12}),
          %?debugVal(StateAfterDay1),
          assert_streak(1,StateAfterDay1,"gym"),
          assert_not_broken(StateAfterDay1,"gym"),

          % streak continues, not affected by case
          StateAfterDay2 = tweet(["GYM"],{2,12}),
          %?debugVal(StateAfterDay2),
          assert_streak(2,StateAfterDay2,"gym"),

          % stream broken - doesn't see 'foo' as continuation
          tweet(["foo"],{3,12}),
          StateAfterDay4 = tweet(["gym","foo"],{4,12}),
          %?debugVal(StateAfterDay4),
          assert_broken(StateAfterDay4,"gym"),
          assert_previous_streak_length(2,StateAfterDay4,"gym"),

          % streak can see previous streak correctly
          tweet(["monkey"],{5,10}),
          tweet(["shopping"],{5,10}),
          tweet(["shopping"],{6,10}),
          tweet(["shopping"],{7,10}),
          StateAfterDay10 = tweet(["gym"],{10,10}),
          %?debugVal(StateAfterDay10),
          assert_broken(StateAfterDay10,"gym"),
          assert_previous_streak_length(1,StateAfterDay10,"gym")
       end)]
    end
  }.


tweet (Hashtags,Ts) ->
  tweet(Hashtags,Ts,"bobo",2369204930).
tweet (Hashtags,{Day,Hour},Username,UserId) ->
  Secs = calendar:datetime_to_gregorian_seconds({{2014,1,Day},{Hour,56,44}}),
  Tweet = #tweet{username=Username,text="Some shiz",hashtags=Hashtags,user_id=UserId,id=random:uniform(1000000000),gregorian_seconds=Secs},
  tweet_state(Tweet).

test_start_link() ->
  start_link({test_pg_conf()}).

test_pg_conf() -> app_config:pg_config("postgres://habitpop_test:pass@localhost:5432/habitpop_test").

initialize_db(Pg) ->
  lists:foreach(fun(Sql) ->
    {{_,_},_} = pgsql_connection:simple_query(Sql,Pg)
  end,[
    "DROP TABLE IF EXISTS habtoms, habits_to_habtoms, users, streaks",
    "CREATE TABLE habtoms ( id BIGSERIAL, user_id BIGINT NOT NULL, text varchar(150), happened_at TIMESTAMPTZ NOT NULL, source_tweet_id BIGINT );",
    "CREATE TABLE habits_to_habtoms ( habit varchar(150) NOT NULL, habtom_id INT NOT NULL);",
    "CREATE TABLE users ( id BIGINT NOT NULL, username varchar(50) NOT NULL, created_at TIMESTAMPTZ NOT NULL );",
    "CREATE TABLE streaks ( id SERIAL, user_id BIGINT NOT NULL, habit varchar(150) NOT NULL, started_at DATE NOT NULL, latest_at TIMESTAMPTZ, length INT NOT NULL DEFAULT 1);",
    "CREATE UNIQUE INDEX streaks_uid_habit_started_at ON streaks (user_id,habit,started_at)"
  ]).

find_habit_state(Hs,Name) ->
  St = lists:keyfind(Name,1,Hs),
  case St of
    false -> fail(io_lib:format("Should have had a habit state: '~s' in ~p",[Name,Hs]));
    {_K,X} -> X
  end.

fail(Str) ->
  ?debugMsg(Str),
  exit(failed).

assert_streak(N,#tweet_state{habits_with_status=Hs},Name) ->
  St = find_habit_state(Hs,Name),
  ?assertEqual(N,St#habit_status.streak_length).

assert_new_habit(#tweet_state{habits_with_status=Hs},Name) ->
  St = find_habit_state(Hs,Name),
  ?assertEqual(true,St#habit_status.new_habit).

assert_broken(#tweet_state{habits_with_status=Hs},Name) ->
  St = find_habit_state(Hs,Name),
  ?assertEqual(St#habit_status.broke_streak,true).
assert_not_broken(#tweet_state{habits_with_status=Hs},Name) ->
  St = find_habit_state(Hs,Name),
  ?assertEqual(St#habit_status.broke_streak,false).

assert_previous_streak_length(N,#tweet_state{habits_with_status=Hs},Name) ->
  St = find_habit_state(Hs,Name),
  ?assertEqual(N,St#habit_status.previous_streak_length).

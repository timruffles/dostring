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
    TweetState = persist_and_retrieve_user_state(Pg,Tweet),
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
persist_and_retrieve_user_state(Pg,#tweet{gregorian_seconds=CreatedAt,hashtags=Hashtags} = Tweet) ->
  {Age,HabitStatuses} = retrieve_user_habit_states(Pg,Tweet),
  HabitNamesStatuses = lists:zip(Hashtags,HabitStatuses),
  HabitStatusList = lists:map(fun habit_status_pair/1,HabitNamesStatuses),
  NewState = #tweet_state{username=Tweet#tweet.username,habits_with_status=HabitStatusList,signedup_at=Age},
  ok = persist_new_states_from_tweet(Pg,Tweet,NewState),
  NewState.

-spec retrieve_user_habit_states(any(),#tweet{}) -> {non_neg_integer(),[#habit_status{}]}.
retrieve_user_habit_states (Pg,#tweet{user_id=UserId,hashtags=Hashtags}) ->
  User = pgsql_connection:param_query("select * from users where id = ? limit 1", [UserId], Pg),
  UserAgeInSeconds = case User of
    [] -> 0;
    [{CreatedAt}] -> calendar:datetime_to_gregorian_seconds(CreatedAt)
  end,
  HabitStatuses = lists:map(fun(Habit) ->
    case pgsql_connection:param_query(
        "select happend_at from habtoms where
          user_id=?
          AND exists 
            (select * from habits_to_habtoms where habtom_id=habits.id AND habit=?)
          order by happened_at DESC limit 1",
        [UserId,Habit], Pg) of
      [] -> new_habit;
      [{HappenedAt}] -> calendar:datetime_to_gregorian_seconds(HappenedAt)
    end
  end,Hashtags),
  {UserAgeInSeconds,HabitStatuses}.

-spec persist_new_states_from_tweet(any(),#tweet{},#tweet_state{}) -> ok.
persist_new_states_from_tweet (Pg,Tweet,#tweet_state{signedup_at=Age}) ->
  ok = persist_habtoms(Pg,Tweet),
  ok = case Age of
    0 -> persist_signup_event(Pg,Tweet);
    _Else -> ok
  end.

-spec persist_habtoms(any(),#tweet{}) -> ok.
persist_habtoms (Pg,#tweet{user_id=UserId,hashtags=Hashtags,text=Text,gregorian_seconds=AtSeconds,id=Id}) ->
  InsertData = [UserId,Text,calendar:gregorian_seconds_to_datetime(AtSeconds),Id],
  {{insert,_,_},[{HabtomId}]} = pgsql_connection:extended_query("INSERT INTO habtoms (user_id,text,happened_at,source_tweet_id) VALUES ($1::bigint,$2::varchar,$3::timestamptz,$4::bigint) RETURNING id",InsertData,Pg),
  HashtagsForBatch = lists:map(fun (Ht) -> [Ht,HabtomId] end,Hashtags),
  Res = pgsql_connection:batch_query("INSERT INTO habits_to_habtoms (habit,habtom_id) VALUES ($1::varchar,$2::bigint)",HashtagsForBatch,Pg),
  Res = pgsql_connection:param_query("UPDATE streaks WHERE habit IN ? AND broken_at IS NULL AND ",HashtagsForBatch,Pg),
  ok = assert__inserts(Res),
  ok.

-spec persist_signup_event(any(),#tweet{}) -> ok.
persist_signup_event (Pg,#tweet{user_id=UserId,gregorian_seconds=CreatedAt,username=Username} = Tweet) ->
  InsertData = [UserId,Username,calendar:gregorian_seconds_to_datetime(CreatedAt)],
  {{insert,_,_},[{_Id}]} = pgsql_connection:extended_query(
    "INSERT INTO users (id,username,created_at) VALUES ($1::bigint,$2::varchar,$3::timestamptz) RETURNING id",InsertData,Pg),
  ok.

assert__inserts (Res) ->
  lists:foreach(fun ({{insert,_,_},[]}) ->
    ok
  end,Res),
  ok.

-spec habit_status_pair({string(),non_neg_integer()}) -> [{string(),habit_status()}].
habit_status_pair({Name,[]}) ->
  {Name,new_habit};
habit_status_pair({Name,MostRecentHabitAtGregorianSeconds}) ->
  DiffDays = dates:streak_day_diff(CreatedAt,MostRecentHabitAtGregorianSeconds),
  Status = #habit_status{broke_streak=DiffDays =/= 0,streak_length=length(Items)},
  {Name,Status}.


functional_test() ->
  {
    setup,
    fun() -> {ok,Pid} = test_start_link(), Pid end,
    fun(Pid) -> exit(Pid,normal) end,
    fun(Srv) ->
      [?_test(
       begin
          % simple ensure can see a streak
          tweet(["gym"],{1,10}),
          StateAfterDay1 = tweet(["gym","sandwich"],{1,12}),
          assert_streak(1,StateAfterDay1,"gym"),
          assert_not_broken(StateAfterDay1,"gym"),

          % streak continues, not affected by case
          StateAfterDay2 = tweet(["GYM"],{2,12}),
          assert_streak(2,StateAfterDay2,"gym"),

          % stream broken - doesn't see 'foo' as continuation
          tweet(["foo"],{3,12}),
          StateAfterDay4 = tweet(["gym","foo"],{4,12}),
          assert_broken(StateAfterDay4,"gym"),
          assert_previous_streak_length(2,StateAfterDay4,"gym"),

          % streak can see previous streak correctly
          tweet(["monkey"],{5,10}),
          tweet(["shopping"],{5,10}),
          tweet(["shopping"],{6,10}),
          tweet(["shopping"],{7,10}),
          StateAfterDay10 = tweet(["gym"],{10,10}),
          assert_broken(StateAfterDay10,"gym"),
          assert_previous_streak_length(1,StateAfterDay10,"gym")
       end)]
    end
  }.


tweet (Hashtags,Ts) ->
  tweet(Hashtags,Ts,"bobo",12340923901).
tweet (Hashtags,{Day,Hour},Username,UserId) ->
  Secs = calendar:datetime_to_gregorian_seconds({{2014,1,Day},{Hour,56,44}}),
  Tweet = #tweet{username=Username,user_id=UserId,id=random:uniform(1000000000),gregorian_seconds=Secs},
  tweet_state(Tweet).

test_start_link() ->
  start_link(app_config:pg_config("postgres://habitpop_test:pass@localhost:5432/habitpop_test")).


find_habit_state(#tweet_state{habits_with_status=Hs},Name) ->
  lists:keyfind(Name,1,Hs),
  case St of
    false -> error("Should have had a habit state");
    X -> X
  end.

assert_streak(N,#tweet_state{habits_with_status=Hs},Name) ->
  St = find_habit_state(Hs,Name),
  ?assertEqual(N,St#habit_status.streak_length).

assert_broken(#tweet_state{habits_with_status=Hs},Name) ->
  St = find_habit_state(Hs,Name),
  ?assertEqual(St#habit_status.broke_streak,true).
assert_not_broken(#tweet_state{habits_with_status=Hs},Name) ->
  St = find_habit_state(Hs,Name),
  ?assertEqual(St#habit_status.broke_streak,false).

assert_previous_streak_length(N,#tweet_state{habits_with_status=Hs},Name) ->
  St = find_habit_state(Hs,Name),
  ?assertEqual(St#habit_status.previous_streak_length,N).

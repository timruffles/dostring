-define(DAY,24*60*60).
-define(HOUR,60*60).

-record(tweet,{username :: string(),user_id :: non_neg_integer(),id :: non_neg_integer(), hashtags :: [string()],text :: string() ,gregorian_seconds :: non_neg_integer()}).
-record(tweet_state,{username :: string(),habits_with_status :: [habit_status_pair()],signedup_at :: newuser | non_neg_integer() }).
-record(habit_status,{broke_streak :: boolean(),streak_length :: non_neg_integer(),previous_streak_length :: non_neg_integer(),new_habit::boolean(),count_today :: non_neg_integer()}).

-type habit_status() :: #habit_status{}.
-type habit_status_pair() :: {string(),habit_status()}.

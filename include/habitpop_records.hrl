-define(DAY,24*60*60).
-define(HOUR,60*60).

-record(tweet,{username,user_id,id,hashtags,text,gregorian_seconds}).
-record(tweet_state,{username,habits_with_status,signedup_at}).
-record(habit_status,{broke_streak,streak_length}).

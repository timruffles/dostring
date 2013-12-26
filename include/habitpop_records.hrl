-define(DAY,24*60*60).
-define(HOUR,60*60).

-record(tweet,{username,user_id,id,hashtags,text,gregorian_seconds}).
-record(tweet_state,{username,habits_with_status,days_old}).
-record(habit_status,{streak_days,latest_days_ago,total,today_total}).

-record(streak_day,{days_ago,count}).

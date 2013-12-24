# Habitpop

This twitter bot allows you to easily build up habits.

storage:

user$USERNAME -> hash with general fields
USERNAME$habits -> HASH of habits streaks


## starting

@habitpop #gym
-> @aliceorbob great start on your #gym habit!

@habitpop #gym #bacon
-> @aliceorbob great start on your #gym and #bacon habits!

@habitpop #gym #bacon
-> @aliceorbob starting your #gym and #bacon habits on the same day? We admire your confidence!

@habitpop #toomanyhashtags
-> @aliceorbob great start on your new habit(s)


@habitpop #gym
-> @aliceorbob ok, every journey starts with a first step

@habitpop #gym
-> @aliceorbob day two - keep growing that chain!

@habitpop #gym
-> @aliceorbob day three - nice work

@habitpop #gym
-> @aliceorbob great job on the last streak of 3 in your #gym habit, let's bet it this time

@habitpop #gym #food
-> @aliceorbob two in one day, nice one!

@habitpop #gym #gym
-> @aliceorbob double #gym today, great!

@habitpop #gym
-> wow, @aliceorbob is on a streak of 35 in their #gym habit, their personal best! congrats!


Command:

USER has just HABIT_1, HABIT_2 ... HABIT_N

USER = 1..20
TOTAL = 140

user - new or old
habits
- new
- old
-- streak
-- broken


~user ~was_good started ~new_habits habits, kept up the ~chain habit, missed a day of your ~missed habits

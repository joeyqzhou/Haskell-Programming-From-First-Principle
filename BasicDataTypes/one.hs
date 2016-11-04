data Mood = Low | High deriving Show
changeMood :: Mood -> Mood
changeMood Low = High
changeMood _ = Low



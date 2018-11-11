module Exercises02 where

class PopQuiz a

-- | Which of the following instances require 'FlexibleInstances'? Don't cheat
-- :D This is a tricky one, but look out for nested concrete types!

-- instance PopQuiz Bool            -- N
-- instance PopQuiz [Bool]          -- Y
-- instance PopQuiz [a]             -- N
-- instance PopQuiz (a, b)          -- N
-- instance PopQuiz [(a, b)]        -- Y
-- instance PopQuiz (IO a)          -- N

newtype RIO  r a = RIO (r -> IO a) -- Remember, this is a /new type/.
type    RIO' r a =      r -> IO a

-- instance PopQuiz (RIO Int a)     -- Y
-- instance PopQuiz (RIO r a)       -- N
-- instance PopQuiz (RIO' r a)      -- Y
-- instance PopQuiz (r -> IO a)     -- Y
-- instance PopQuiz (a -> b) -- We can write (a -> b) as ((->) a b).  -- N
-- instance PopQuiz (a -> b -> c)   -- Y
-- instance PopQuiz (a, b, c)       -- N
-- instance PopQuiz (a, (b, c))     -- Y
-- instance PopQuiz ()              -- N
-- instance PopQuiz (a, b, c, a)    -- Y

data Pair  a = Pair  a  a
type Pair' a =      (a, a)

-- instance PopQuiz (a, a)          -- Y
-- instance PopQuiz (Pair a)        -- N 
-- instance PopQuiz (Pair' a)       -- Y

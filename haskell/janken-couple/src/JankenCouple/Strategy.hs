module JankenCouple.Strategy where

import JankenCouple.Hand
data Strategy = OnlyStone | OnlyScissors
  deriving (Eq, Show)

select :: Strategy -> Hand
select OnlyStone = Stone
select OnlyScissors = Scissors

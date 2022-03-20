module JankenCouple.Strategy where

import JankenCouple.Hand

data Strategy =
  OnlyStone |
  OnlyScissors |
  OnlyPaper |
  OrderedStrategy { current :: Hand }
  deriving (Eq, Show)

orderedFrom :: Hand -> Strategy
orderedFrom h = OrderedStrategy h

select :: Strategy -> Hand
select OnlyStone = Stone
select OnlyScissors = Scissors
select OnlyPaper = Paper
select (OrderedStrategy current) = current

next :: Strategy -> Strategy
next (OrderedStrategy Stone) = OrderedStrategy(Scissors)
next (OrderedStrategy Scissors) = OrderedStrategy(Paper)
next (OrderedStrategy Paper) = OrderedStrategy(Stone)
next s = s

package philomagi.practices_by_game.janken_for_couple
package player

import hand.Hand

trait Strategy {
  def select: Hand
}
object Strategy {
  object Only {
    object OnlyStone extends Strategy {
      override def select: Hand = Hand.Stone
    }
    object OnlyScissors extends Strategy {
      override def select: Hand = Hand.Scissors
    }
    object OnlyPaper extends Strategy {
      override def select: Hand = Hand.Paper
    }
  }
}

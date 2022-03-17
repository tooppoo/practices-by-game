package philomagi.practices_by_game.janken_for_couple
package player

import hand.Hand
import player.Player.Battle.{Loser, NoSide, Winner}
import player.Player.{Id, Name, WinCount}

class Player private (
                       val id: Id,
                       val name: Name,
                       val strategy: Strategy,
                       val winCount: WinCount = WinCount.zero
                     ) {
  def battle(other: Player): (Player.Battle.Result, Player.Battle.Result) = selectHand.battle(other.selectHand) match {
    case Hand.Battle.Win => (
      asWinner,
      other.asLoser
    )
    case Hand.Battle.Lose => (
      other.asWinner,
      asLoser
    )
    case Hand.Battle.Draw => (asNoSide, other.asNoSide)
  }

  def copy(
            id: Id = id,
            name: Name = name,
            strategy: Strategy = strategy,
            winCount: WinCount = winCount
          ) = new Player(id, name, strategy, winCount)

  private def selectHand = strategy.select
  private def asWinner: Winner = new Player(id, name, strategy, winCount) with Winner
  private def asLoser: Loser = new Player(id, name, strategy, winCount) with Loser
  private def asNoSide: NoSide = new Player(id, name, strategy, winCount) with NoSide
}
object Player {
  def apply(id: Id, name: Name, strategy: Strategy) = new Player(id, name, strategy)

  case class Id(value: String) {
    require(value.nonEmpty)
  }
  case class Name(value: String) {
    require(value.nonEmpty)
  }
  case class WinCount private (value: Int) {
    require(value >= 0)

    def increment: WinCount = WinCount(value + 1)
  }
  object  WinCount {
    def zero = new WinCount(0)
  }

  object Battle {
    sealed trait Result { this: Player =>
      def toNext: Player = this
    }
    trait Winner extends Result { this: Player =>
      override def toNext: Player = copy(winCount = winCount.increment)
    }
    trait Loser extends Result { this: Player =>
    }
    trait NoSide extends Result { this: Player =>
    }
  }
}

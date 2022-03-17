package philomagi.practices_by_game.janken_for_couple
package game

import player.Player

class Janken(private val players: (Player, Player)) {
  def play(count: Janken.PlayCount): (Player, Player) =
    count
      .toRange
      .foldLeft(players) {
        (prev, _) => __play(prev)
      }

  private[this] def __play(players: (Player, Player)): (Player, Player) = players match {
    case (p1, p2) => p1 battle p2 match {
      case (p1After, p2After) => (p1After.toNext, p2After.toNext)
    }
  }
}
object Janken {
  case class PlayCount(value: Int) {
    require(value >= 1)

    def toRange: Seq[Int] = 1 to value
  }
}

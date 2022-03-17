package philomagi.practices_by_game.janken_for_couple

import game.Janken
import player.{Player, Strategy}

object Application extends App {
  val p1 = Player(
    Player.Id("player-1"),
    Player.Name("John Doe"),
    Strategy.Only.OnlyStone,
  )
  val p2 = Player(
    Player.Id("player-2"),
    Player.Name("Una Nancy Owen"),
    Strategy.Only.OnlyScissors,
  )

  val game = new Janken((p1, p2))

  val (p1After, p2After) = game.play(Janken.PlayCount(5))

  println(s"${p1After.name} won ${p1After.winCount}")
  println(s"${p2After.name} won ${p2After.winCount}")
}

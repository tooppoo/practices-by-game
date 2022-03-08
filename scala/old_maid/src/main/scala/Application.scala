package philomagi.practices_by_game.old_maid

import core.card.Deck
import core.game.{Dealer, OldMaid}
import core.player.Player
import core.util.Shuffle
import core.util.Shuffle.Preset

import scala.sys.exit
import scala.util.Random

object Application extends App {
  implicit val shuffle: Preset.RandomShuffle = Shuffle.Preset.RandomShuffle(new Random())

  val game = OldMaid(Dealer.apply)
    .addPlayer(Player(Player.Name("player-1")))
    .addPlayer(Player(Player.Name("player-2")))
    .addPlayer(Player(Player.Name("player-3")))
    .addPlayer(Player(Player.Name("player-4")))

  val result = game.setUp() match {
    case Left(e) =>
      println(e.getMessage)

      exit(1)
    case Right(g) =>
      g.play(Deck.full)
  }

  println(result)
}

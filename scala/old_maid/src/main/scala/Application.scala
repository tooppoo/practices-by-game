package philomagi.practices_by_game.old_maid

import core.card.Deck
import core.event.EventBus
import core.game.Events.{Deal, GameOver, SetupBegin, SetupFinish}
import core.game.{Dealer, OldMaid}
import core.player.Events.{DumpCard, MoveCard}
import core.player.Player
import core.shuffle.Shuffle
import core.shuffle.Shuffle.Preset

import scala.sys.exit
import scala.util.Random

object Application extends App {
  implicit val shuffle: Preset.RandomShuffle = Shuffle.Preset.RandomShuffle(new Random())

  val game = OldMaid(Dealer.apply)
    .addPlayer(Player(Player.Name("player-1")))
    .addPlayer(Player(Player.Name("player-2")))
    .addPlayer(Player(Player.Name("player-3")))
    .addPlayer(Player(Player.Name("player-4")))

  EventBus.on {
    case SetupBegin(members) =>
      println(s"start old-maid with $members")

    case Deal(aCard, to) =>
      println(s"deal $aCard to $to")

    case SetupFinish(members, alreadyFinished) =>
      println(s"setup finished. players are $members.")

      if (alreadyFinished.nonEmpty)
        println(s"but $alreadyFinished are already finished.")

    case MoveCard(from, to, aCard) =>
      println(s"$aCard moved from $from to $to")

    case DumpCard(owner, aCard) =>
      println(s"$owner dumped $aCard from the hand")

    case GameOver(_) =>
      println("game over!!!")

    case _ => nop()
  }

  val result = game.setUp() match {
    case Left(e) =>
      println(e.getMessage)

      exit(1)
    case Right(g) =>
      g.play(Deck.full)
  }

  println(s"players finished in order ${result.map(_.name).mkString(",")}")

  private def nop(): Unit = { /* no operation */ }
}

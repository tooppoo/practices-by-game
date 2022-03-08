package philomagi.practices_by_game.old_maid
package core.game

import core.card.{Card, Deck}
import core.event.EventBus
import core.game.Dealer.PlayerWithOrder
import core.game.Events.Deal
import core.player.Phase.Preparing

import scala.annotation.tailrec

class Dealer {
  def deal(from: Deck, to: Seq[Preparing]): Seq[Preparing] = {
    val players = to.zipWithIndex.map {
      case (p, i) => PlayerWithOrder(p, i)
    }
    val dealt = __deal(Some(from), players)

    dealt.sortBy(_.index).map(_.player).ensuring(
      _.map(_.name) == to.map(_.name)
    )
  }

  @tailrec
  private def __deal(deckMaybeEmpty: Option[Deck], to: Seq[PlayerWithOrder]): Seq[PlayerWithOrder] =
    (deckMaybeEmpty, to) match {
      case (Some(deck), nextPlayer :: restPlayer) =>
        val (aCard, nextDeck) = deck.drawn

        EventBus.emit(Deal(aCard, to = nextPlayer.player))

        __deal(nextDeck, restPlayer :+ nextPlayer.accept(aCard))
      case _ => to
    }

}
object Dealer {
  def apply = new Dealer

  private case class PlayerWithOrder(player: Preparing, index: Int) {
    def accept(aCard: Card): PlayerWithOrder = copy(player = player.accept(aCard))
  }
}

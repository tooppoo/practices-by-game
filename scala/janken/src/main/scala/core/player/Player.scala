package philomagi.practices_by_game.janken
package core.player

import core.card.Card
import core.player.Phase.Preparing

class Player private[player] (
                               val name: Player.Name,
                               val strategy: Strategy = new Strategy.SelectRandom,
                               private val cardsInHand: Seq[Card]
                             ) {
  lazy val cards: this.CardsInHand = new CardsInHand(cardsInHand)

  class CardsInHand(private val cards: Seq[Card]) {
    def insert(aCard: Card): CardsInHand = if (cards.contains(aCard)) {
      new CardsInHand(
        cards.filterNot(c => c == aCard)
      )
    } else {
      new CardsInHand(
        cards :+ aCard
      )
    }

    def provide(candidate: Candidate): (Card, CardsInHand) = {
      require(cards.nonEmpty)

      val aCard = cards(candidate.at)

      (
        aCard,
        new CardsInHand(
          cards.filterNot(c => c == aCard)
        )
      )
    }

    def count: Int = cards.length

    def isEmpty: Boolean = cards.isEmpty
    def nonEmpty: Boolean = !isEmpty

    def candidates: Seq[this.Candidate] =
      (0 until count)
        .map(Candidate)
        .ensuring(_.length == count)

    def toSeq: Seq[Card] = cards

    case class Candidate(at: Int) {
      require(0 <= at && at < count)
    }
  }

  object CardsInHand {
    def empty = new CardsInHand(Seq.empty)
  }
}
object Player {
  def apply(name: Name, strategy: Strategy = Strategy.SelectRandom()): Preparing
  = new Player(name, strategy, Seq.empty) with Preparing

  case class Name(name: String) {
    require(name.nonEmpty)
  }
}

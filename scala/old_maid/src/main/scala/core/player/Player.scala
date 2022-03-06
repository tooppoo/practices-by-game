package philomagi.practices_by_game.old_maid
package core.player

import core.card.Card
import core.player.Phase.Preparing

class Player private[player] (
                               val name: Player.Name,
                               val strategy: Strategy,
                               private val cardsInHand: Seq[Card]
                             ) {
  lazy val cards: this.CardsInHand = CardsInHand(cardsInHand)

  override def equals(obj: Any): Boolean = obj match {
    case other: Player => name == other.name && cards == other.cards
    case _ => false
  }

  override def toString: String = (name, cardsInHand).toString()

  case class CardsInHand(private val cards: Seq[Card]) {
    def notContain(aCard: Card): Boolean = !cards.contains(aCard)

    override def equals(obj: Any): Boolean = obj match {
      case other: Player#CardsInHand => cards == other.cards
      case _ => false
    }

    def insert(aCard: Card): CardsInHand = if (cards.contains(aCard)) {
      copy(
        cards.filterNot(c => c == aCard)
      )
    } else {
      copy(
        cards :+ aCard
      )
    }

    def provide(candidate: Candidate): (Card, CardsInHand) = {
      require(cards.nonEmpty, "can not provide any card from empty cards")

      val aCard = cards(candidate.at)

      (
        aCard,
        copy(
          cards.filterNot(c => c == aCard)
        )
      )
    }

    def count: Int = cards.length

    def isEmpty: Boolean = cards.isEmpty
    def nonEmpty: Boolean = !isEmpty

    def candidates: Seq[Candidate] =
      (0 until count)
        .map(Candidate)
        .ensuring(
          _.length == count,
          "must not provide more candidates than cards in hand and must provide candidates of whole cards"
        )

    def toSeq: Seq[Card] = cards

    override def toString: String = cards.toString()

    case class Candidate(at: Int) {
      require(0 <= at && at < count, "candidate must exist within cards-in-hand")
    }
  }
}
object Player {
  def apply(name: Name, strategy: Strategy = Strategy.SelectRandom()): Preparing
  = new Player(name, strategy, Seq.empty) with Preparing

  case class Name(name: String) {
    require(name.nonEmpty, "player name must not be empty")

    override def toString: String = name
  }
}

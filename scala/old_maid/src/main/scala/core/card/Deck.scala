package philomagi.practices_by_game.old_maid
package core.card

import core.util.Shuffle

class Deck private (private val cards: Seq[Card]) {
  require(cards.nonEmpty)

  def drawn: (Card, Option[Deck]) = cards match {
    case drawn :: Nil => (drawn, None)
    case drawn :: rest => (drawn, Some(new Deck(rest)))
  }

  def count: Int = cards.length

  def shuffle(implicit shuffle: Shuffle): Deck = new Deck(shuffle.shuffle(cards))
}
object Deck {
  lazy val full: Deck = new Deck(
    Seq(
      (1 to 13).map(Card.NumberCard.Number).map(Card.NumberCard(_)),
      (1 to 13).map(Card.NumberCard.Number).map(Card.NumberCard(_)),
      (1 to 13).map(Card.NumberCard.Number).map(Card.NumberCard(_)),
      (1 to 13).map(Card.NumberCard.Number).map(Card.NumberCard(_)),
      Seq(Card.Joker)
    ).flatten
  )
}

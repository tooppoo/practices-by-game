package philomagi.practices_by_game.old_maid
package core.card

import core.util.Shuffle.Shuffle

class Deck private (private val cards: Seq[Card]) {
  def drawn: (Card, Option[Deck]) = cards match {
    case drawn :: Nil => (drawn, None)
    case drawn :: rest => (drawn, Some(new Deck(rest)))
  }

  def count: Int = cards.length

  def shuffle(implicit shuffle: Shuffle[Card]): Deck = new Deck(shuffle(cards))
}
object Deck {
  def full: Deck = new Deck(
    Seq(
      (1 to 13).map(Card.NumberCard.Number).map(Card.NumberCard(_)),
      (1 to 13).map(Card.NumberCard.Number).map(Card.NumberCard(_)),
      (1 to 13).map(Card.NumberCard.Number).map(Card.NumberCard(_)),
      (1 to 13).map(Card.NumberCard.Number).map(Card.NumberCard(_)),
      Seq(Card.Joker)
    ).flatten
  )
}

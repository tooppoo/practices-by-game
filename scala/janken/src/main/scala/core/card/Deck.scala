package philomagi.practices_by_game.janken
package core.card

import scala.util.Random

class Deck private (private val cards: Seq[Card]) {
  def drawn: (Card, Deck) = {
    val drawn :: rest = cards

    (drawn, new Deck(rest))
  }

  def count: Int = cards.length

  def shuffle(implicit randomizer: Seq[Card] => Seq[Card]): Deck = new Deck(randomizer(cards))
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

  object Implicits {
    object Default {
      implicit lazy val randomShuffle: Seq[Card] => Seq[Card] = Random.shuffle(_)
    }
  }
}

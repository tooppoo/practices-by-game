package philomagi.practices_by_game.old_maid
package core.game

import core.card.{Card, Deck}
import core.player.Phase.Preparing
import core.player.Player

import org.scalatest.funspec.AnyFunSpec

class DealerTest extends AnyFunSpec {
  describe("deal a deck to players") {
    val n = (n: Int) => Card.NumberCard(Card.NumberCard.Number(n))
    val joker = Card.Joker
    val expectAs = (p: Preparing, cards: Seq[Card]) => cards.foldLeft(p)((pn, c) => pn.accept(c))

    it("should deal cards from the deck, one at a time, in order") {
      val deck = Deck.full
      val dealer = Dealer.apply

      val p1 :: p2 :: p3 :: p4 :: p5 :: _ = Seq(
        Player(Player.Name("p1")),
        Player(Player.Name("p2")),
        Player(Player.Name("p3")),
        Player(Player.Name("p4")),
        Player(Player.Name("p5")),
      )

      val expected = Seq(
        expectAs(p1, Seq(
          n(1), n(6), n(11), n(3),  n(8), n(13), n(5), n(10), n(2), n(7), n(12),
        )),
        expectAs(p2, Seq(
          n(2), n(7), n(12), n(4),  n(9),  n(1), n(6), n(11), n(3), n(8), n(13),
        )),
        expectAs(p3, Seq(
          n(3), n(8), n(13), n(5), n(10),  n(2), n(7), n(12), n(4), n(9), joker,
        )),
        expectAs(p4, Seq(
          n(4), n(9),  n(1), n(6), n(11),  n(3), n(8), n(13), n(5), n(10),
        )),
        expectAs(p5, Seq(
          n(5), n(10), n(2), n(7), n(12),  n(4), n(9),  n(1), n(6), n(11),
        )),
      )

      assert(Player(Player.Name("p1")).==(Player(Player.Name("p1"))))
      assert(expected == dealer.deal(from = deck, to = Seq(p1, p2, p3, p4, p5)))
    }
  }
}

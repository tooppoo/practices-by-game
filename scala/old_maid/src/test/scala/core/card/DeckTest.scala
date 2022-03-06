package philomagi.practices_by_game.old_maid
package core.card

import org.scalatest.funspec.AnyFunSpec

class DeckTest extends AnyFunSpec {
  describe("draw a card") {
    it("should provide a drawn card and rest of deck") {
      val deck = Deck.full
      val (drawn, rest) = deck.drawn

      assert((drawn, rest.count) == (Card.NumberCard(Card.NumberCard.Number(1)), 52))
    }
    it("should mutate receiver deck") {
      val deck = Deck.full
      val _ = deck.drawn

      assert(deck.count == 53)
    }
  }
}

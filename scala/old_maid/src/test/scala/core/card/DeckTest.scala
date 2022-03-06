package philomagi.practices_by_game.old_maid
package core.card

import org.scalatest.funspec.AnyFunSpec

class DeckTest extends AnyFunSpec {
  describe("draw a card") {
    describe("some cards left after") {
      it("should provide a drawn card and Some deck") {
        val deck = Deck.full

        deck.drawn match {
          case (drawn, Some(_)) => assert(drawn == Card.NumberCard(Card.NumberCard.Number(1)))
          case _ => fail()
        }
      }
    }

    it("should not mutate receiver deck") {
      val deck = Deck.full
      val _ = deck.drawn

      assert(deck.count == 53)
    }
  }
}

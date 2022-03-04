package core.card

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks
import philomagi.practices_by_game.janken.core.card.Card

class CardTest extends AnyFunSpec with TableDrivenPropertyChecks {
  describe("compare") {
    describe("equality") {
      val testCases = Table(
        ("left", "right", "expected"),
        (
          Card.NumberCard(Card.NumberCard.Number(1)),
          Card.NumberCard(Card.NumberCard.Number(1)),
          true,
        ),
        (
          Card.NumberCard(Card.NumberCard.Number(1)),
          Card.NumberCard(Card.NumberCard.Number(2)),
          false,
        ),
        (
          Card.NumberCard(Card.NumberCard.Number(2)),
          Card.NumberCard(Card.NumberCard.Number(1)),
          false,
        ),
        (
          Card.NumberCard(Card.NumberCard.Number(2)),
          Card.Joker,
          false,
        ),
        (
          Card.Joker,
          Card.NumberCard(Card.NumberCard.Number(2)),
          false,
        ),
        (
          Card.Joker,
          Card.Joker,
          false,
        ),
      )

      forAll(testCases) { (left, right, theyAreEqual) =>
        describe(s"$left == $right") {
          it(s"should be evaluated as $theyAreEqual") {
            (left == right) should be (theyAreEqual)
          }
        }
      }
    }

  }

  describe("NumberCard") {
    describe("generate") {
      describe("with invalid number") {
        val testCases = Table(
          "number",
          -1,
          0,
          14,
          100,
        )

        forAll(testCases) { number =>
          describe(s"number is $number") {
            it("should be failed") {
              assertThrows[IllegalArgumentException] {
                Card.NumberCard(Card.NumberCard.Number(number))
              }
            }
          }
        }
      }
    }
  }
}

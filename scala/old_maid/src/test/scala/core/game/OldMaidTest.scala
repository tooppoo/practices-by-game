package philomagi.practices_by_game.old_maid
package core.game

import core.card.Card
import core.player.{Player, Strategy}

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.prop.TableDrivenPropertyChecks

class OldMaidTest extends AnyFunSpec with TableDrivenPropertyChecks {
  describe("process") {
    describe("no player rest") {
      describe("drawer's rest cards and drawn's are same") {
        val testCases = Table(
          ("case name", "drawer", "drawn"),
          (
            "game can not end",
            Player(Player.Name("p1"), strategy = OldMaidTest.AlwaysDrawFirstStrategy)
              .accept(Card.NumberCard(Card.NumberCard.Number(1))),
            Player(Player.Name("p2"), strategy = OldMaidTest.AlwaysDrawFirstStrategy)
              .accept(Card.Joker),
          ),
          (
            "game can end but joker illegally dumped",
            Player(Player.Name("p1"), strategy = OldMaidTest.AlwaysDrawFirstStrategy)
              .accept(Card.NumberCard(Card.NumberCard.Number(1))),
            Player(Player.Name("p2"), strategy = OldMaidTest.AlwaysDrawFirstStrategy)
              .accept(Card.NumberCard(Card.NumberCard.Number(1))),
          ),
        )

        forAll(testCases) { (caseName, drawer, drawn) =>
          it(s"should not be allowed because $caseName") {
            for {
              drawerReady <- drawer.getReady
              drawnReady <- drawn.getReady
            } {
              val p1 = drawerReady.asDrawer
              val p2 = drawnReady.asDrawn

              assertThrows[IllegalArgumentException] {
                OldMaid.Phase.Playable.process(p1, p2, Seq.empty, Seq.empty)
              }
            }
          }
        }
      }

      describe("drawer's rest cards and drawn's are not same") {
        val testCases = Table(
          ("case name", "drawer", "drawn", "expected order"),
          (
            "drawer finish by draw",
            Player(Player.Name("drawer"), OldMaidTest.AlwaysDrawFirstStrategy)
              .accept(Card.NumberCard(Card.NumberCard.Number(1)))
              .getReady,
            Player(Player.Name("drawn"), OldMaidTest.AlwaysDrawFirstStrategy)
              .accept(Card.NumberCard(Card.NumberCard.Number(1)))
              .accept(Card.Joker)
              .getReady,
            Seq("drawer", "drawn"),
          ),
          (
            "drawn finish by draw",
            Player(Player.Name("drawer"), OldMaidTest.AlwaysDrawFirstStrategy)
              .accept(Card.NumberCard(Card.NumberCard.Number(1)))
              .accept(Card.Joker)
              .getReady,
            Player(Player.Name("drawn"), OldMaidTest.AlwaysDrawFirstStrategy)
              .accept(Card.NumberCard(Card.NumberCard.Number(1)))
              .getReady,
            Seq("drawn", "drawer"),
          ),
          (
            "both could not finish at once",
            Player(Player.Name("p1"), OldMaidTest.AlwaysDrawFirstStrategy)
              .accept(Card.NumberCard(Card.NumberCard.Number(1)))
              .getReady,
            Player(Player.Name("p2"), OldMaidTest.AlwaysDrawFirstStrategy)
              .accept(Card.Joker)
              .accept(Card.NumberCard(Card.NumberCard.Number(1)))
              .getReady,
            Seq("p2", "p1"),
          ),
        )

        forAll(testCases) { (caseName, drawerReady, drawnReady, expectedOrder) =>
          describe(caseName) {
            it(s"should list players as finish in order $expectedOrder") {
              for {
                drawer <- drawerReady
                drawn <- drawnReady
              } {
                val finished = OldMaid.Phase.Playable.process(drawer.asDrawer, drawn.asDrawn, Seq.empty, Seq.empty)

                assert(expectedOrder == finished.map(_.name.toString))
              }
            }
          }
        }
      }
    }
  }
}

object OldMaidTest {
  object AlwaysDrawFirstStrategy extends Strategy {
    override def selectCandidateFrom[C <: Player#CardsInHand#Candidate](candidates: Seq[C]): C = candidates.head
  }
}

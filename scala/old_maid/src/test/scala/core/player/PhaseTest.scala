package philomagi.practices_by_game.old_maid
package core.player

import core.card.Card
import core.player.Phase.{Drawer, Drawn, Finish, GetReady}

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class PhaseTest extends AnyFunSpec with TableDrivenPropertyChecks {
  describe("Preparing") {
    describe("any card not left when get-ready") {
      it("should transit to Finish phase") {
        val player = Player(Player.Name("p"))
          .accept(
            Card.NumberCard(Card.NumberCard.Number(1))
          )
          .accept(
            Card.NumberCard(Card.NumberCard.Number(1))
          )

        player.getReady match {
          case Left(phase) => phase shouldBe a[Finish]
          case _ => fail("should be left")
        }
      }
    }
    describe("some cards left when get-ready") {
      it("should transit to GetReady phase") {
        val player = Player(Player.Name("p"))
          .accept(
            Card.NumberCard(Card.NumberCard.Number(1))
          )

        player.getReady match {
          case Right(phase) => phase shouldBe a[GetReady]
          case _ => fail("should be right")
        }
      }
    }
  }

  describe("Drawn") {
    describe("after drawn") {
      describe("any card not left in the hand") {
        val testCases = Table(
          "last card in hand",
          Card.NumberCard(Card.NumberCard.Number(1)),
          Card.Joker
        )

        forAll(testCases) { lastCard =>
          describe(s"last card in hand is $lastCard") {
            it("should transit to Finish phase") {
              val player = Player(Player.Name("p")).accept(lastCard)

              for {
                ready <- player.getReady
              } {
                val drawn = ready.asDrawn
                val c = drawn.candidates.head
                val (_, after) = drawn.provide(c)

                after match {
                  case Left(phase) => phase shouldBe a[Finish]
                  case _ => fail("should be right")
                }
              }
            }
          }
        }
      }

      describe("some cards left in the hand") {
        val testCases = Table(
          "player",
          Player(Player.Name("p"))
            .accept(
              Card.NumberCard(Card.NumberCard.Number(1))
            )
            .accept(
              Card.NumberCard(Card.NumberCard.Number(2))
            ),
          Player(Player.Name("p"))
            .accept(Card.Joker)
            .accept(
              Card.NumberCard(Card.NumberCard.Number(2))
            ),
        )

        forAll(testCases) { player =>
          describe(s"player is $player") {
            it("should transit to Drawer phase after drawn") {
              for {
                ready <- player.getReady
              } {
                val drawn = ready.asDrawn
                val c = drawn.candidates.head
                val (_, after) = drawn.provide(c)

                after match {
                  case Right(phase) => phase shouldBe a[Drawer]
                  case _ => fail("should be left")
                }
              }
            }

            it("should not have provided card in hand") {
              for {
                ready <- player.getReady
              } {
                val drawn = ready.asDrawn
                val c = drawn.candidates.head
                val (aCard, after) = drawn.provide(c)

                after match {
                  case Right(phase) => assert(phase.cards.notContain(aCard))
                  case _ => fail("should be left")
                }
              }
            }
          }
        }
      }
    }

    describe("when skip") {
      it("should transit to drawer without drawn") {
        val player = Player(Player.Name("p"))
          .accept(
            Card.NumberCard(Card.NumberCard.Number(1))
          )

        for {
          ready <- player.getReady
        } {
          ready.asDrawn.skip shouldBe a[Drawer]
        }
      }
    }
  }

  describe("Drawer") {
    describe("after draw") {
      describe("any card not left in the hand") {
        it("should transit to Finish phase") {
          val drawer = Player(Player.Name("drawer"))
            .accept(
              Card.NumberCard(Card.NumberCard.Number(1))
            )
          val drawn = Player(Player.Name("drawn"))
            .accept(
              Card.NumberCard(Card.NumberCard.Number(1))
            )

          for {
            drawerReady <- drawer.getReady
            drawnReady <- drawn.getReady
          } {
            val (drawerNext, _) = drawerReady.asDrawer.drawFrom(drawnReady.asDrawn)

            drawerNext match {
              case Left(phase) => phase shouldBe a[Finish]
            }
          }
        }
      }
      describe("some cards left in the hand") {
        it("should transit to Drawn phase") {
          val drawer = Player(Player.Name("drawer"))
            .accept(
              Card.NumberCard(Card.NumberCard.Number(1))
            )
            .accept(
              Card.NumberCard(Card.NumberCard.Number(2))
            )
          val drawn = Player(Player.Name("drawn"))
            .accept(
              Card.NumberCard(Card.NumberCard.Number(1))
            )

          for {
            drawerReady <- drawer.getReady
            drawnReady <- drawn.getReady
          } {
            val (drawerNext, _) = drawerReady.asDrawer.drawFrom(drawnReady.asDrawn)

            drawerNext match {
              case Right(phase) => phase shouldBe a[Drawn]
            }
          }
        }
      }
    }
  }
}

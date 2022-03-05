package philomagi.practices_by_game.janken
package core.player

import core.card.Card

object Phase {
  trait Preparing { this: Player =>
    def accept(aCard: Card): Preparing =
      new Player(name = name, cardsInHand = cards.insert(aCard).toSeq) with Preparing

    def getReady: Preparing.NextOfGetReady = if (cards.isEmpty) {
      Left(
        new Player(name, cardsInHand = cards.toSeq.ensuring(_.isEmpty)) with Finish
      )
    } else {
      Right(
        new Player(name, cardsInHand = cards.toSeq.ensuring(_.nonEmpty)) with GetReady
      )
    }
  }
  object Preparing {
    type NextOfGetReady = Either[Finish, GetReady]
  }

  trait GetReady { this: Player =>
    require(cards.nonEmpty)

    def asDrawn: Drawn =
      new Player(name, cardsInHand = cards.toSeq.ensuring(_.nonEmpty)) with Drawn

    def asDrawer: Drawer =
      new Player(name, cardsInHand = cards.toSeq.ensuring(_.nonEmpty)) with Drawer
  }

  trait Drawn { this: Player =>
    require(cards.nonEmpty)

    def provide(candidate: cards.Candidate): (Card, Drawn.NextOfDrawn) = {
      val (aCard, rest) = cards.provide(candidate)
      val next = if (rest.isEmpty) {
        Left(
          new Player(name, cardsInHand = rest.toSeq.ensuring(_.isEmpty)) with Finish
        )
      } else {
        Right(
          new Player(name, cardsInHand = rest.toSeq.ensuring(_.nonEmpty)) with Drawer
        )
      }

      (aCard, next)
    }

    def candidates: Seq[cards.Candidate] = cards.candidates

    def skip: Drawer = new Player(name, cardsInHand = cards.toSeq.ensuring(_.nonEmpty)) with Drawer
  }
  object Drawn {
    type NextOfDrawn = Either[Finish, Drawer]
  }

  trait Drawer { this: Player =>
    require(cards.nonEmpty)

    def drawFrom(drawn: Drawn): (Drawer.NextOfDrawer, Drawn.NextOfDrawn) = {
      val candidate = strategy.selectCandidateFrom(drawn.candidates)
      val (aCard, nextOfDrawn) = drawn.provide(candidate)

      val nextCardsInHand = cards.insert(aCard)
      val nextOfDrawer = if (nextCardsInHand.isEmpty) {
        Left(
          new Player(name, cardsInHand = nextCardsInHand.toSeq.ensuring(_.isEmpty)) with Finish
        )
      } else {
        Right(
          new Player(name, cardsInHand = nextCardsInHand.toSeq.ensuring(_.nonEmpty)) with Drawn
        )
      }

      (nextOfDrawer, nextOfDrawn)
    }
  }
  object Drawer {
    type NextOfDrawer = Either[Finish, Drawn]
  }

  trait Finish { this: Player =>
    require(cards.isEmpty)

    override val name: Player.Name = this.name
  }
}

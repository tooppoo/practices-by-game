package philomagi.practices_by_game.old_maid
package core.player

import core.card.Card

trait Phase { this: Player =>
  val name: Player.Name
  val cards: Player#CardsInHand
}
object Phase {
  trait Preparing extends Phase { this: Player =>
    def accept(aCard: Card): Preparing =
      new Player(name, strategy, cards.insert(aCard).toSeq) with Preparing

    def getReady: Preparing.NextOfGetReady = if (cards.isEmpty) {
      Left(
        new Player(name, strategy, cards.toSeq.ensuring(_.isEmpty)) with Finish
      )
    } else {
      Right(
        new Player(name, strategy, cards.toSeq.ensuring(_.nonEmpty)) with GetReady
      )
    }
  }
  object Preparing {
    type NextOfGetReady = Either[Finish, GetReady]
  }

  trait GetReady extends Phase with Behavior.CanGiveUp { this: Player =>
    require(cards.nonEmpty)

    def asDrawn: Drawn =
      new Player(name, strategy, cards.toSeq.ensuring(_.nonEmpty)) with Drawn

    def asDrawer: Drawer =
      new Player(name, strategy, cards.toSeq.ensuring(_.nonEmpty)) with Drawer
  }

  trait Drawn extends Phase with Behavior.CanGiveUp { this: Player =>
    require(cards.nonEmpty)

    def provide(candidate: cards.Candidate): (Card, Drawn.NextOfDrawn) = {
      val (aCard, rest) = cards.provide(candidate)
      val next = if (rest.isEmpty) {
        Left(
          new Player(name, strategy, rest.toSeq.ensuring(_.isEmpty)) with Finish
        )
      } else {
        Right(
          new Player(name, strategy, rest.toSeq.ensuring(_.nonEmpty)) with Drawer
        )
      }

      (aCard, next)
    }

    def candidates: Seq[cards.Candidate] = cards.candidates

    def skip: Drawer = new Player(name, strategy, cards.toSeq.ensuring(_.nonEmpty)) with Drawer
  }
  object Drawn {
    type NextOfDrawn = Either[Finish, Drawer]
  }

  trait Drawer extends Phase with Behavior.CanGiveUp { this: Player =>
    require(cards.nonEmpty)

    def drawFrom(drawn: Drawn): (Drawer.NextOfDrawer, Drawn.NextOfDrawn) = {
      val candidate = strategy.selectCandidateFrom(drawn.candidates)
      val (aCard, nextOfDrawn) = drawn.provide(candidate)

      val nextCardsInHand = cards.insert(aCard)
      val nextOfDrawer = if (nextCardsInHand.isEmpty) {
        Left(
          new Player(name, strategy, nextCardsInHand.toSeq.ensuring(_.isEmpty)) with Finish
        )
      } else {
        Right(
          new Player(name, strategy, nextCardsInHand.toSeq.ensuring(_.nonEmpty)) with Drawn
        )
      }

      (nextOfDrawer, nextOfDrawn)
    }
  }
  object Drawer {
    type NextOfDrawer = Either[Finish, Drawn]
  }

  trait Finish extends Phase { this: Player =>
    require(cards.isEmpty)
  }

  object Behavior {
    trait CanGiveUp { this: Player =>
      def giveUp: Finish = {
        require(cards.toSeq == Seq(Card.Joker))

        new Player(name, strategy, cardsInHand = Seq.empty) with Finish
      }
    }
  }
}

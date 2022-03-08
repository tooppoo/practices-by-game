package philomagi.practices_by_game.old_maid
package core.player

import core.card.Card
import core.event.EventBus
import core.player.Events.{MoveCard, Transit}
import core.player.Phase.Behavior.{CanGiveUp, CanTransit}

import scala.util.chaining._

trait Phase { this: Player =>
  val name: Player.Name
  val cards: Player#CardsInHand
  val strategy: Strategy

  def currentPhase: String
}
object Phase {
  trait Preparing extends Phase with CanTransit { this: Player =>
    def accept(aCard: Card): Preparing =
      new Player(name, strategy, cards.insert(aCard).toSeq) with Preparing

    def getReady: Preparing.NextOfGetReady = if (cards.isEmpty) {
      Left(
        transit(from = this, to = new Player(name, strategy, cards.toSeq.ensuring(_.isEmpty)) with Finish)
      )
    } else {
      Right(
        transit(from = this, to = new Player(name, strategy, cards.toSeq.ensuring(_.nonEmpty)) with GetReady)
      )
    }

    override def currentPhase: String = "Preparing"
  }
  object Preparing {
    type NextOfGetReady = Either[Finish, GetReady]
  }

  trait GetReady extends Phase with CanGiveUp with CanTransit { this: Player =>
    require(cards.nonEmpty)

    def asDrawn: Drawn = transit(
      this,
      new Player(name, strategy, cards.toSeq.ensuring(_.nonEmpty)) with Drawn
    )

    def asDrawer: Drawer = transit(
      this,
      new Player(name, strategy, cards.toSeq.ensuring(_.nonEmpty)) with Drawer
    )

    override def currentPhase: String = "GetReady"
  }

  trait Drawn extends Phase with CanGiveUp with CanTransit { this: Player =>
    require(cards.nonEmpty)

    def provide(candidate: cards.Candidate): (Card, Drawn.NextOfDrawn) = {
      val (aCard, rest) = cards.provide(candidate)
      val next = if (rest.isEmpty)
        Left(
          transit(this, new Player(name, strategy, rest.toSeq.ensuring(_.isEmpty)) with Finish)
        )
      else
        Right(
          transit(this, new Player(name, strategy, rest.toSeq.ensuring(_.nonEmpty)) with Drawer)
        )

      (aCard, next)
    }

    def candidates: Seq[cards.Candidate] = cards.candidates

    def skip: Drawer = transit(
      this,
      new Player(name, strategy, cards.toSeq.ensuring(_.nonEmpty)) with Drawer
    )

    override def currentPhase: String = "Drawn"
  }
  object Drawn {
    type NextOfDrawn = Either[Finish, Drawer]
  }

  trait Drawer extends Phase with CanGiveUp with CanTransit { this: Player =>
    require(cards.nonEmpty)

    def drawFrom(drawn: Drawn): (Drawer.NextOfDrawer, Drawn.NextOfDrawn) = {
      val candidate = strategy.selectCandidateFrom(drawn.candidates)
      val (aCard, nextOfDrawn) = drawn.provide(candidate)

      EventBus.emit(
        MoveCard(from = drawn, to = this, aCard)
      )

      val nextCardsInHand = cards.insert(aCard)
      val nextOfDrawer = if (nextCardsInHand.isEmpty)
        Left(
          transit(this, new Player(name, strategy, nextCardsInHand.toSeq.ensuring(_.isEmpty)) with Finish)
        )
      else
        Right(
          transit(this, new Player(name, strategy, nextCardsInHand.toSeq.ensuring(_.nonEmpty)) with Drawn)
        )

      (nextOfDrawer, nextOfDrawn)
    }

    override def currentPhase: String = "Drawer"
  }
  object Drawer {
    type NextOfDrawer = Either[Finish, Drawn]
  }

  trait Finish extends Phase { this: Player =>
    require(cards.isEmpty)

    override def currentPhase: String = "Finish"
  }

  object Behavior {
    trait CanGiveUp extends CanTransit { this: Phase =>
      def giveUp: Finish = {
        require(cards.toSeq == Seq(Card.Joker))

        transit(this, new Player(name, strategy, cardsInHand = Seq.empty) with Finish)
      }
    }

    trait CanTransit {
      protected def transit[C <: Phase, N <: Phase](from: C, to: N): N = to.tap { next =>
        EventBus.emit(Transit(from, next))
      }
    }
  }
}

package philomagi.practices_by_game.old_maid
package core.game

import core.card.Deck
import core.game.OldMaid.Phase.Playable.{PlayersOrderByFinishedEarly, process}
import core.game.OldMaid.Phase.Preparing.Exception.TooLessPlayersException
import core.player.Phase.{Drawer, Drawn, Finish, GetReady, Preparing => PreparingPlayer}
import core.util.Shuffle

import scala.annotation.tailrec

class OldMaid private[game](
                             private val dealer: Dealer,
                             private val players: Seq[PreparingPlayer]
                           )

object OldMaid {
  def apply(dealer: Dealer, deck: Deck) = new OldMaid(dealer, Seq.empty) with Phase.Preparing

  object Phase {
    trait Preparing {
      this: OldMaid =>
      def addPlayer(p: PreparingPlayer): Preparing = new OldMaid(dealer, players :+ p) with Preparing

      def setUp(): Either[Exception, Playable] = if (players.length >= 2) {
        Right(
          new OldMaid(dealer, players) with Playable
        )
      } else {
        Left(
          new TooLessPlayersException("players must exist more than or equal 2")
        )
      }
    }

    object Preparing {
      object Exception {
        class TooLessPlayersException(message: String) extends Exception(message)
      }
    }

    trait Playable {
      this: OldMaid =>
      require(players.length >= 2, "can not play old-maid with alone")

      def play(deck: Deck = Deck.full)(implicit shuffle: Shuffle): PlayersOrderByFinishedEarly = {
        val shuffledDeck = deck.shuffle
        val shuffledPlayers = shuffle.shuffle(players)

        val dealtPlayers = dealer.deal(from = shuffledDeck, to = shuffledPlayers)

        val (getReadies, alreadyFinished) = dealtPlayers.map(_.getReady).foldLeft(
          (Seq.empty[GetReady], Seq.empty[Finish])
        ) {
          case ((getReadies, alreadyFinished), Right(p)) => (getReadies :+ p, alreadyFinished)
          case ((getReadies, alreadyFinished), Left(p)) => (getReadies, alreadyFinished :+ p)
        }

        val result = getReadies match {
          case last :: Nil => alreadyFinished :+ last.giveUp
          case drawer :: drawn :: rest => process(
            drawer.asDrawer,
            drawn.asDrawn,
            rest.map(_.asDrawn),
            alreadyFinished
          )
        }

        result.ensuring(_.length == players.length)
      }
    }

    object Playable {
      type PlayersOrderByFinishedEarly = Seq[Finish]

      @tailrec
      def process(
                   drawer: Drawer,
                   drawn: Drawn,
                   rest: Seq[Drawn],
                   finished: PlayersOrderByFinishedEarly
                 ): PlayersOrderByFinishedEarly = {
        require(
          if (rest.isEmpty)
            drawer.cards.count != drawn.cards.count
          else
            true,
          "when rest not exist, drawer's rest cards and drawn's must not be same"
        )

        drawer.drawFrom(drawn) match {
          case (Left(finishByDraw), Left(finishByDrawn)) =>
            val nextFinished = finished :+ finishByDrawn :+ finishByDraw

            rest match {
              case last :: Nil => nextFinished :+ last.giveUp
              case nextDrawer :: nextDrawn :: rest => process(
                nextDrawer.skip,
                nextDrawn,
                rest,
                nextFinished,
              )
            }

          case (Left(finishByDraw), Right(nextDrawer)) =>
            val nextFinished = finished :+ finishByDraw

            rest match {
              case Nil => nextFinished :+ nextDrawer.giveUp
              case nextDrawn :: nextRest => process(nextDrawer, nextDrawn, nextRest, nextFinished)
            }

          case (Right(notFinishByDraw), Left(finishByDrawn)) =>
            val nextFinished = finished :+ finishByDrawn

            rest match {
              case Nil => nextFinished :+ notFinishByDraw.giveUp
              case _ =>
                val nextDrawer :: nextDrawn :: nextRest = rest :+ notFinishByDraw

                process(nextDrawer.skip, nextDrawn, nextRest, nextFinished)
            }

          case (Right(notFinishByDraw), Right(nextDrawer)) =>
            val nextDrawn :: nextRest = rest :+ notFinishByDraw

            process(nextDrawer, nextDrawn, nextRest, finished)
        }
      }
    }
  }
}

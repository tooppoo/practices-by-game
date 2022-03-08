package philomagi.practices_by_game.old_maid
package core.game

import core.card.Card
import core.event.Event
import core.player.Phase.{Finish, GetReady, Preparing}

object Events {
  case class SetupBegin(members: Seq[Preparing]) extends Event
  case class Deal(aCard: Card, to: Preparing) extends Event
  case class SetupFinish(members: Seq[GetReady], alreadyFinished: Seq[Finish]) extends Event

  case class GameOver(members: Seq[Finish]) extends Event
}

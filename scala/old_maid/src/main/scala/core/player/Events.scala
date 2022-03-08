package philomagi.practices_by_game.old_maid
package core.player

import core.card.Card
import core.event.Event
import core.player.Phase.{Drawer, Drawn}

object Events {
  case class MoveCard(from: Drawn, to: Drawer, aCard: Card) extends Event

  case class DumpCard(who: Player, aCard: Card) extends Event
}

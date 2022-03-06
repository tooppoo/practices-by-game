package philomagi.practices_by_game.old_maid
package core.util

import scala.util.Random

object Shuffle {
  type Shuffle[T] = Seq[T] => Seq[T]

  object Preset {
    def randomShuffle[T](implicit randomizer: Random): Shuffle[T] = randomizer.shuffle(_)
  }
}

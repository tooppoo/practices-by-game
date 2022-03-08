package philomagi.practices_by_game.old_maid
package core.shuffle

import scala.util.Random

trait Shuffle {
  def shuffle[T](xs: Seq[T]): Seq[T]
}
object Shuffle {
  object Preset {
    case class RandomShuffle(random: Random) extends Shuffle {
      override def shuffle[T](xs: Seq[T]): Seq[T] = random.shuffle(xs)
    }
  }
}

package philomagi.practices_by_game.old_maid
package core.player

import scala.util.Random

trait Strategy {
  def selectCandidateFrom[C <: Player#CardsInHand#Candidate](candidates: Seq[C]): C
}
object Strategy {
  case class SelectRandom(random: Random = new Random()) extends Strategy {
    def selectCandidateFrom[C <: Player#CardsInHand#Candidate](candidates: Seq[C]): C = {
      require(candidates.nonEmpty)

      val index = random.between(0, candidates.length)

      candidates(index)
    }
  }
}

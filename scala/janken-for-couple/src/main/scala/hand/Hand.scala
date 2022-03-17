package philomagi.practices_by_game.janken_for_couple
package hand

sealed trait Hand {
  def battle(other: Hand): Hand.Battle.Result
}
object Hand {
  case object Stone extends Hand {
    override def battle(other: Hand): Battle.Result = other match {
      case Stone => Battle.Draw
      case Scissors => Battle.Win
      case Paper => Battle.Lose
    }

    override def toString: String = "グー"
  }
  case object Scissors extends Hand {
    override def battle(other: Hand): Battle.Result = other match {
      case Stone => Battle.Win
      case Scissors => Battle.Draw
      case Paper => Battle.Lose
    }

    override def toString: String = "チョキ"
  }
  case object Paper extends Hand {
    override def battle(other: Hand): Battle.Result = other match {
      case Stone => Battle.Win
      case Scissors => Battle.Lose
      case Paper => Battle.Draw
    }

    override def toString: String = "パー"
  }

  object Battle {
    sealed trait Result

    case object Win extends Result
    case object Lose extends Result
    case object Draw extends Result
  }
}

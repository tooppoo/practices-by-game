package philomagi.practices_by_game.old_maid
package core.card

sealed trait Card
object Card {
  case class NumberCard(private val number: NumberCard.Number) extends Card {
    override def toString: String = s"$number"

    override def equals(obj: Any): Boolean = (this, obj) match {
      case (Card.NumberCard(thisNumber), Card.NumberCard(otherNumber)) => thisNumber == otherNumber
      case _ => false
    }
  }
  object NumberCard {
    case class Number(value: Int) {
      require( 1 <= value && value <= 13)

      override def toString: String = s"$value"
    }
  }

  case object Joker extends Card {
    override def toString: String = "Joker"
  }
}

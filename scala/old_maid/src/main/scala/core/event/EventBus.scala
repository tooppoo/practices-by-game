package philomagi.practices_by_game.old_maid
package core.event

trait EventBus
object EventBus extends EventBus {
  private var handlers: Seq[EventBus.EventHandler] = Seq.empty

  def on(f: EventBus.EventHandler): EventBus = {
    this.handlers = this.handlers :+ f

    this
  }

  def emit(e: Event): EventBus = {
    for { f <- handlers } f(e)

    this
  }

  type EventHandler = Event => Unit
}

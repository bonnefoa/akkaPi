package akkapi.supervisor

import se.scalablesolutions.akka.actor.{Actor}
import se.scalablesolutions.akka.config.OneForOneStrategy

/**
 * Created by IntelliJ IDEA.
 *
 * @author Anthonin Bonnefoy
 */

case class DoSupervise(actor: Actor)

class RandomSupervisor extends Actor {
  trapExit = List(classOf[Exception])
  faultHandler = Some(OneForOneStrategy(3, 100))

  def receive: PartialFunction[Any, Unit] = {
    case DoSupervise(actor: Actor) =>
      log.info("Supervising worker: " + actor)
      startLink(actor)

    case unknown =>
      log.error("Unknown event: %s", unknown)
  }

  override def stop = synchronized {
    super[Actor].stop
    getLinkedActors.toArray.toList.asInstanceOf[List[Actor]].foreach {
      actor =>
        actor.stop
        log.info("Shutting actor down: %s", actor)
    }
    log.info("Stopping supervisor: %s", this)
  }

}

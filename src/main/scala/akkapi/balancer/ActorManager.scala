package akkapi.balancer

import se.scalablesolutions.akka.actor.Actor
import akkapi.random.RandomSupplier
import akkapi.pi.PiActor
import akkapi.recorder.ResultRecorder

/**
 * Created by IntelliJ IDEA.
 * @author Anthonin Bonnefoy
 */
class ActorManager extends Actor {
  def receive: PartialFunction[Any, Unit] = {
    case actorManagerCreatePiActor: ActorManagerCreatePiActor =>
      linkAndLog(new PiActor)
    case actorManagerCreateRandomSupplier: ActorManagerCreateRandomSupplier =>
      linkAndLog(new RandomSupplier)
    case actorManagerrecordResult: ActorManagerCreateRecordResul =>
      linkAndLog(new ResultRecorder)
  }

  def linkAndLog(actor: Actor) {
    link(Actor)
    if (!actor.isRunning)
      log.error("Actor couldn't be started")
    else
      log.debug("actor started " + actor)
  }

}

case class ActorManagerActorNotStarted
case class ActorManagerCreateRandomSupplier()
case class ActorManagerCreatePiActor()
case class ActorManagerCreateRecordResul()
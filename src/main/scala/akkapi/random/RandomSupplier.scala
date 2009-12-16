package akkapi.random

import se.scalablesolutions.akka.config.ScalaConfig._
import se.scalablesolutions.akka.actor.Actor

/**
 * Created by IntelliJ IDEA.
 *
 * @author Anthonin Bonnefoy
 */

case class AskRandom()

class RandomSupplier() extends Actor {
  lifeCycle = Some(LifeCycle(Permanent))

  def receive: PartialFunction[Any, Unit] = {
    case AskRandom() =>
      val res = Math.random
      log.debug("Replying " + res)
      reply(res)

    case other =>
      log.error("Unknown event: %s", other)
  }

  override def preRestart(reason: AnyRef) {
    log.debug("pre-restarting " + this)
  }

  override def postRestart(reason: AnyRef) {
    log.debug("post-restarting " + this)
  }

  override def toString = "[RandomSupplier]"
}
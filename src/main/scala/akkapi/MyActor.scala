package akkapi

/**
 * Created by IntelliJ IDEA.
 * User: sora
 * Date: Dec 15, 2009
 * Time: 9:54:53 PM
 * To change this template use File | Settings | File Templates.
 */

import se.scalablesolutions.akka.config.ScalaConfig._
import se.scalablesolutions.akka.actor.{OneForOneStrategy, Actor}

sealed trait Message

case class Supervise(worker: Worker) extends Message

case class DoWork(work: Work) extends Message

case object Die extends Message

class Worker(workerName: String) extends Actor {
  lifeCycleConfig = Some(LifeCycle(Permanent, 100))

  def receive: PartialFunction[Any, Unit] = {

    case DoWork(work: String) =>
      log.debug("start working... at: " + work)

    case Reset =>
      log.info("%s has been reset", toString)

    case Die =>
      log.debug("Dying...")
      throw new RuntimeException("I'm dead: " + this.toString)

    case other =>
      log.error("Unknown event: %s", other)
  }

  override def preRestart(reason: AnyRef, config: Option[AnyRef]) {
    log.debug("pre-restarting " + this)
  }

  override def postRestart(reason: AnyRef, config: Option[AnyRef]) {
    log.debug("post-restarting " + this)
  }

  override def toString = "[" + workerName + "]"

}

class MyActor


package akkapi.random

import se.scalablesolutions.akka.config.ScalaConfig._
import se.scalablesolutions.akka.actor.Actor
import org.uncommons.maths.random.MersenneTwisterRNG

/**
 * Created by IntelliJ IDEA.
 *
 * @author Anthonin Bonnefoy
 */

sealed trait RandomMessage
case class AskRandom() extends RandomMessage
case class AskRandomBetween(min: Double, max: Double) extends RandomMessage
case class AskRandomListBetween(size: Int, min: Double, max: Double) extends RandomMessage
case class AskRandomList(size: Int) extends RandomMessage


class RandomSupplier(workerName: String) extends Actor {
  lifeCycle = Some(LifeCycle(Permanent))

  def receive: PartialFunction[Any, Unit] = {
    case AskRandom() =>
      replyAndLog(RandomGenerator.nextDouble)
    case AskRandomBetween(min, max) =>
      replyAndLog(RandomGenerator.nextDouble(min, max))
    case AskRandomList(size) =>
      replyAndLog(RandomGenerator.listDouble(size))
    case AskRandomListBetween(size, min, max) =>
      replyAndLog(RandomGenerator.listDouble(size))
    case other =>
      log.error("Unknown event: %s", other)
  }

  private def replyAndLog(result: Any) = {
    log.debug("Replying " + result)
    reply(result)
  }

  override def preRestart(reason: AnyRef) {
    log.debug("pre-restarting " + this)
  }

  override def postRestart(reason: AnyRef) {
    log.debug("post-restarting " + this)
  }

  override def toString = "[" + workerName + "]"
}


object RandomGenerator {
  val mersenne = new MersenneTwisterRNG

  def nextDouble(): Double = {
    mersenne.nextDouble
  }

  /**
   * Generate a random double
   */
  def nextDouble(min: Double, max: Double): Double = {
    mersenne.nextDouble * (max - min) + min
  }

  /**
   * Generate a list of random double
   */
  def listDouble(size: Int): List[Double] = {
    (0 until size).flatMap(
      i => nextDouble :: Nil
      ).toList
  }

  /**
   * Generate a list of random double
   */
  def listDouble(size: Int, min: Int, max: Int): List[Double] = {
    (0 until size).flatMap(
      i => nextDouble(min, max) :: Nil
      ).toList
  }

}
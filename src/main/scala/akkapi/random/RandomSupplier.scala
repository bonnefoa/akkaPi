package akkapi.random

import se.scalablesolutions.akka.config.ScalaConfig._
import se.scalablesolutions.akka.actor.Actor
import org.uncommons.maths.random.MersenneTwisterRNG

/**
 * Created by IntelliJ IDEA.
 *
 * @author Anthonin Bonnefoy
 */

case class AskRandom()
case class AskRandomBetween(min: Double, max: Double)

class RandomSupplier() extends Actor {
  lifeCycle = Some(LifeCycle(Permanent))

  def receive: PartialFunction[Any, Unit] = {
    case AskRandom() =>
      val res = RandomGenerator.nextDouble
      log.debug("Replying " + res)
      reply(res)
    case AskRandomBetween(min, max) =>
      val res = RandomGenerator.nextDouble(min, max)
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
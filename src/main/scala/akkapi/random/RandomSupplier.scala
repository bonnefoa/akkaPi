package akkapi.random

import se.scalablesolutions.akka.config.ScalaConfig._
import org.uncommons.maths.random.MersenneTwisterRNG
import se.scalablesolutions.akka.actor.Actor

/**
 * Created by IntelliJ IDEA.
 *
 * @author Anthonin Bonnefoy
 */

sealed trait RandomMessage
case class AskRandom() extends RandomMessage
case class AskRandomAsync() extends RandomMessage
case class AskRandomBetween(min: Double, max: Double) extends RandomMessage
case class AskRandomBetweenAsync(min: Double, max: Double) extends RandomMessage
case class AskRandomListBetween(size: Int, min: Double, max: Double) extends RandomMessage
case class AskRandomListBetweenAsync(size: Int, min: Double, max: Double) extends RandomMessage
case class AskRandomList(size: Int) extends RandomMessage
case class AskRandomListAsync(size: Int) extends RandomMessage


class RandomSupplier(actorName: String) extends Actor {
  lifeCycle = Some(LifeCycle(Permanent))
  timeout = 100
  id = actorName

  def receive: PartialFunction[Any, Unit] = {
    case AskRandom() =>
      replyAndLog(RandomGenerator.nextDouble)
    case AskRandomBetween(min, max) =>
      replyAndLog(RandomGenerator.nextDouble(min, max))
    case AskRandomList(size) =>
      replyAndLog(RandomGenerator.listDouble(size))
    case AskRandomListBetween(size, min, max) =>
      replyAndLog(RandomGenerator.listDouble(size, min, max))
    case AskRandomAsync() =>
      replyToSenderAndLog(RandomGenerator.nextDouble)
    case AskRandomBetweenAsync(min, max) =>
      replyToSenderAndLog(RandomGenerator.nextDouble(min, max))

    case AskRandomListAsync(size) =>
      replyToSenderAndLog(RandomGenerator.listDouble(size))

    case AskRandomListBetweenAsync(size, min, max) =>
      replyToSenderAndLog(RandomGenerator.listDouble(size, min, max))
    case other =>
      log.error("Unknown event: %s", other)
  }

  private def replyToSenderAndLog(result: Any) = {
    //    log.debug("Replying to Sender " + sender + " result " + result)
    if (sender.isDefined)
      sender.get ! Some(result)
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

  override def toString = "[" + actorName + "]"
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
  def listDouble(size: Int, min: Double, max: Double): List[Double] = {
    (0 until size).flatMap(
      i => nextDouble(min, max) :: Nil
      ).toList
  }

}
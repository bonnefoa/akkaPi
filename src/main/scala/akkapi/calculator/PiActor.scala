package akkapi.calculator

import se.scalablesolutions.akka.config.ScalaConfig._
import se.scalablesolutions.akka.config.ScalaConfig.LifeCycle
import se.scalablesolutions.akka.actor.{ActorRegistry, Actor}
import akkapi.random.RandomSupplier

/**
 * Calculate Pi
 *
 * @author Anthonin Bonnefoy
 */

sealed trait PiMessage
case class AskPi() extends PiMessage

class PiActor(id: String) extends Actor {
  lifeCycle = Some(LifeCycle(Permanent))

  def getRandomSupplier: RandomSupplier = {
    val listRandomSupplier = ActorRegistry.actorsFor(classOf[RandomSupplier])
    listRandomSupplier.head.asInstanceOf[RandomSupplier]
  }

  def receive: PartialFunction[Any, Unit] = {
    case AskPi() =>
      log.debug("Received askPi ")
    case other =>
      log.error("Unknown event: %s", other)
  }

}

class PiCalculator {
  def estimatePi(randomList: List[(Double, Double)]): Double = {
    getNumberPointsInCircle(randomList) / randomList.size.asInstanceOf[Double] * 4
  }

  def getNumberPointsInCircle(randomList: List[(Double, Double)]): Int = {
    (0 /: randomList)((seed: Int, tuple) => {
      val x = tuple._1
      val y = tuple._2
      if (Math.sqrt(x * x + y * y) < 1) seed + 1
      else seed
    })
  }
}
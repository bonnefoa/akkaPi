package akkapi.calculator

import se.scalablesolutions.akka.config.ScalaConfig._
import se.scalablesolutions.akka.config.ScalaConfig.LifeCycle
import se.scalablesolutions.akka.actor.{ActorRegistry, Actor}
import akkapi.random.{AskRandom, RandomSupplier}

/**
 * Calculate Pi
 *
 * @author Anthonin Bonnefoy
 */

sealed trait PiMessage
case class AskPi() extends PiMessage
case class AskPiWithNumberPoints(numberOfPoints: Int) extends PiMessage

class PiActor(id: String) extends Actor {
  lifeCycle = Some(LifeCycle(Permanent))

  def getRandomSupplier: RandomSupplier = {
    val listRandomSupplier = ActorRegistry.actorsFor(classOf[RandomSupplier])
    listRandomSupplier.head.asInstanceOf[RandomSupplier]
  }

  def receive: PartialFunction[Any, Unit] = {
    case AskPi() =>
      log.debug("Received askPi ")
      val estimatedPi = estimatePi(1000)
      log.debug("Estimated pi :" + estimatedPi)
      reply(estimatedPi)
    case AskPiWithNumberPoints(numberOfPoints) =>
      log.debug("Received askPi with numberOfPoints :" + numberOfPoints)
      val estimatedPi = estimatePi(numberOfPoints)
      log.debug("Estimated pi :" + estimatedPi)
      reply(estimatedPi)
    case other =>
      log.error("Unknown event: %s", other)
  }

  def estimatePi(numberOfPoints: Int): Double = {
    val piCalculatorStateful = new PiCalculatorStateful
    val randomSupplier = ActorRegistry.actorsFor(classOf[RandomSupplier]).head
    (1 to numberOfPoints).foreach(i => {
      val x = randomSupplier !! new AskRandom
      val y = randomSupplier !! new AskRandom
      piCalculatorStateful.addPoint(x.get, y.get)
    })
    piCalculatorStateful.processPi
  }
}

class PiCalculatorStateful {
  var insideCircle = 0
  var numberOfPoints = 0

  def addPoint(x: Double, y: Double) {
    numberOfPoints += 1
    if (Math.sqrt(x * x + y * y) < 1) insideCircle += 1
  }

  def processPi: Double = {
    insideCircle / numberOfPoints.asInstanceOf[Double] * 4
  }
}

class PiCalculator {
  def estimatePi(randomList: List[(Double, Double)]): Double = {
    getNumberPointsInCircle(randomList) / randomList.size.asInstanceOf[Double] * 4
  }

  def getNumberPointsInCircle(randomList: List[(Double, Double)]): Int = {
    (0 /: randomList)((seed: Int, tuple) => {
      val (x, y) = tuple
      if (Math.sqrt(x * x + y * y) < 1) seed + 1
      else seed
    })
  }
}
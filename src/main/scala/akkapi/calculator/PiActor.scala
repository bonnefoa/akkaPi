package akkapi.calculator

import se.scalablesolutions.akka.config.ScalaConfig._
import se.scalablesolutions.akka.config.ScalaConfig.LifeCycle
import se.scalablesolutions.akka.actor.{ActorRegistry, Actor}
import akkapi.random._

/**
 * Calculate Pi
 *
 * @author Anthonin Bonnefoy
 */

sealed trait PiMessage
case class EstimatePiWithNumberOfPoints(numberOfPoints: Int) extends PiMessage
case class EstimatePiWithNumberOfPointsAndBatchSize(numberOfPoints: Int, batchSize: Int) extends PiMessage

class PiActor(id: String) extends Actor {
  lifeCycle = Some(LifeCycle(Permanent))
  timeout = 10000
  var piCalculatorStateful = new PiCalculatorStateful

  def getRandomSupplier: RandomSupplier = {
    val listRandomSupplier = ActorRegistry.actorsFor(classOf[RandomSupplier])
    log.debug("Got randomSupplierList " + listRandomSupplier)
    listRandomSupplier.head.asInstanceOf[RandomSupplier]
  }

  def receive: PartialFunction[Any, Unit] = {
    case Some(point:Double) =>
      log.debug("Received a point :" + point)
      piCalculatorStateful.addPoint(point)

    case Some(listPoints:List[Double]) =>
      log.debug("Received a list :" + listPoints)
      piCalculatorStateful.addPoints(listPoints)

    case EstimatePiWithNumberOfPoints(numberOfPoints) =>
      log.debug("Received askPi with numberOfPoints :" + numberOfPoints)
      val estimatedPi = estimatePi {
        randomSupplier: Actor =>
          (1 to numberOfPoints).foreach(i => {
            randomSupplier ! new AskRandomAsync()
            randomSupplier ! new AskRandomAsync()
          })
      }
      log.debug("Estimated pi :" + estimatedPi)
      if (sender.isDefined)
        sender.get ! Some(estimatedPi)

    case EstimatePiWithNumberOfPointsAndBatchSize(numberOfPoints, batchSize) =>
      log.debug("Received askPi with numberOfPoints :" + numberOfPoints + " and batchSize " + batchSize)
      val estimatedPi = estimatePi {
        randomSupplier: Actor =>
          (1 to numberOfPoints by batchSize).foreach(i => {
            randomSupplier ! new AskRandomListAsync(batchSize)
            randomSupplier ! new AskRandomListAsync(batchSize)
          })
      }
      log.debug("Estimated pi :" + estimatedPi)
      if (sender.isDefined)
        sender.get ! Some(estimatedPi)
    case other =>
      log.error("Unknown event: %s", other)
  }

  def estimatePi(askRandom: Actor => Unit): Double = {
    piCalculatorStateful = new PiCalculatorStateful
    val randomSupplier = getRandomSupplier
    askRandom(randomSupplier)
    piCalculatorStateful.processPi
  }

}

class PiCalculatorStateful {
  var insideCircle = 0
  var numberOfPoints = 0

  var listBuffer: Option[List[Double]] = None
  var pointBuffer: Option[Double] = None

  def addPoints(list: List[Double]) {
    if (listBuffer.isDefined) {
      addPoints(listBuffer.get, list)
      listBuffer = None
    } else listBuffer = Some(list)
  }

  def addPoint(double: Double) {
    if (pointBuffer.isDefined) {
      addPoint(pointBuffer.get, double)
      pointBuffer = None
    } else pointBuffer = Some(double)
  }

  def addPoints(listX: List[Double], listY: List[Double]) {
    for{x <- listX
        y <- listY
    } yield addPoint(x, y)
  }

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
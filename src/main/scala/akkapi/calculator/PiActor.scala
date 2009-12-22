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

sealed trait PiActorMessage
case class EstimatePiWithNumberOfPoints(numberOfPoints: Int) extends PiActorMessage
case class EstimatePiWithNumberOfPointsAndBatchSize(numberOfPoints: Int, batchSize: Int) extends PiActorMessage

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
    case Some(point: Double) =>
      log.debug("Received a point :" + point)
      piCalculatorStateful.addPoint(point)
    case Some(listPoints: List[Double]) =>
      log.debug("Received a list :" + listPoints)
      piCalculatorStateful.addPoints(listPoints)
    case EstimatePiWithNumberOfPoints(numberOfPoints) =>
      log.debug("Received askPi with numberOfPoints :" + numberOfPoints)
      piCalculatorStateful = new PiCalculatorStateful(numberOfPoints)
      val randomSupplier = getRandomSupplier
      (1 to numberOfPoints).foreach(i => {
        randomSupplier ! new AskRandomAsync()
        randomSupplier ! new AskRandomAsync()
      })
    
    case EstimatePiWithNumberOfPointsAndBatchSize(numberOfPoints, batchSize) =>
      log.debug("Received askPi with numberOfPoints :" + numberOfPoints + " and batchSize " + batchSize)
      piCalculatorStateful = new PiCalculatorStateful(numberOfPoints)
      val randomSupplier = getRandomSupplier
      (1 to numberOfPoints by batchSize).foreach(i => {
        randomSupplier ! new AskRandomListAsync(batchSize)
        randomSupplier ! new AskRandomListAsync(batchSize)
      })
    case other =>
      log.error("Unknown event: %s", other)
  }

  def waitResultAndSendResponse(sender: Actor) {
    while (!piCalculatorStateful.isComplete){
//      this.
    }

  }
}

class PiCalculatorStateful(val expectedNumberOfPoints: Int) {
  var insideCircle = 0
  var currentNumberOfPoints = 0

  var listBuffer: Option[List[Double]] = None
  var pointBuffer: Option[Double] = None

  def isComplete = {
    currentNumberOfPoints > expectedNumberOfPoints
  }

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
    expectedNumberOfPoints += 1
    if (Math.sqrt(x * x + y * y) < 1) insideCircle += 1
  }

  def processPi: Double = {
    insideCircle / expectedNumberOfPoints.asInstanceOf[Double] * 4
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
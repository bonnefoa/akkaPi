package akkapi.pi

import se.scalablesolutions.akka.actor.{ActorRegistry, Actor}
import akkapi.random._
import se.scalablesolutions.akka.util.Logging

/**
 * PiActor will supply pi estimation through differents message.<br />
 * When it received a request, it will ask for all necessary random numbers to the Random Supplier.
 * Once all random number are regrouped, it send asynchronously the pi estimation to the request sender.
 *
 * @author Anthonin Bonnefoy
 */
sealed trait PiActorMessage
case class BusyPiActor()
case class EstimatePiWithNumberOfPoints(numberOfPoints: Int) extends PiActorMessage
case class EstimatePiWithNumberOfPointsAndBatchSize(numberOfPoints: Int, batchSize: Int) extends PiActorMessage
case class PiResponse(result: Double, numberOfPoints: Int) extends PiActorMessage

class PiActor(id: String) extends Actor {
  timeout = 10000

  var busy = false

  var piCalculatorStateful: PiCalculatorStateful = null

  def getRandomSupplier: RandomSupplier = {
    val listRandomSupplier = ActorRegistry.actorsFor(classOf[RandomSupplier])
    log.debug("Got randomSupplierList " + listRandomSupplier)
    listRandomSupplier.head.asInstanceOf[RandomSupplier]
  }

  def receive: PartialFunction[Any, Unit] = {
    case Some(point: Double) =>
      //      log.debug("Received a point :" + point)
      piCalculatorStateful.addPoint(point)
      sendIfComplete
    case Some(listPoints: List[Double]) =>
      //      log.debug("Received a list :" + listPoints)
      piCalculatorStateful.addPoints(listPoints)
      sendIfComplete
    case EstimatePiWithNumberOfPoints(numberOfPoints) =>
      log.debug("Received askPi with numberOfPoints :" + numberOfPoints)
      sendRandomRequest(numberOfPoints) {
        randomSupplier =>
          (1 to numberOfPoints).foreach(i => {
            randomSupplier ! new AskRandomAsync()
            randomSupplier ! new AskRandomAsync()
          })
      }
    case EstimatePiWithNumberOfPointsAndBatchSize(numberOfPoints, batchSize) =>
      sendRandomRequest(numberOfPoints) {
        randomSupplier =>
          (0 until numberOfPoints by batchSize).foreach(i => {
            randomSupplier ! new AskRandomListAsync(batchSize)
            randomSupplier ! new AskRandomListAsync(batchSize)
          })
      }
    case other =>
      log.error("Unknown event: %s", other)
  }

  def sendIfComplete {
    if (piCalculatorStateful.isComplete) {
      piCalculatorStateful.sender ! PiResponse(piCalculatorStateful.processPi, piCalculatorStateful.currentNumberOfPoints)
      busy = false
      piCalculatorStateful = null
    }
  }

  def sendRandomRequest(numberOfPoints: Int)(block: Actor => unit) {
    if (busy) {
      log.error("PiActor already busy")
      sender.get ! new BusyPiActor
    } else {
      busy = true
      piCalculatorStateful = new PiCalculatorStateful(numberOfPoints)(sender.get)
      val randomSupplier = getRandomSupplier
      block(randomSupplier)
    }
  }
}

class PiCalculatorStateful(val expectedNumberOfPoints: Int)(val sender: Actor) extends Logging {
  var insideCircle = 0
  var currentNumberOfPoints = 0

  var listBuffer: Option[List[Double]] = None
  var pointBuffer: Option[Double] = None

  def isComplete = {
    currentNumberOfPoints >= expectedNumberOfPoints
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
    (listX zip listY).foreach(tuple => addPoint(tuple._1, tuple._2))
  }

  def addPoint(x: Double, y: Double) {
    currentNumberOfPoints += 1
    if (Math.sqrt(x * x + y * y) < 1) insideCircle += 1
  }

  def processPi: Double = {
    log.debug("Processing pi with " + currentNumberOfPoints + " points")
    insideCircle / currentNumberOfPoints.asInstanceOf[Double] * 4
  }
}

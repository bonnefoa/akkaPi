package akkapi.recorder

import se.scalablesolutions.akka.actor.{ActorRegistry, Actor}
import akkapi.pi.{EstimatePiWithNumberOfPointsAndBatchSize, PiActor}
import se.scalablesolutions.akka.util.Logging

/**
 * Configure actor manager
 * @author Anthonin Bonnefoy
 */
object ResultRecorder {
  val defaultBatchSize: Int = 1000
}

import ResultRecorder._

/**
 * Ask for pi estimation and record results in a persistent storage.
 *
 * @author Anthonin Bonnefoy
 */
class ResultRecorder extends Actor with Logging {
  /**
   * Should use persistence on this
   */
  var piStatistique = new PiStatistique(Nil)

  def getPiActor: Actor = {
    val listRandomSupplier = ActorRegistry.actorsFor(classOf[PiActor])
    log.debug("Got piActorList " + listRandomSupplier)
    listRandomSupplier.head.asInstanceOf[PiActor]
  }

  def receive: PartialFunction[Any, Unit] = {
    case PiResponse(piEstimate, numberOfPoints) =>
      log.debug("Received " + piEstimate)
      piStatistique = piStatistique.addResult(PiResult(piEstimate, numberOfPoints))
    case other =>
      log.error("Unknown response : " + other)
  }

  def sendRequestForPi(numberOfPoints: Int) = {
    getPiActor ! (EstimatePiWithNumberOfPointsAndBatchSize(numberOfPoints, defaultBatchSize))
  }

}

case class PiStatistique(listResult: List[PiResult]) {
  def addResult(piResult: PiResult): PiStatistique = {
    new PiStatistique(piResult :: listResult)
  }

  lazy val totalweight = {
    (0D /: listResult)((seed, res) => seed + res.weight)
  }
  lazy val mean = {
    (0D /: listResult)((seed, res) => seed + res.value * res.weight) / totalweight
  }
}

/**
 * Value : Pi estimation found.<br />
 * Weight : Number of random points used to find this estimation.
 */
case class PiResult(value: Double, weight: Double)
case class ResultRecorderMessage(piStatistique: PiStatistique)
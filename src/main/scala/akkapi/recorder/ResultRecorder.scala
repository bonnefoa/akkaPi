package akkapi.recorder

import se.scalablesolutions.akka.actor.{ActorRegistry, Actor}
import se.scalablesolutions.akka.util.Logging
import akkapi.pi.{PiResponse, EstimatePiWithNumberOfPointsAndBatchSize, PiActor}
import akkapi.balancer.Balancer

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


  def receive: PartialFunction[Any, Unit] = {
    case RequestRecorderPiEstimateRequests(numberOfPoints) =>
      log.debug("Request pi")
      sendRequestForPi(numberOfPoints)
    case askPiStatistique: RequestRecorderAskPiStatistique =>
      log.debug("Sending piStatistique " + piStatistique + " to " + sender)
      if (sender.isDefined)
        sender.get ! new RequestRecorderPiStatistiqueResponse(piStatistique)
    case PiResponse(piEstimate, numberOfPoints) =>
      log.debug("Received " + piEstimate)
      piStatistique = piStatistique.addResult(RequestRecorderPiResult(piEstimate, numberOfPoints))
    case other =>
      log.error("Unknown response : " + other)
  }

  def sendRequestForPi(numberOfPoints: Int) {
    val freeActors = Balancer.getFreePiActor
    if (freeActors.isDefined)
      freeActors.get ! (EstimatePiWithNumberOfPointsAndBatchSize(numberOfPoints, defaultBatchSize))
    else {
      log.debug("No free actors for the moment, retry later")
    }
  }
}

case class PiStatistique(listResult: List[RequestRecorderPiResult]) {
  def addResult(piResult: RequestRecorderPiResult): PiStatistique = {
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
case class RequestRecorderPiResult(value: Double, weight: Double)

case class RequestRecorderPiEstimateRequests(numberOfPoints: Int)
case class RequestRecorderAskPiStatistique()
case class RequestRecorderPiStatistiqueResponse(piStatistique: PiStatistique)
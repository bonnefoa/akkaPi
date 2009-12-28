package akkapi.recorder

import se.scalablesolutions.akka.actor.{ActorRegistry, Actor}
import se.scalablesolutions.akka.util.Logging
import akkapi.pi.{EstimatePiWithNumberOfPointsAndBatchSize, PiActor}

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

  def getFreePiActor: Option[Actor] = {
    val listPiActor = ActorRegistry.actorsFor(classOf[PiActor]).asInstanceOf[List[PiActor]]
    val filteredList = listPiActor
            .filter((actor: PiActor) => !actor.busy)
    if (filteredList == Nil) None
    else Some(filteredList.head.asInstanceOf[Actor])
  }


  def receive: PartialFunction[Any, Unit] = {
    case askPiStatistique: AskPiStatistique =>
      log.debug("Sending piStatistique " + piStatistique + " to " + sender)
      if (sender.isDefined)
        sender.get ! new PiStatistiqueResponse(piStatistique)
    case PiResponse(piEstimate, numberOfPoints) =>
      log.debug("Received " + piEstimate)
      piStatistique = piStatistique.addResult(PiResult(piEstimate, numberOfPoints))
    case other =>
      log.error("Unknown response : " + other)
  }

  def sendRequestForPi(numberOfPoints: Int) {
    val freeActors = getFreePiActor
    if (freeActors.isDefined)
      freeActors.get ! (EstimatePiWithNumberOfPointsAndBatchSize(numberOfPoints, defaultBatchSize))
    else {
      log.debug("No free actors for the moment, waiting")
      Thread.sleep(100)
      sendRequestForPi(numberOfPoints)
    }
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

case class AskPiStatistique()
case class PiStatistiqueResponse(piStatistique: PiStatistique)